# Season stats from the MLB Stats API and Baseball Savant.
#
# Sourced by refresh_data.R and by the R tests; defining things here has no side effects, nothing runs on source.
# Callers load the packages in the first block. Run everything from the repository root.
#
# One code path builds every season: build_season_stats(year) returns the hitter and pitcher tables for any
# year, current or finished. A finished season written to disk (build_baseline_season) is the same table, so the
# formulas for current stats and baselines cannot drift apart. Tables are keyed by mlbamid (the MLB player id).
#
# Where each field comes from (checked against FanGraphs for 2023-2025, see docs/fangraphs-spike-results.md):
#   hitters   counts and Age        MLB stats=season (Age is the age on June 30 of the season)
#             wOBA                  MLB stats=sabermetrics
#             xwOBA                 Savant expected_statistics (est_woba)
#             Barrels               Savant statcast (barrels, a count)
#   pitchers  counts and Age        MLB stats=season
#             FIP, BABIP, LOB%      computed from the counts (LOB% is clamped to 0-100)
#             xERA                  Savant expected_statistics (xera)
#             barrel%, O-Swing%,    Savant custom leaderboard
#             whiff%
# Hitters and pitchers are separate lists: a pitcher who batted is not a hitter, and a position player who pitched is
# not a pitcher (Savant's pitcher leaderboards have no rows for them anyway). Ohtani, whose primary position is TWP,
# is in both.
# Units: the percent columns (barrel_percent, o_swing_percent, whiff_percent, lob_percent) are 0-100, babip is
# a fraction. FanGraphs' files held fractions for the first three, so do not mix the two.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(stringi)
  library(httr)
  library(jsonlite)
})

# =============================================================================
# GENERAL HELPERS
# =============================================================================

safe_divide <- function(numerator, denominator) {
  ifelse(is.na(denominator) | denominator == 0, NA_real_, numerator / denominator)
}

nan_to_na <- function(value) {
  ifelse(is.nan(value), NA_real_, value)
}

# Prefer the MLB value and fall back to the FanGraphs one when the MLB join found no match.
coalesce_joined <- function(primary, fallback) {
  ifelse(!is.na(primary), primary, fallback)
}

# Starters throw at least half their games as starts or average more than four innings an appearance.
classify_pitcher_role <- function(gs, g, ip) {
  if_else(gs / g >= .5 | ip / g > 4, "SP", "RP")
}

# The MLB and Savant endpoints fail transiently now and then; a few retries keep one blip from costing a day
# of data. Waits grow between attempts (10s, then 20s).
with_retry <- function(fn, label, attempts = 3, wait_seconds = 10) {
  for (attempt in seq_len(attempts)) {
    result <- tryCatch(fn(), error = function(e) e)
    if (!inherits(result, "error")) {
      return(result)
    }
    cat(label, "failed (attempt", attempt, "of", attempts, "):", conditionMessage(result), "\n")
    if (attempt < attempts) {
      Sys.sleep(wait_seconds * attempt)
    }
  }
  stop(label, " failed after ", attempts, " attempts: ", conditionMessage(result), call. = FALSE)
}

# Fail with the missing column names up front instead of a cryptic error (or silent NAs) further down when an
# upstream source renames or drops a field.
require_columns <- function(data, columns, label) {
  missing <- setdiff(columns, colnames(data))
  if (length(missing) > 0) {
    stop(label, " is missing expected columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(data)
}

parse_mlb_number <- function(value) {
  parsed <- suppressWarnings(as.numeric(value))
  parsed[is.nan(parsed)] <- NA_real_
  parsed
}

# MLB reports innings as "123.2" meaning 123 and two thirds.
parse_mlb_innings <- function(value) {
  parts <- str_split_fixed(as.character(value), "\\.", 2)
  whole <- suppressWarnings(as.numeric(parts[, 1]))
  remainder <- suppressWarnings(as.numeric(parts[, 2]))
  whole[is.na(whole)] <- 0
  remainder[is.na(remainder)] <- 0
  whole + remainder / 3
}

normalize_mlbam_id <- function(value) {
  value <- as.character(value)
  value[value == "" | value == "NA"] <- NA_character_
  value
}

ascii_name <- function(name) {
  stri_trans_general(name, id = "Latin-ASCII")
}

# =============================================================================
# HTTP
# =============================================================================

# Honest and generic: says what the client is and that the volume is low, without naming anyone.
DATA_SOURCE_USER_AGENT <- "baseball-data-refresh (hobby project; a handful of requests per run)"

default_http_get <- function(url) {
  response <- GET(url, user_agent(DATA_SOURCE_USER_AGENT), timeout(60))
  if (http_error(response)) {
    stop("status ", status_code(response))
  }
  content(response, as = "text", encoding = "UTF-8")
}

# `http_get` is a function(url) returning the response body as text; the tests pass a stub instead of the network.
fetch_text <- function(url, label, http_get = default_http_get, attempts = 3) {
  with_retry(function() http_get(url), label, attempts = attempts)
}

# =============================================================================
# MLB STATS API
# =============================================================================

mlb_stats_url <- function(stats, group, year, extra = "") {
  paste0(
    "https://statsapi.mlb.com/api/v1/stats?",
    "stats=", stats,
    "&group=", group,
    "&playerPool=ALL",
    "&season=", year,
    extra,
    "&limit=10000"
  )
}

# The splits of one stats response as a flat table. The feeds used here return one row per player (a traded
# player's teams are already combined); a duplicate id would be counted twice, so it stops the run.
parse_mlb_stats <- function(text, label, min_rows = 1, one_row_per_player = TRUE) {
  parsed <- fromJSON(text, flatten = TRUE)
  splits <- parsed$stats$splits[[1]]
  if (is.null(splits) || NROW(splits) == 0) {
    stop(label, " returned no rows", call. = FALSE)
  }
  splits <- tibble::as_tibble(splits)
  require_columns(splits, "player.id", label)
  # Some feeds mix in minor-league rows (sport.id other than 1); only the majors count.
  if ("sport.id" %in% colnames(splits)) {
    splits <- filter(splits, sport.id == 1)
  }
  if (one_row_per_player && anyDuplicated(splits$player.id) > 0) {
    stop(label, " has more than one row for a player; the one-row-per-player assumption no longer holds", call. = FALSE)
  }
  if (nrow(splits) < min_rows) {
    stop(label, " returned ", nrow(splits), " rows, fewer than the ", min_rows, " expected", call. = FALSE)
  }
  splits
}

fetch_mlb_stats <- function(year, group, http_get = default_http_get, stats = "season", min_rows = 1, one_row_per_player = TRUE) {
  label <- paste0("MLB Stats API ", stats, " ", group, " ", year)
  extra <- if (identical(stats, "season")) "&hydrate=person(currentTeam,primaryPosition)" else ""
  text <- fetch_text(mlb_stats_url(stats, group, year, extra), label, http_get)
  parse_mlb_stats(text, label, min_rows, one_row_per_player)
}

# Season hitting counts for every player with a plate appearance or not; build_hitter_season decides who counts.
fetch_mlb_hitting_season <- function(year, http_get = default_http_get, min_rows = 1) {
  label <- paste0("MLB Stats API season hitting ", year)
  fetch_mlb_stats(year, "hitting", http_get, min_rows = min_rows) |>
    require_columns(c("player.fullName", "player.primaryPosition.abbreviation", "stat.age", "stat.gamesPlayed", "stat.atBats", "stat.plateAppearances", "stat.hits", "stat.doubles", "stat.triples", "stat.homeRuns", "stat.baseOnBalls", "stat.hitByPitch", "stat.sacFlies", "stat.strikeOuts", "stat.totalBases"), label) |>
    transmute(
      mlbamid = normalize_mlbam_id(player.id),
      Name = ascii_name(player.fullName),
      Age = parse_mlb_number(stat.age),
      primary_position = player.primaryPosition.abbreviation,
      G = parse_mlb_number(stat.gamesPlayed),
      AB = parse_mlb_number(stat.atBats),
      PA = parse_mlb_number(stat.plateAppearances),
      H = parse_mlb_number(stat.hits),
      `2B` = parse_mlb_number(stat.doubles),
      `3B` = parse_mlb_number(stat.triples),
      HR = parse_mlb_number(stat.homeRuns),
      BB = parse_mlb_number(stat.baseOnBalls),
      HBP = parse_mlb_number(stat.hitByPitch),
      SF = parse_mlb_number(stat.sacFlies),
      SO = parse_mlb_number(stat.strikeOuts),
      TB = parse_mlb_number(stat.totalBases)
    )
}

fetch_mlb_pitching_season <- function(year, http_get = default_http_get, min_rows = 1) {
  label <- paste0("MLB Stats API season pitching ", year)
  fetch_mlb_stats(year, "pitching", http_get, min_rows = min_rows) |>
    require_columns(c("player.fullName", "player.primaryPosition.abbreviation", "stat.age", "stat.gamesPitched", "stat.gamesStarted", "stat.inningsPitched", "stat.battersFaced", "stat.hits", "stat.runs", "stat.earnedRuns", "stat.homeRuns", "stat.strikeOuts", "stat.baseOnBalls", "stat.intentionalWalks", "stat.hitByPitch", "stat.atBats", "stat.sacFlies"), label) |>
    transmute(
      mlbamid = normalize_mlbam_id(player.id),
      name = ascii_name(player.fullName),
      age = parse_mlb_number(stat.age),
      primary_position = player.primaryPosition.abbreviation,
      g = parse_mlb_number(stat.gamesPitched),
      gs = parse_mlb_number(stat.gamesStarted),
      ip = parse_mlb_innings(stat.inningsPitched),
      tbf = parse_mlb_number(stat.battersFaced),
      h = parse_mlb_number(stat.hits),
      r = parse_mlb_number(stat.runs),
      er = parse_mlb_number(stat.earnedRuns),
      hr = parse_mlb_number(stat.homeRuns),
      so = parse_mlb_number(stat.strikeOuts),
      bb = parse_mlb_number(stat.baseOnBalls),
      ibb = parse_mlb_number(stat.intentionalWalks),
      hbp = parse_mlb_number(stat.hitByPitch),
      ab = parse_mlb_number(stat.atBats),
      sf = parse_mlb_number(stat.sacFlies)
    )
}

# wOBA for every hitter the MLB sabermetrics feed knows (it matched FanGraphs' wOBA to within 0.00002).
fetch_mlb_hitter_woba <- function(year, http_get = default_http_get, min_rows = 1) {
  label <- paste0("MLB Stats API sabermetrics hitting ", year)
  fetch_mlb_stats(year, "hitting", http_get, stats = "sabermetrics", min_rows = min_rows) |>
    require_columns("stat.woba", label) |>
    transmute(mlbamid = normalize_mlbam_id(player.id), wOBA = parse_mlb_number(stat.woba))
}

# =============================================================================
# BASEBALL SAVANT
# =============================================================================
# Savant has no documented API; these are the CSV links behind the public leaderboards. The files start with a
# byte-order mark, quote some numbers, and name the first column "last_name, first_name". player_id is the MLB id.

savant_url <- function(leaderboard, year, type) {
  base <- "https://baseballsavant.mlb.com/leaderboard/"
  if (leaderboard == "custom") {
    return(paste0(
      base, "custom?year=", year, "&type=", type, "&filter=&min=1",
      "&selections=pa,babip,barrel_batted_rate,oz_swing_percent,whiff_percent",
      "&chart=false&x=pa&y=pa&r=no&chartType=beeswarm&csv=true"
    ))
  }
  paste0(base, leaderboard, "?type=", type, "&year=", year, "&position=&team=&min=1&csv=true")
}

parse_savant_csv <- function(text, required_columns, label, min_rows = 1) {
  if (is.na(text) || !nzchar(trimws(text))) {
    stop(label, " returned an empty response", call. = FALSE)
  }
  # A bot challenge or error page comes back as HTML with a 200 or 403; say so instead of failing on columns.
  if (grepl("^\\s*(<!doctype|<html|<)", text, ignore.case = TRUE)) {
    stop(label, " returned HTML instead of CSV (a bot challenge or an error page)", call. = FALSE)
  }
  data <- suppressWarnings(read_csv(I(text), col_types = cols(.default = col_guess(), player_id = col_character()), show_col_types = FALSE))
  require_columns(data, c("player_id", required_columns), label)
  data <- mutate(data, mlbamid = normalize_mlbam_id(player_id))
  if (anyNA(data$mlbamid) || anyDuplicated(data$mlbamid) > 0) {
    stop(label, " has a missing or repeated player_id", call. = FALSE)
  }
  if (nrow(data) < min_rows) {
    stop(label, " returned ", nrow(data), " rows, fewer than the ", min_rows, " expected", call. = FALSE)
  }
  data
}

fetch_savant_csv <- function(leaderboard, year, type, required_columns, http_get = default_http_get, min_rows = 1) {
  label <- paste0("Savant ", leaderboard, " ", type, " ", year)
  text <- fetch_text(savant_url(leaderboard, year, type), label, http_get)
  parse_savant_csv(text, required_columns, label, min_rows)
}

fetch_savant_hitter_xwoba <- function(year, http_get = default_http_get, min_rows = 1) {
  fetch_savant_csv("expected_statistics", year, "batter", "est_woba", http_get, min_rows) |>
    transmute(mlbamid, xwOBA = parse_mlb_number(est_woba))
}

fetch_savant_hitter_barrels <- function(year, http_get = default_http_get, min_rows = 1) {
  fetch_savant_csv("statcast", year, "batter", "barrels", http_get, min_rows) |>
    transmute(mlbamid, Barrels = parse_mlb_number(barrels))
}

fetch_savant_pitcher_xera <- function(year, http_get = default_http_get, min_rows = 1) {
  fetch_savant_csv("expected_statistics", year, "pitcher", "xera", http_get, min_rows) |>
    transmute(mlbamid, x_era = parse_mlb_number(xera))
}

fetch_savant_pitcher_custom <- function(year, http_get = default_http_get, min_rows = 1) {
  fetch_savant_csv("custom", year, "pitcher", c("barrel_batted_rate", "oz_swing_percent", "whiff_percent"), http_get, min_rows) |>
    transmute(
      mlbamid,
      barrel_percent = parse_mlb_number(barrel_batted_rate),
      o_swing_percent = parse_mlb_number(oz_swing_percent),
      whiff_percent = parse_mlb_number(whiff_percent)
    )
}

# =============================================================================
# COMPUTED FIELDS
# =============================================================================

# FanGraphs' FIP in these files counts intentional walks (BB + HBP); leaving them out did not reproduce it.
# The constant makes league FIP equal league ERA, computed per season from the league totals.
fip_constant <- function(ip, er, hr, bb, hbp, so) {
  league_era <- 9 * sum(er, na.rm = TRUE) / sum(ip, na.rm = TRUE)
  league_era - (13 * sum(hr, na.rm = TRUE) + 3 * (sum(bb, na.rm = TRUE) + sum(hbp, na.rm = TRUE)) - 2 * sum(so, na.rm = TRUE)) / sum(ip, na.rm = TRUE)
}

compute_fip <- function(hr, bb, hbp, so, ip, constant) {
  safe_divide(13 * hr + 3 * (bb + hbp) - 2 * so, ip) + constant
}

compute_babip <- function(h, hr, ab, so, sf) {
  safe_divide(h - hr, ab - so - hr + sf)
}

# Left on base %, as a percent. Clamped to 0-100 because a pitcher with a single inning can land outside it; NA when
# nobody could have scored (no baserunners, or one home run and nothing else).
compute_lob_percent <- function(h, bb, hbp, hr, r) {
  denominator <- h + bb + hbp - 1.4 * hr
  lob <- if_else(denominator <= 0, NA_real_, 100 * (h + bb + hbp - r) / denominator)
  pmin(100, pmax(0, lob))
}

# =============================================================================
# SEASON TABLES
# =============================================================================

# Everyone with a plate appearance, except pitchers who happened to bat. Ohtani's primary position is TWP, so Ohtani stays.
# A hitter Savant has no batted-ball row for (a few players with one to three PA) has no barrels, so 0.
build_hitter_season <- function(mlb_hitting, mlb_woba, savant_xwoba, savant_barrels) {
  mlb_hitting |>
    filter(PA > 0, is.na(primary_position) | primary_position != "P") |>
    left_join(mlb_woba, by = "mlbamid") |>
    left_join(savant_xwoba, by = "mlbamid") |>
    left_join(savant_barrels, by = "mlbamid") |>
    mutate(Barrels = if_else(is.na(Barrels), 0, Barrels)) |>
    select(mlbamid, Name, Age, G, AB, PA, H, `2B`, `3B`, HR, BB, HBP, SF, SO, TB, Barrels, wOBA, xwOBA) |>
    arrange(as.numeric(mlbamid))
}

# Everyone who faced a batter, except position players who pitched in a blowout (Ohtani's primary position is TWP, so Ohtani
# stays). The FIP constant uses the whole league, position players included, so it is computed before anyone is filtered out.
build_pitcher_season <- function(mlb_pitching, savant_xera, savant_custom) {
  fip_const <- with(mlb_pitching, fip_constant(ip, er, hr, bb, hbp, so))
  mlb_pitching |>
    filter(tbf > 0, is.na(primary_position) | primary_position %in% c("P", "TWP")) |>
    left_join(savant_xera, by = "mlbamid") |>
    left_join(savant_custom, by = "mlbamid") |>
    mutate(
      position = classify_pitcher_role(gs, g, ip),
      fip = compute_fip(hr, bb, hbp, so, ip, fip_const),
      babip = compute_babip(h, hr, ab, so, sf),
      lob_percent = compute_lob_percent(h, bb, hbp, hr, r)
    ) |>
    select(mlbamid, name, age, position, g, gs, ip, tbf, h, r, er, hr, bb, ibb, hbp, so, ab, sf, x_era, fip, barrel_percent, o_swing_percent, whiff_percent, babip, lob_percent) |>
    mutate(across(c(ip, fip, babip, lob_percent), ~ round(.x, 6))) |>
    arrange(as.numeric(mlbamid))
}

# Fewest rows each feed should return for a full season (2023-2025 returned 650-870). A feed far below that means a
# changed or throttled response, which should stop the run rather than write a thin file.
full_season_min_rows <- list(
  mlb_hitting = 600, mlb_pitching = 700, mlb_woba = 600,
  savant_xwoba = 600, savant_barrels = 600, savant_xera = 700, savant_custom = 700
)
# For a season in progress: only rule out an empty or truncated response.
any_season_min_rows <- list(
  mlb_hitting = 1, mlb_pitching = 1, mlb_woba = 1,
  savant_xwoba = 1, savant_barrels = 1, savant_xera = 1, savant_custom = 1
)

# Seven requests. Returns list(hitters, pitchers).
build_season_stats <- function(year, min_rows = full_season_min_rows, http_get = default_http_get) {
  cat("Building", year, "season stats from the MLB Stats API and Baseball Savant\n")
  list(
    hitters = build_hitter_season(
      fetch_mlb_hitting_season(year, http_get, min_rows$mlb_hitting),
      fetch_mlb_hitter_woba(year, http_get, min_rows$mlb_woba),
      fetch_savant_hitter_xwoba(year, http_get, min_rows$savant_xwoba),
      fetch_savant_hitter_barrels(year, http_get, min_rows$savant_barrels)
    ),
    pitchers = build_pitcher_season(
      fetch_mlb_pitching_season(year, http_get, min_rows$mlb_pitching),
      fetch_savant_pitcher_xera(year, http_get, min_rows$savant_xera),
      fetch_savant_pitcher_custom(year, http_get, min_rows$savant_custom)
    )
  )
}

# =============================================================================
# BASELINE FILES
# =============================================================================

baseline_file <- function(kind, year, dir = ".") {
  file.path(dir, paste0("baseline-", kind, "-", year, ".csv"))
}

# Writes baseline-hitters-<year>.csv and baseline-pitchers-<year>.csv for a finished season. Refuses to replace
# existing files unless force is TRUE, and writes neither file if either already exists.
write_baseline_season <- function(year, stats, dir = ".", force = FALSE) {
  files <- c(hitters = baseline_file("hitters", year, dir), pitchers = baseline_file("pitchers", year, dir))
  existing <- files[file.exists(files)]
  if (length(existing) > 0 && !force) {
    stop("Refusing to overwrite ", paste(existing, collapse = ", "), "; pass --force to replace it", call. = FALSE)
  }
  write_csv(stats$hitters, files[["hitters"]])
  write_csv(stats$pitchers, files[["pitchers"]])
  cat("Wrote", files[["hitters"]], "(", nrow(stats$hitters), "hitters ) and", files[["pitchers"]], "(", nrow(stats$pitchers), "pitchers )\n")
  invisible(files)
}

build_baseline_season <- function(year, force = FALSE, dir = ".", http_get = default_http_get) {
  files <- c(baseline_file("hitters", year, dir), baseline_file("pitchers", year, dir))
  if (any(file.exists(files)) && !force) {
    stop("Refusing to overwrite ", paste(files[file.exists(files)], collapse = ", "), "; pass --force to replace it", call. = FALSE)
  }
  write_baseline_season(year, build_season_stats(year, http_get = http_get), dir, force)
}
