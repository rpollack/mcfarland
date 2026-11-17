library(dplyr)
library(readr)
library(baseballr)
library(stringr)
library(stringi)
library(httr)
library(jsonlite)
library(janitor)

# =============================================================================
# CONFIGURATION
# =============================================================================
# Yearly rollover: bump current_year once the new season has games (before that the stats APIs return no rows
# and this script stops), and add last season's exports as fangraphs-leaderboards-<year>.csv and
# pitcher-stats-<year>.csv. The hitter file is a FanGraphs leaderboard export; create the pitcher file with
#   Rscript refresh_data.R export-pitcher-season <last season>
# so it has the columns the baseline expects. The three seasons before current_year are the baseline, oldest
# first; the weights make last season count five times as much as the oldest one.
current_year <- 2026
baseline_seasons <- data.frame(year = current_year - 3:1, weight = c(1, 3, 5))

# The metrics compared against each player's own baseline. Each gets <metric>_diff (current minus baseline) and
# <metric>_lg_adj_diff (that difference minus how far the whole league moved).
hitter_metric_cols <- c("AVG", "OBP", "SLG", "K_pct", "BB_pct", "Barrel_pct", "BABIP", "wOBA", "xwOBA", "xwOBA_wOBA_gap")
pitcher_metric_cols <- c("era", "k_percent", "bb_percent", "k_minus_bb_percent", "xera", "o_swing_percent", "csw_percent", "barrel_percent", "lob_percent", "babip")

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

# The MLB and FanGraphs endpoints fail transiently now and then; a few retries keep one blip from costing a day
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

# =============================================================================
# FRESHNESS
# =============================================================================

read_data_through_date <- function() {
  if (!file.exists("data_freshness.json")) {
    return(NA_character_)
  }

  parsed <- fromJSON("data_freshness.json")
  if (is.null(parsed$data_through_date)) {
    return(NA_character_)
  }
  as.character(parsed$data_through_date)
}

# The refresh runs in the morning, so the stats cover games through yesterday (Central time).
current_data_through_date <- function() {
  as.character(as.Date(Sys.time(), tz = "America/Chicago") - 1)
}

# =============================================================================
# FETCHING CURRENT-SEASON DATA
# =============================================================================

fetch_mlb_stats <- function(current_year, group) {
  url <- paste0(
    "https://statsapi.mlb.com/api/v1/stats?",
    "stats=season",
    "&group=", group,
    "&playerPool=ALL",
    "&season=", current_year,
    "&hydrate=person(currentTeam,primaryPosition)",
    "&limit=10000"
  )

  response <- with_retry(function() {
    response <- GET(url, timeout(60))
    if (http_error(response)) {
      stop("status ", status_code(response))
    }
    response
  }, paste("MLB Stats API request for", group))

  parsed <- fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
  splits <- parsed$stats$splits[[1]]
  if (is.null(splits) || nrow(splits) == 0) {
    stop("MLB Stats API returned no ", group, " rows")
  }

  tibble::as_tibble(splits)
}

fetch_mlb_hitter_basics <- function(current_year) {
  fetch_mlb_stats(current_year, "hitting") |>
    transmute(
      mlbamid = normalize_mlbam_id(player.id),
      AB_mlb = parse_mlb_number(stat.atBats),
      PA_mlb = parse_mlb_number(stat.plateAppearances),
      H_mlb = parse_mlb_number(stat.hits),
      HR_mlb = parse_mlb_number(stat.homeRuns),
      BB_mlb = parse_mlb_number(stat.baseOnBalls),
      HBP_mlb = parse_mlb_number(stat.hitByPitch),
      SF_mlb = parse_mlb_number(stat.sacFlies),
      SO_mlb = parse_mlb_number(stat.strikeOuts),
      TB_mlb = parse_mlb_number(stat.totalBases)
    )
}

fetch_mlb_pitcher_basics <- function(current_year) {
  fetch_mlb_stats(current_year, "pitching") |>
    transmute(
      mlbamid = normalize_mlbam_id(player.id),
      g_mlb = parse_mlb_number(stat.gamesPitched),
      gs_mlb = parse_mlb_number(stat.gamesStarted),
      ip_mlb = parse_mlb_innings(stat.inningsPitched),
      tbf_mlb = parse_mlb_number(stat.battersFaced),
      h_mlb = parse_mlb_number(stat.hits),
      er_mlb = parse_mlb_number(stat.earnedRuns),
      r_mlb = parse_mlb_number(stat.runs),
      hr_mlb = parse_mlb_number(stat.homeRuns),
      so_mlb = parse_mlb_number(stat.strikeOuts),
      bb_mlb = parse_mlb_number(stat.baseOnBalls),
      ibb_mlb = parse_mlb_number(stat.intentionalWalks),
      hbp_mlb = parse_mlb_number(stat.hitByPitch),
      ab_mlb = parse_mlb_number(stat.atBats),
      sf_mlb = parse_mlb_number(stat.sacFlies)
    )
}

fetch_fangraphs_hitter_current_contract <- function(current_year) {
  fg_hitters <- with_retry(
    function() fg_batter_leaders(pos = "np", startseason = current_year, endseason = current_year),
    "FanGraphs hitter leaders request"
  )
  fg_hitters |>
    require_columns(c("PlayerName", "Age", "playerid", "xMLBAMID", "AB", "PA", "1B", "2B", "3B", "HR", "H", "HBP", "SF", "wOBA", "xwOBA", "SO", "BB", "Barrels"), "FanGraphs hitter leaders") |>
    select(Name = PlayerName, Age, PlayerId = playerid, mlbamid = xMLBAMID, AB, PA, `1B`, `2B`, `3B`, HR, H, HBP, SF, wOBA, xwOBA, SO, BB, Barrels) |>
    mutate(
      PlayerId = as.character(PlayerId),
      mlbamid = normalize_mlbam_id(mlbamid),
      Name = stri_trans_general(Name, id = "Latin-ASCII"),
      TB = `1B` + 2 * `2B` + 3 * `3B` + 4 * HR
    )
}

fetch_fangraphs_pitcher_current_contract <- function(current_year) {
  params <- list(
    pos       = "all",
    stats     = "pit",
    lg        = "all",
    qual      = "1",
    season    = as.character(current_year),
    season1   = as.character(current_year),
    startdate = "",
    enddate   = "",
    month     = "0",
    ind       = "0",
    type      = "c,7,8,13,-1,43,44,48,51,-1,-1,6,332,45,62,-1,59,6,17,18,19,20,24,14,45,62,43,51,324,328,332,139,140,141,142,143,144,204,210,3,331",
    sortcol   = "4",
    sortdir   = "default",
    pageitems = "2000000",
    pagenum   = "1"
  )

  qstring <- paste0(names(params), "=",
                    vapply(params, URLencode, "", reserved = TRUE),
                    collapse = "&"
  )
  url_api <- paste0("https://www.fangraphs.com/api/leaders/major-league/data?", qstring)

  raw <- with_retry(function() fromJSON(url_api), "FanGraphs pitcher leaders request")
  raw$data |>
    tibble::as_tibble() |>
    clean_names() |>
    mutate(name = str_remove_all(name, "<[^>]+>")) |>
    require_columns(c("playerid", "x_mlbamid", "name", "season", "age", "g", "gs", "ip", "tbf", "h", "er", "hr", "so", "bb", "ibb", "hbp", "babip", "lob_percent", "fip", "x_fip", "hr_fb", "f_bv", "c_tv", "s_fv", "o_swing_percent", "c_sw_str_percent", "x_era", "position", "ev90", "barrels", "barrel_percent"), "FanGraphs pitcher leaders") |>
    select(playerid, mlbamid = x_mlbamid, name, season, age, g, gs, ip, tbf, h, er, hr, so, bb, ibb, hbp, babip, lob_percent, fip, x_fip, hr_fb, f_bv, c_tv, s_fv, o_swing_percent, csw_percent = c_sw_str_percent, x_era, position, ev90, barrels, barrel_percent) |>
    mutate(
      playerid = as.character(playerid),
      mlbamid = normalize_mlbam_id(mlbamid),
      name = stri_trans_general(name, id = "Latin-ASCII")
    )
}

# =============================================================================
# SHARED AGGREGATION
# =============================================================================
# The same hitter and pitcher metrics are needed three ways: per player over the baseline seasons, league-wide
# over the baseline seasons, and (hitters) per player and league-wide for the current season. Each way feeds the
# same summary function so the formulas cannot drift apart. Input rows carry a weight column `w`: the season
# weight for baselines, 1 for the current season.

summarize_hitter_metrics <- function(data, na.rm = TRUE, ...) {
  summarize(
    data,
    AVG = safe_divide(sum(H * w, na.rm = na.rm), sum(AB * w, na.rm = na.rm)),
    OBP = safe_divide(sum(H * w, na.rm = na.rm) + sum(BB * w, na.rm = na.rm) + sum(HBP * w, na.rm = na.rm), sum(AB * w, na.rm = na.rm) + sum(BB * w, na.rm = na.rm) + sum(HBP * w, na.rm = na.rm) + sum(SF * w, na.rm = na.rm)),
    SLG = safe_divide(sum(TB * w, na.rm = na.rm), sum(AB * w, na.rm = na.rm)),
    K_pct = 100 * safe_divide(sum(SO * w, na.rm = na.rm), sum(PA * w, na.rm = na.rm)),
    BB_pct = 100 * safe_divide(sum(BB * w, na.rm = na.rm), sum(PA * w, na.rm = na.rm)),
    Barrel_pct = 100 * safe_divide(sum(Barrels * w, na.rm = na.rm), sum(PA * w, na.rm = na.rm)),
    BABIP = safe_divide(sum(H * w, na.rm = na.rm) - sum(HR * w, na.rm = na.rm), sum(AB * w, na.rm = na.rm) - sum(SO * w, na.rm = na.rm) - sum(HR * w, na.rm = na.rm) + sum(SF * w, na.rm = na.rm)),
    wOBA = weighted.mean(wOBA, w = PA * w, na.rm = TRUE),
    xwOBA = weighted.mean(xwOBA, w = PA * w, na.rm = TRUE),
    xwOBA_wOBA_gap = xwOBA - wOBA,
    PA = sum(PA, na.rm = na.rm),
    ...,
    .groups = "drop"
  ) |>
    mutate(across(c(wOBA, xwOBA, xwOBA_wOBA_gap, Barrel_pct), nan_to_na))
}

summarize_pitcher_baseline_metrics <- function(data, na.rm = TRUE) {
  summarize(
    data,
    era = safe_divide(sum(er * w, na.rm = na.rm), sum(ip * w, na.rm = na.rm)) * 9,
    k_percent = safe_divide(sum(so * w, na.rm = na.rm), sum(tbf * w, na.rm = na.rm)) * 100,
    bb_percent = safe_divide(sum(bb * w, na.rm = na.rm), sum(tbf * w, na.rm = na.rm)) * 100,
    k_minus_bb_percent = k_percent - bb_percent,
    xera = weighted.mean(x_era, w = tbf * w, na.rm = TRUE),
    era_xera_gap = era - xera,
    fip = weighted.mean(fip, w = tbf * w, na.rm = TRUE),
    era_fip_gap = era - fip,
    barrel_percent = weighted.mean(barrel_percent, w = tbf * w, na.rm = TRUE) * 100,
    o_swing_percent = weighted.mean(o_swing_percent, w = tbf * w, na.rm = TRUE) * 100,
    babip = weighted.mean(babip, w = tbf * w, na.rm = TRUE),
    lob_percent = weighted.mean(lob_percent, w = tbf * w, na.rm = TRUE) * 100,
    csw_percent = weighted.mean(c_sw_str_percent, w = tbf * w, na.rm = TRUE) * 100,
    hr_fb = weighted.mean(hr_fb, w = tbf * w, na.rm = TRUE),
    .groups = "drop"
  )
}

# =============================================================================
# BASELINES (weighted history)
# =============================================================================

# Every season file must carry the columns the summaries use. A file in another shape (a raw FanGraphs export
# instead of the layout below, say) would otherwise bind in as all-NA rows and quietly drop that season.
hitter_baseline_columns <- c("Name", "PlayerId", "AB", "PA", "H", "1B", "2B", "3B", "HR", "BB", "HBP", "SF", "SO", "Barrels", "wOBA", "xwOBA")
pitcher_baseline_columns <- c("playerid", "name", "position", "er", "ip", "so", "bb", "tbf", "x_era", "fip", "barrel_percent", "o_swing_percent", "babip", "lob_percent", "c_sw_str_percent", "hr_fb")

read_baseline_seasons <- function(file_prefix, required_columns) {
  seasons <- lapply(seq_len(nrow(baseline_seasons)), function(i) {
    file <- paste0(file_prefix, baseline_seasons$year[i], ".csv")
    if (!file.exists(file)) {
      stop("Baseline season file ", file, " is missing; add last season's export before refreshing.", call. = FALSE)
    }
    read_csv(file, show_col_types = FALSE) |>
      require_columns(required_columns, file) |>
      mutate(year = baseline_seasons$year[i], w = baseline_seasons$weight[i])
  })
  bind_rows(seasons)
}

read_hitter_baseline_seasons <- function() {
  read_baseline_seasons("fangraphs-leaderboards-", hitter_baseline_columns) |>
    mutate(
      TB = `1B` + 2 * `2B` + 3 * `3B` + 4 * HR,
      Name = stri_trans_general(Name, id = "Latin-ASCII")
    )
}

read_pitcher_baseline_seasons <- function() {
  read_baseline_seasons("pitcher-stats-", pitcher_baseline_columns) |>
    mutate(playerid = as.character(playerid))
}

# Writes pitcher-stats-<year>.csv for a finished season: the same FanGraphs pull used for the current season,
# with the role classified the way the current-season stats classify it, in the layout read_baseline_seasons
# expects. Run it once a year (see CONFIGURATION).
export_pitcher_season <- function(year) {
  file <- paste0("pitcher-stats-", year, ".csv")
  fetch_fangraphs_pitcher_current_contract(year) |>
    mutate(position = classify_pitcher_role(gs, g, ip)) |>
    rename(c_sw_str_percent = csw_percent) |>
    write_csv(file)
  cat("Wrote", file, "\n")
}

# =============================================================================
# CURRENT SEASON
# =============================================================================

# FanGraphs supplies the player list and the Statcast fields; MLB's own totals win for the counting stats
# because they update sooner. Rows with no MLB match fall back to FanGraphs and are flagged source_fallback.
join_hitter_sources <- function(fg_contract, mlb_basics) {
  fg_contract |>
    left_join(mlb_basics, by = "mlbamid") |>
    mutate(
      source_fallback = is.na(PA_mlb),
      AB = coalesce_joined(AB_mlb, AB),
      PA = coalesce_joined(PA_mlb, PA),
      H = coalesce_joined(H_mlb, H),
      HR = coalesce_joined(HR_mlb, HR),
      BB = coalesce_joined(BB_mlb, BB),
      HBP = coalesce_joined(HBP_mlb, HBP),
      SF = coalesce_joined(SF_mlb, SF),
      SO = coalesce_joined(SO_mlb, SO),
      TB = coalesce_joined(TB_mlb, TB),
      w = 1
    )
}

combine_hitter_current_stats <- function(hitters_joined) {
  hitters_joined |>
    group_by(Name, PlayerId) |>
    summarize_hitter_metrics(Age = first(Age), mlbamid = first(mlbamid), source_fallback = any(source_fallback))
}

combine_pitcher_current_stats <- function(fg_contract, mlb_basics) {
  fg_contract |>
    left_join(mlb_basics, by = "mlbamid") |>
    mutate(
      g = coalesce_joined(g_mlb, g),
      gs = coalesce_joined(gs_mlb, gs),
      ip = coalesce_joined(ip_mlb, ip),
      tbf = coalesce_joined(tbf_mlb, tbf),
      h = coalesce_joined(h_mlb, h),
      er = coalesce_joined(er_mlb, er),
      # The FanGraphs pitcher feed has no runs allowed, so an unmatched pitcher falls back to earned runs. That
      # only slightly overstates his LOB%.
      r = coalesce_joined(r_mlb, er),
      hr = coalesce_joined(hr_mlb, hr),
      so = coalesce_joined(so_mlb, so),
      bb = coalesce_joined(bb_mlb, bb),
      ibb = coalesce_joined(ibb_mlb, ibb),
      hbp = coalesce_joined(hbp_mlb, hbp),
      ab = ab_mlb,
      sf = sf_mlb,
      xera = x_era,
      position = classify_pitcher_role(gs, g, ip),
      k_percent = 100 * safe_divide(so, tbf),
      bb_percent = 100 * safe_divide(bb, tbf),
      k_minus_bb_percent = k_percent - bb_percent,
      era = 9 * safe_divide(er, ip),
      xera_gap = era - x_era,
      fip_era_gap = era - fip,
      babip = safe_divide(h - hr, ab - so - hr + sf),
      lob_denominator = h + bb + hbp - (1.4 * hr),
      lob_percent = if_else(lob_denominator <= 0, NA_real_, 100 * safe_divide(h + bb + hbp - r, lob_denominator)),
      barrel_percent = barrel_percent * 100,
      csw_percent = csw_percent * 100,
      o_swing_percent = o_swing_percent * 100,
      source_fallback = is.na(tbf_mlb)
    )
}

build_pitcher_league_current <- function(pitching_stats_current) {
  pitching_stats_current |>
    summarize(
      era = 9 * safe_divide(sum(er, na.rm = TRUE), sum(ip, na.rm = TRUE)),
      k_percent = 100 * safe_divide(sum(so, na.rm = TRUE), sum(tbf, na.rm = TRUE)),
      bb_percent = 100 * safe_divide(sum(bb, na.rm = TRUE), sum(tbf, na.rm = TRUE)),
      k_minus_bb_percent = k_percent - bb_percent,
      xera = weighted.mean(xera, w = tbf, na.rm = TRUE),
      o_swing_percent = weighted.mean(o_swing_percent, w = tbf, na.rm = TRUE),
      csw_percent = weighted.mean(csw_percent, w = tbf, na.rm = TRUE),
      barrel_percent = weighted.mean(barrel_percent, w = tbf, na.rm = TRUE),
      lob_percent = weighted.mean(lob_percent, w = tbf, na.rm = TRUE),
      babip = weighted.mean(babip, w = tbf, na.rm = TRUE)
    )
}

# =============================================================================
# DIFFS AGAINST BASELINE
# =============================================================================
# `data` holds <metric>_cur and <metric>_l3 columns from joining current stats to the baseline.

add_baseline_diffs <- function(data, metric_cols) {
  for (metric in metric_cols) {
    data[[paste0(metric, "_diff")]] <- data[[paste0(metric, "_cur")]] - data[[paste0(metric, "_l3")]]
  }
  data
}

build_league_diffs <- function(current, baseline, metric_cols) {
  diffs <- list()
  for (metric in metric_cols) {
    diffs[[metric]] <- current[[metric]][1] - baseline[[metric]][1]
  }
  diffs
}

add_league_adjusted_diffs <- function(data, metric_cols, league_diffs) {
  for (metric in metric_cols) {
    diff_col <- paste0(metric, "_diff")
    adjusted_col <- paste0(metric, "_lg_adj_diff")
    data[[adjusted_col]] <- data[[diff_col]] - league_diffs[[metric]]
  }
  data
}

# =============================================================================
# LOOKUP AND VALIDATION
# =============================================================================

build_player_lookup <- function(full_stats_hitters, full_stats_pitchers) {
  hitter_names <- full_stats_hitters$Name
  pitcher_names <- full_stats_pitchers$Name
  duplicate_names <- intersect(hitter_names, pitcher_names)

  bind_rows(
    full_stats_hitters |>
      select(Name, PlayerId, Age, player_type, mlbamid) |>
      mutate(
        compound_id = paste0(PlayerId, "_", player_type),
        display_name = ifelse(Name %in% duplicate_names, paste0(Name, " (Hitter)"), Name),
        position_detail = "Hitter"
      ),
    full_stats_pitchers |>
      select(Name, PlayerId, Age, player_type, position, mlbamid) |>
      mutate(
        compound_id = paste0(PlayerId, "_", player_type),
        display_name = ifelse(Name %in% duplicate_names, paste0(Name, " (", position, ")"), Name),
        position_detail = position
      )
  ) |>
    arrange(Name)
}

build_served_player_inventory <- function(locked_hitters, locked_pitchers, locked_lookup) {
  locked_lookup |>
    select(PlayerId, player_type, display_name, compound_id, position_detail) |>
    left_join(
      bind_rows(
        locked_hitters |> select(PlayerId, player_type, Name, Age, mlbamid) |> mutate(locked_source_file = "full_stats_hitters.csv"),
        locked_pitchers |> select(PlayerId, player_type, Name, Age, mlbamid) |> mutate(locked_source_file = "full_stats_pitchers.csv")
      ),
      by = c("PlayerId", "player_type")
    ) |>
    group_by(PlayerId, player_type) |>
    mutate(locked_row_identity = row_number()) |>
    ungroup()
}

row_keys <- function(data) {
  paste0(data$PlayerId, "_", data$player_type)
}

compare_numeric_parity <- function(locked_data, candidate_data, tolerance = 0.001) {
  locked_data <- locked_data |> mutate(.row_key = row_keys(locked_data))
  candidate_data <- candidate_data |> mutate(.row_key = row_keys(candidate_data))
  common_fields <- intersect(colnames(locked_data), colnames(candidate_data))
  common_fields <- setdiff(common_fields, c(".row_key", "PlayerId", "player_type", "Name", "mlbamid", "position", "Age"))

  joined <- locked_data |>
    select(.row_key, all_of(common_fields)) |>
    inner_join(
      candidate_data |> select(.row_key, all_of(common_fields)),
      by = ".row_key",
      suffix = c("_locked", "_candidate")
    )

  mismatches <- lapply(common_fields, function(field) {
    locked_col <- paste0(field, "_locked")
    candidate_col <- paste0(field, "_candidate")
    if (!is.numeric(joined[[locked_col]]) || !is.numeric(joined[[candidate_col]])) {
      return(NULL)
    }

    diff <- abs(joined[[locked_col]] - joined[[candidate_col]])
    bad <- which((is.na(joined[[locked_col]]) != is.na(joined[[candidate_col]])) | (!is.na(diff) & diff > tolerance))
    if (length(bad) == 0) {
      return(NULL)
    }

    tibble::tibble(
      row_key = joined$.row_key[bad],
      field = field,
      locked_value = joined[[locked_col]][bad],
      candidate_value = joined[[candidate_col]][bad],
      difference = joined[[candidate_col]][bad] - joined[[locked_col]][bad]
    )
  })

  bind_rows(mismatches)
}

# Compares the freshly built candidate files with the files currently in production. When the data-through date
# has not moved, the candidate must match production exactly (same players, same stats); when it has moved the
# stats are expected to differ, so only the integrity checks apply (unique IDs, every served player has an MLB id).
validate_migration_parity <- function(locked_hitters, locked_pitchers, locked_lookup, candidate_hitters, candidate_pitchers, candidate_lookup, locked_data_through_date, candidate_data_through_date) {
  locked_all <- bind_rows(locked_hitters, locked_pitchers)
  candidate_all <- bind_rows(candidate_hitters, candidate_pitchers)
  locked_keys <- row_keys(locked_all)
  candidate_keys <- row_keys(candidate_all)
  same_cutoff <- !is.na(locked_data_through_date) && locked_data_through_date == candidate_data_through_date

  missing_players <- setdiff(locked_keys, candidate_keys)
  added_players <- setdiff(candidate_keys, locked_keys)
  missing_mlbamids <- candidate_all |> filter(is.na(mlbamid) | mlbamid == "") |> select(PlayerId, player_type, Name)
  duplicate_compound_ids <- candidate_lookup |> count(compound_id) |> filter(n > 1)

  locked_compounds <- locked_lookup$compound_id
  candidate_compounds <- candidate_lookup$compound_id
  missing_compounds <- setdiff(locked_compounds, candidate_compounds)
  added_compounds <- setdiff(candidate_compounds, locked_compounds)

  fallback_rows <- bind_rows(
    candidate_hitters |> filter(source_fallback) |> transmute(PlayerId, player_type, Name, source_fallback = "fangraphs_basic_missing_mlb_match"),
    candidate_pitchers |> filter(source_fallback) |> transmute(PlayerId, player_type, Name, source_fallback = "fangraphs_basic_missing_mlb_match")
  )
  stat_mismatches <- if (same_cutoff) {
    bind_rows(
      compare_numeric_parity(locked_hitters, candidate_hitters),
      compare_numeric_parity(locked_pitchers, candidate_pitchers)
    )
  } else {
    tibble::tibble()
  }

  report <- list(
    status = "pass",
    locked_data_through_date = locked_data_through_date,
    candidate_data_through_date = candidate_data_through_date,
    same_cutoff = same_cutoff,
    row_counts = list(
      locked_hitters = nrow(locked_hitters),
      candidate_hitters = nrow(candidate_hitters),
      locked_pitchers = nrow(locked_pitchers),
      candidate_pitchers = nrow(candidate_pitchers),
      locked_lookup = nrow(locked_lookup),
      candidate_lookup = nrow(candidate_lookup)
    ),
    missing_players = missing_players,
    added_players = added_players,
    missing_lookup_compound_ids = missing_compounds,
    added_lookup_compound_ids = added_compounds,
    duplicate_compound_ids = duplicate_compound_ids,
    missing_mlbamids = missing_mlbamids,
    fallback_rows = fallback_rows,
    stat_mismatches = stat_mismatches,
    warnings = list()
  )

  failures <- c()
  if (same_cutoff && nrow(locked_hitters) != nrow(candidate_hitters)) failures <- c(failures, "hitter row count changed")
  if (same_cutoff && nrow(locked_pitchers) != nrow(candidate_pitchers)) failures <- c(failures, "pitcher row count changed")
  if (same_cutoff && length(missing_players) > 0) failures <- c(failures, "locked players missing from candidates")
  if (same_cutoff && length(added_players) > 0) failures <- c(failures, "candidate contains non-locked players")
  if (same_cutoff && (length(missing_compounds) > 0 || length(added_compounds) > 0)) failures <- c(failures, "lookup compound IDs changed")
  if (nrow(duplicate_compound_ids) > 0) failures <- c(failures, "duplicate compound IDs")
  if (nrow(missing_mlbamids) > 0) failures <- c(failures, "served players missing mlbamid")
  if (same_cutoff && nrow(stat_mismatches) > 0) failures <- c(failures, "stat parity mismatches")

  report$failures <- failures
  if (length(failures) > 0) {
    report$status <- "fail"
  }

  report
}

promote_candidate_outputs <- function(report) {
  if (!identical(report$status, "pass")) {
    cat("Migration parity failed; production CSVs were not overwritten.\n")
    cat("Failures:", paste(report$failures, collapse = "; "), "\n")
    return(FALSE)
  }

  file.copy("candidate_full_stats_hitters.csv", "full_stats_hitters.csv", overwrite = TRUE)
  file.copy("candidate_full_stats_pitchers.csv", "full_stats_pitchers.csv", overwrite = TRUE)
  file.copy("candidate_player_lookup.csv", "player_lookup.csv", overwrite = TRUE)
  TRUE
}

# =============================================================================
# ENTRY POINT
# =============================================================================

script_args <- commandArgs(trailingOnly = TRUE)
if (length(script_args) == 2 && script_args[1] == "export-pitcher-season") {
  export_pitcher_season(as.integer(script_args[2]))
  quit(save = "no")
}

# =============================================================================
# HITTERS
# =============================================================================

hitter_baseline_history <- read_hitter_baseline_seasons()
hitters_last_3 <- hitter_baseline_history |>
  group_by(Name, PlayerId) |>
  summarize_hitter_metrics(na.rm = FALSE) |>
  mutate(PlayerId = as.character(PlayerId))
hitter_league_baseline <- summarize_hitter_metrics(hitter_baseline_history)

fangraphs_hitters <- fetch_fangraphs_hitter_current_contract(current_year)
mlb_hitter_basics <- fetch_mlb_hitter_basics(current_year)
hitters_joined <- join_hitter_sources(fangraphs_hitters, mlb_hitter_basics)
hitters_this_year <- combine_hitter_current_stats(hitters_joined)
hitter_league_current <- summarize_hitter_metrics(hitters_joined)
hitter_league_diffs <- build_league_diffs(hitter_league_current, hitter_league_baseline, hitter_metric_cols)

full_stats_hitters <-
  hitters_this_year |>
  left_join(hitters_last_3, by = c("Name", "PlayerId"), suffix = c("_cur", "_l3")) |>
  add_baseline_diffs(hitter_metric_cols) |>
  mutate(player_type = "hitter") |>
  add_league_adjusted_diffs(hitter_metric_cols, hitter_league_diffs) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# =============================================================================
# PITCHERS
# =============================================================================

pitcher_baseline_history <- read_pitcher_baseline_seasons()
pitching_stats_last_3 <- pitcher_baseline_history |>
  group_by(playerid, name, position) |>
  summarize_pitcher_baseline_metrics(na.rm = FALSE)
pitcher_league_baseline <- summarize_pitcher_baseline_metrics(pitcher_baseline_history)

fangraphs_pitchers <- fetch_fangraphs_pitcher_current_contract(current_year)
mlb_pitcher_basics <- fetch_mlb_pitcher_basics(current_year)
pitching_stats_current <- combine_pitcher_current_stats(fangraphs_pitchers, mlb_pitcher_basics)
pitcher_league_current <- build_pitcher_league_current(pitching_stats_current)
pitcher_league_diffs <- build_league_diffs(pitcher_league_current, pitcher_league_baseline, pitcher_metric_cols)

# Baselines are role-specific (SP vs RP), so match on playerid and position. Names are not part of the key
# because the same player can be spelled differently across sources (for example "Jake Latz" and "Jacob Latz"),
# which silently dropped their baseline. If a spelling variant ever produces two baseline rows for one player
# and role, keep the first so the join cannot duplicate current-season rows.
pitching_stats_last_3_by_role <- pitching_stats_last_3 |>
  select(-name) |>
  distinct(playerid, position, .keep_all = TRUE)

full_stats_pitchers <-
  pitching_stats_current |>
  left_join(pitching_stats_last_3_by_role, by = c("playerid", "position"), suffix = c("_cur", "_l3")) |>
  add_baseline_diffs(pitcher_metric_cols) |>
  mutate(player_type = "pitcher") |>
  add_league_adjusted_diffs(pitcher_metric_cols, pitcher_league_diffs) |>
  select(-matches("_mlb$"), -any_of(c("team", "primary_position", "r", "ab", "sf", "lob_denominator"))) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  rename(Name = name, PlayerId = playerid, Age = age)

# =============================================================================
# VALIDATE CANDIDATES, THEN PROMOTE
# =============================================================================

candidate_full_stats_hitters <- full_stats_hitters |> select(-source_fallback)
candidate_full_stats_pitchers <- full_stats_pitchers |> select(-source_fallback)
candidate_player_lookup <- build_player_lookup(candidate_full_stats_hitters, candidate_full_stats_pitchers)

locked_full_stats_hitters <- read_csv("full_stats_hitters.csv", show_col_types = FALSE) |>
  mutate(PlayerId = as.character(PlayerId), player_type = "hitter")
locked_full_stats_pitchers <- read_csv("full_stats_pitchers.csv", show_col_types = FALSE) |>
  mutate(PlayerId = as.character(PlayerId), player_type = "pitcher")
locked_player_lookup <- read_csv("player_lookup.csv", show_col_types = FALSE) |>
  mutate(PlayerId = as.character(PlayerId))

served_player_inventory <- build_served_player_inventory(locked_full_stats_hitters, locked_full_stats_pitchers, locked_player_lookup)

locked_data_through_date <- read_data_through_date()
data_through_date <- current_data_through_date()

parity_report <- validate_migration_parity(
  locked_full_stats_hitters,
  locked_full_stats_pitchers,
  locked_player_lookup,
  full_stats_hitters,
  full_stats_pitchers,
  candidate_player_lookup,
  locked_data_through_date,
  data_through_date
)

write_csv(candidate_full_stats_hitters, "candidate_full_stats_hitters.csv")
write_csv(candidate_full_stats_pitchers, "candidate_full_stats_pitchers.csv")
write_csv(candidate_player_lookup, "candidate_player_lookup.csv")
write_csv(served_player_inventory, "served_player_inventory.csv")
write_json(parity_report, "migration_parity_report.json", auto_unbox = TRUE, pretty = TRUE, na = "null")

promoted <- promote_candidate_outputs(parity_report)

if (promoted) {
  write_json(
    list(
      refreshed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      refreshed_timezone = "UTC",
      data_through_date = data_through_date
    ),
    "data_freshness.json",
    auto_unbox = TRUE,
    pretty = TRUE
  )
}

cat("Data refreshed at:", as.character(Sys.time()), "\n")
cat("Migration parity status:", parity_report$status, "\n")
cat("Candidate hitters:", nrow(candidate_full_stats_hitters), "\n")
cat("Candidate pitchers:", nrow(candidate_full_stats_pitchers), "\n")
cat("Candidate total players:", nrow(candidate_player_lookup), "\n")
cat("Production files promoted:", promoted, "\n")

# Check MLB ID coverage
hitters_with_mlb_id <- sum(!is.na(candidate_full_stats_hitters$mlbamid) & candidate_full_stats_hitters$mlbamid != "")
pitchers_with_mlb_id <- sum(!is.na(candidate_full_stats_pitchers$mlbamid) & candidate_full_stats_pitchers$mlbamid != "")

cat("Hitters with MLB IDs:", hitters_with_mlb_id, "/", nrow(candidate_full_stats_hitters), "\n")
cat("Pitchers with MLB IDs:", pitchers_with_mlb_id, "/", nrow(candidate_full_stats_pitchers), "\n")
