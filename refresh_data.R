library(dplyr)
library(readr)
library(baseballr)
library(stringr)
library(stringi)
library(httr)
library(jsonlite)
library(janitor)

current_year <- 2026

hitter_baseline_cache_file <- paste0("weighted_baselines_hitters_", current_year, ".csv")
pitcher_baseline_cache_file <- paste0("weighted_baselines_pitchers_", current_year, ".csv")

read_or_build_baseline <- function(cache_file, builder, label) {
  if (file.exists(cache_file)) {
    cached <- read_csv(cache_file, show_col_types = FALSE)
    if ("baseline_for_year" %in% colnames(cached) && all(cached$baseline_for_year == current_year)) {
      cat("Using cached", label, "weighted baselines from", cache_file, "\n")
      return(cached |> select(-baseline_for_year))
    }

    cat("Ignoring stale", label, "weighted baseline cache:", cache_file, "\n")
  }

  cat("Building", label, "weighted baselines for", current_year, "\n")
  baseline <- builder()
  write_csv(baseline |> mutate(baseline_for_year = current_year), cache_file)
  baseline
}

safe_divide <- function(numerator, denominator) {
  ifelse(is.na(denominator) | denominator == 0, NA_real_, numerator / denominator)
}

parse_mlb_number <- function(value) {
  parsed <- suppressWarnings(as.numeric(value))
  parsed[is.nan(parsed)] <- NA_real_
  parsed
}

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

coalesce_joined <- function(primary, fallback) {
  ifelse(!is.na(primary), primary, fallback)
}

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

current_data_through_date <- function() {
  refresh_time_ct <- as.POSIXct(format(Sys.time(), tz = "America/Chicago", usetz = TRUE), tz = "America/Chicago")
  as.character(as.Date(refresh_time_ct) - 1)
}

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

  response <- GET(url)
  if (http_error(response)) {
    stop("MLB Stats API request failed for ", group, ": ", status_code(response))
  }

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
      team = team.name,
      primary_position = player.primaryPosition.abbreviation,
      AB_mlb = parse_mlb_number(stat.atBats),
      PA_mlb = parse_mlb_number(stat.plateAppearances),
      H_mlb = parse_mlb_number(stat.hits),
      `2B_mlb` = parse_mlb_number(stat.doubles),
      `3B_mlb` = parse_mlb_number(stat.triples),
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
      team = team.name,
      primary_position = player.primaryPosition.abbreviation,
      g_mlb = parse_mlb_number(stat.gamesPitched),
      gs_mlb = parse_mlb_number(stat.gamesStarted),
      ip_mlb = parse_mlb_innings(stat.inningsPitched),
      outs_mlb = parse_mlb_number(stat.outs),
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
  fg_batter_leaders(pos = "np", startseason = current_year, endseason = current_year) |>
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

  raw <- fromJSON(url_api)
  raw$data |>
    tibble::as_tibble() |>
    clean_names() |>
    mutate(name = str_remove_all(name, "<[^>]+>")) |>
    select(playerid, mlbamid = x_mlbamid, name, season, age, g, gs, ip, tbf, h, er, hr, so, bb, ibb, hbp, babip, lob_percent, fip, x_fip, hr_fb, f_bv, c_tv, s_fv, o_swing_percent, csw_percent = c_sw_str_percent, x_era, position, ev90, barrels, barrel_percent) |>
    mutate(
      playerid = as.character(playerid),
      mlbamid = normalize_mlbam_id(mlbamid),
      name = stri_trans_general(name, id = "Latin-ASCII")
    )
}

combine_hitter_current_stats <- function(fg_contract, mlb_basics) {
  fg_contract |>
    left_join(mlb_basics, by = "mlbamid") |>
    mutate(
      AB_src = coalesce_joined(AB_mlb, AB),
      PA_src = coalesce_joined(PA_mlb, PA),
      H_src = coalesce_joined(H_mlb, H),
      `2B_src` = coalesce_joined(`2B_mlb`, `2B`),
      `3B_src` = coalesce_joined(`3B_mlb`, `3B`),
      HR_src = coalesce_joined(HR_mlb, HR),
      BB_src = coalesce_joined(BB_mlb, BB),
      HBP_src = coalesce_joined(HBP_mlb, HBP),
      SF_src = coalesce_joined(SF_mlb, SF),
      SO_src = coalesce_joined(SO_mlb, SO),
      TB_src = coalesce_joined(TB_mlb, TB),
      source_fallback = is.na(PA_mlb)
    ) |>
    group_by(Name, PlayerId) |>
    summarize(
      AVG = safe_divide(sum(H_src, na.rm = TRUE), sum(AB_src, na.rm = TRUE)),
      OBP = safe_divide(sum(H_src, na.rm = TRUE) + sum(BB_src, na.rm = TRUE) + sum(HBP_src, na.rm = TRUE), sum(AB_src, na.rm = TRUE) + sum(BB_src, na.rm = TRUE) + sum(HBP_src, na.rm = TRUE) + sum(SF_src, na.rm = TRUE)),
      SLG = safe_divide(sum(TB_src, na.rm = TRUE), sum(AB_src, na.rm = TRUE)),
      K_pct = 100 * safe_divide(sum(SO_src, na.rm = TRUE), sum(PA_src, na.rm = TRUE)),
      BB_pct = 100 * safe_divide(sum(BB_src, na.rm = TRUE), sum(PA_src, na.rm = TRUE)),
      Barrel_pct = 100 * safe_divide(sum(Barrels, na.rm = TRUE), sum(PA_src, na.rm = TRUE)),
      BABIP = safe_divide(sum(H_src, na.rm = TRUE) - sum(HR_src, na.rm = TRUE), sum(AB_src, na.rm = TRUE) - sum(SO_src, na.rm = TRUE) - sum(HR_src, na.rm = TRUE) + sum(SF_src, na.rm = TRUE)),
      wOBA = weighted.mean(wOBA, w = PA_src, na.rm = TRUE),
      xwOBA = weighted.mean(xwOBA, w = PA_src, na.rm = TRUE),
      xwOBA_wOBA_gap = xwOBA - wOBA,
      PA = sum(PA_src, na.rm = TRUE),
      Age = first(Age),
      mlbamid = first(mlbamid),
      source_fallback = any(source_fallback),
      .groups = "drop"
    ) |>
    mutate(across(c(wOBA, xwOBA, xwOBA_wOBA_gap, Barrel_pct), ~ ifelse(is.nan(.x), NA_real_, .x)))
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
      r = coalesce_joined(r_mlb, er),
      hr = coalesce_joined(hr_mlb, hr),
      so = coalesce_joined(so_mlb, so),
      bb = coalesce_joined(bb_mlb, bb),
      ibb = coalesce_joined(ibb_mlb, ibb),
      hbp = coalesce_joined(hbp_mlb, hbp),
      ab = ab_mlb,
      sf = sf_mlb,
      xera = x_era,
      position = if_else(gs / g >= .5 | ip / g > 4, "SP", "RP"),
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

validate_diff_fields <- function(candidate_data) {
  diff_fields <- grep("_diff$", colnames(candidate_data), value = TRUE)
  mismatches <- lapply(diff_fields, function(diff_field) {
    base <- str_remove(diff_field, "_diff$")
    cur_field <- paste0(base, "_cur")
    l3_field <- paste0(base, "_l3")
    if (!cur_field %in% colnames(candidate_data) || !l3_field %in% colnames(candidate_data)) {
      return(NULL)
    }

    expected <- candidate_data[[cur_field]] - candidate_data[[l3_field]]
    actual <- candidate_data[[diff_field]]
    diff <- abs(actual - expected)
    bad <- which(!is.na(diff) & diff > 0.0015)
    if (length(bad) == 0) {
      return(NULL)
    }

    tibble::tibble(
      row_key = row_keys(candidate_data)[bad],
      field = diff_field,
      expected_value = expected[bad],
      actual_value = actual[bad],
      difference = actual[bad] - expected[bad]
    )
  })

  bind_rows(mismatches)
}

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
  diff_mismatches <- bind_rows(
    validate_diff_fields(candidate_hitters),
    validate_diff_fields(candidate_pitchers)
  )

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
    diff_mismatches = diff_mismatches,
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
  if (nrow(diff_mismatches) > 0) failures <- c(failures, "diff fields do not equal current minus baseline")

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

build_hitter_baselines <- function() {
  bind_rows(
    read_csv("fangraphs-leaderboards-2023.csv", show_col_types = FALSE) |> mutate(year = 2023),
    read_csv("fangraphs-leaderboards-2024.csv", show_col_types = FALSE) |> mutate(year = 2024),
    read_csv("fangraphs-leaderboards-2025.csv", show_col_types = FALSE) |> mutate(year = 2025)
  ) |>
    mutate(
      TB = `1B` + 2 * `2B` + 3 * `3B` + 4 * HR,
      Name = stri_trans_general(Name, id = "Latin-ASCII"),
      baseline_weight = case_when(
        year == current_year - 1 ~ 5,
        year == current_year - 2 ~ 3,
        year == current_year - 3 ~ 1,
        TRUE ~ 0
      )
    ) |>
    group_by(Name, PlayerId) |>
    summarize(
      AVG = sum(H * baseline_weight) / sum(AB * baseline_weight),
      OBP = (sum(H * baseline_weight) + sum(BB * baseline_weight) + sum(HBP * baseline_weight)) / (sum(AB * baseline_weight) + sum(BB * baseline_weight) + sum(HBP * baseline_weight) + sum(SF * baseline_weight)),
      SLG = sum(TB * baseline_weight) / sum(AB * baseline_weight),
      K_pct = 100 * sum(SO * baseline_weight) / sum(PA * baseline_weight),
      BB_pct = 100 * sum(BB * baseline_weight) / sum(PA * baseline_weight),
      Barrel_pct = 100 * sum(Barrels * baseline_weight) / sum(PA * baseline_weight),
      BABIP = (sum(H * baseline_weight) - sum(HR * baseline_weight)) / (sum(AB * baseline_weight) - sum(SO * baseline_weight) - sum(HR * baseline_weight) + sum(SF * baseline_weight)),
      wOBA = weighted.mean(wOBA, w = PA * baseline_weight, na.rm = TRUE),
      xwOBA = weighted.mean(xwOBA, w = PA * baseline_weight, na.rm = TRUE),
      xwOBA_wOBA_gap = xwOBA - wOBA,
      PA = sum(PA),
      .groups = "drop"
    )
}

build_pitcher_baselines <- function() {
  bind_rows(
    read_csv("pitcher-stats-2023.csv", show_col_types = FALSE) |> mutate(year = 2023),
    read_csv("pitcher-stats-2024.csv", show_col_types = FALSE) |> mutate(year = 2024),
    read_csv("pitcher-stats-2025.csv", show_col_types = FALSE) |> mutate(year = 2025)
  ) |>
    mutate(
      baseline_weight = case_when(
        year == current_year - 1 ~ 5,
        year == current_year - 2 ~ 3,
        year == current_year - 3 ~ 1,
        TRUE ~ 0
      )
    ) |>
    group_by(playerid, name, position) |>
    summarize(
      era = sum(er * baseline_weight) / sum(ip * baseline_weight) * 9,
      k_percent = sum(so * baseline_weight) / sum(tbf * baseline_weight) * 100,
      bb_percent = sum(bb * baseline_weight) / sum(tbf * baseline_weight) * 100,
      k_minus_bb_percent = k_percent - bb_percent,
      xera = weighted.mean(x_era, w = tbf * baseline_weight, na.rm = TRUE),
      era_xera_gap = era - xera,
      fip = weighted.mean(fip, w = tbf * baseline_weight, na.rm = TRUE),
      era_fip_gap = era - fip,
      barrel_percent = weighted.mean(barrel_percent, w = tbf * baseline_weight, na.rm = TRUE) * 100,
      o_swing_percent = weighted.mean(o_swing_percent, w = tbf * baseline_weight, na.rm = TRUE) * 100,
      babip = weighted.mean(babip, w = tbf * baseline_weight, na.rm = TRUE),
      lob_percent = weighted.mean(lob_percent, w = tbf * baseline_weight, na.rm = TRUE) * 100,
      csw_percent = weighted.mean(c_sw_str_percent, w = tbf * baseline_weight, na.rm = TRUE) * 100,
      hr_fb = weighted.mean(hr_fb, w = tbf * baseline_weight, na.rm = TRUE),
      .groups = "drop"
    )
}

hitter_metric_cols <- c("AVG", "OBP", "SLG", "K_pct", "BB_pct", "Barrel_pct", "BABIP", "wOBA", "xwOBA", "xwOBA_wOBA_gap")
pitcher_metric_cols <- c("era", "k_percent", "bb_percent", "k_minus_bb_percent", "xera", "o_swing_percent", "csw_percent", "barrel_percent", "lob_percent", "babip")

build_hitter_league_baseline <- function() {
  bind_rows(
    read_csv("fangraphs-leaderboards-2023.csv", show_col_types = FALSE) |> mutate(year = 2023),
    read_csv("fangraphs-leaderboards-2024.csv", show_col_types = FALSE) |> mutate(year = 2024),
    read_csv("fangraphs-leaderboards-2025.csv", show_col_types = FALSE) |> mutate(year = 2025)
  ) |>
    mutate(
      TB = `1B` + 2 * `2B` + 3 * `3B` + 4 * HR,
      baseline_weight = case_when(
        year == current_year - 1 ~ 5,
        year == current_year - 2 ~ 3,
        year == current_year - 3 ~ 1,
        TRUE ~ 0
      )
    ) |>
    summarize(
      AVG = sum(H * baseline_weight, na.rm = TRUE) / sum(AB * baseline_weight, na.rm = TRUE),
      OBP = (sum(H * baseline_weight, na.rm = TRUE) + sum(BB * baseline_weight, na.rm = TRUE) + sum(HBP * baseline_weight, na.rm = TRUE)) / (sum(AB * baseline_weight, na.rm = TRUE) + sum(BB * baseline_weight, na.rm = TRUE) + sum(HBP * baseline_weight, na.rm = TRUE) + sum(SF * baseline_weight, na.rm = TRUE)),
      SLG = sum(TB * baseline_weight, na.rm = TRUE) / sum(AB * baseline_weight, na.rm = TRUE),
      K_pct = 100 * sum(SO * baseline_weight, na.rm = TRUE) / sum(PA * baseline_weight, na.rm = TRUE),
      BB_pct = 100 * sum(BB * baseline_weight, na.rm = TRUE) / sum(PA * baseline_weight, na.rm = TRUE),
      Barrel_pct = 100 * sum(Barrels * baseline_weight, na.rm = TRUE) / sum(PA * baseline_weight, na.rm = TRUE),
      BABIP = (sum(H * baseline_weight, na.rm = TRUE) - sum(HR * baseline_weight, na.rm = TRUE)) / (sum(AB * baseline_weight, na.rm = TRUE) - sum(SO * baseline_weight, na.rm = TRUE) - sum(HR * baseline_weight, na.rm = TRUE) + sum(SF * baseline_weight, na.rm = TRUE)),
      wOBA = weighted.mean(wOBA, w = PA * baseline_weight, na.rm = TRUE),
      xwOBA = weighted.mean(xwOBA, w = PA * baseline_weight, na.rm = TRUE),
      xwOBA_wOBA_gap = xwOBA - wOBA
    )
}

build_hitter_league_current <- function(fg_contract, mlb_basics) {
  fg_contract |>
    left_join(mlb_basics, by = "mlbamid") |>
    mutate(
      AB_src = coalesce_joined(AB_mlb, AB),
      PA_src = coalesce_joined(PA_mlb, PA),
      H_src = coalesce_joined(H_mlb, H),
      HR_src = coalesce_joined(HR_mlb, HR),
      BB_src = coalesce_joined(BB_mlb, BB),
      HBP_src = coalesce_joined(HBP_mlb, HBP),
      SF_src = coalesce_joined(SF_mlb, SF),
      SO_src = coalesce_joined(SO_mlb, SO),
      TB_src = coalesce_joined(TB_mlb, TB)
    ) |>
    summarize(
      AVG = safe_divide(sum(H_src, na.rm = TRUE), sum(AB_src, na.rm = TRUE)),
      OBP = safe_divide(sum(H_src, na.rm = TRUE) + sum(BB_src, na.rm = TRUE) + sum(HBP_src, na.rm = TRUE), sum(AB_src, na.rm = TRUE) + sum(BB_src, na.rm = TRUE) + sum(HBP_src, na.rm = TRUE) + sum(SF_src, na.rm = TRUE)),
      SLG = safe_divide(sum(TB_src, na.rm = TRUE), sum(AB_src, na.rm = TRUE)),
      K_pct = 100 * safe_divide(sum(SO_src, na.rm = TRUE), sum(PA_src, na.rm = TRUE)),
      BB_pct = 100 * safe_divide(sum(BB_src, na.rm = TRUE), sum(PA_src, na.rm = TRUE)),
      Barrel_pct = 100 * safe_divide(sum(Barrels, na.rm = TRUE), sum(PA_src, na.rm = TRUE)),
      BABIP = safe_divide(sum(H_src, na.rm = TRUE) - sum(HR_src, na.rm = TRUE), sum(AB_src, na.rm = TRUE) - sum(SO_src, na.rm = TRUE) - sum(HR_src, na.rm = TRUE) + sum(SF_src, na.rm = TRUE)),
      wOBA = weighted.mean(wOBA, w = PA_src, na.rm = TRUE),
      xwOBA = weighted.mean(xwOBA, w = PA_src, na.rm = TRUE),
      xwOBA_wOBA_gap = xwOBA - wOBA
    )
}

build_pitcher_league_baseline <- function() {
  bind_rows(
    read_csv("pitcher-stats-2023.csv", show_col_types = FALSE) |> mutate(year = 2023),
    read_csv("pitcher-stats-2024.csv", show_col_types = FALSE) |> mutate(year = 2024),
    read_csv("pitcher-stats-2025.csv", show_col_types = FALSE) |> mutate(year = 2025)
  ) |>
    mutate(
      baseline_weight = case_when(
        year == current_year - 1 ~ 5,
        year == current_year - 2 ~ 3,
        year == current_year - 3 ~ 1,
        TRUE ~ 0
      )
    ) |>
    summarize(
      era = sum(er * baseline_weight, na.rm = TRUE) / sum(ip * baseline_weight, na.rm = TRUE) * 9,
      k_percent = sum(so * baseline_weight, na.rm = TRUE) / sum(tbf * baseline_weight, na.rm = TRUE) * 100,
      bb_percent = sum(bb * baseline_weight, na.rm = TRUE) / sum(tbf * baseline_weight, na.rm = TRUE) * 100,
      k_minus_bb_percent = k_percent - bb_percent,
      xera = weighted.mean(x_era, w = tbf * baseline_weight, na.rm = TRUE),
      barrel_percent = weighted.mean(barrel_percent, w = tbf * baseline_weight, na.rm = TRUE) * 100,
      o_swing_percent = weighted.mean(o_swing_percent, w = tbf * baseline_weight, na.rm = TRUE) * 100,
      babip = weighted.mean(babip, w = tbf * baseline_weight, na.rm = TRUE),
      lob_percent = weighted.mean(lob_percent, w = tbf * baseline_weight, na.rm = TRUE) * 100,
      csw_percent = weighted.mean(c_sw_str_percent, w = tbf * baseline_weight, na.rm = TRUE) * 100
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
# HITTERS DATA PROCESSING
# =============================================================================

hitters_last_3 <- read_or_build_baseline(hitter_baseline_cache_file, build_hitter_baselines, "hitter")
hitters_last_3 <- hitters_last_3 |> mutate(PlayerId = as.character(PlayerId))
hitter_league_baseline <- build_hitter_league_baseline()

fangraphs_hitters <- fetch_fangraphs_hitter_current_contract(current_year)
mlb_hitter_basics <- fetch_mlb_hitter_basics(current_year)
hitters_this_year <- combine_hitter_current_stats(fangraphs_hitters, mlb_hitter_basics)
hitter_league_current <- build_hitter_league_current(fangraphs_hitters, mlb_hitter_basics)
hitter_league_diffs <- build_league_diffs(hitter_league_current, hitter_league_baseline, hitter_metric_cols)

# Combine hitter data and compute differences
full_stats_hitters <-
  hitters_this_year |>
  left_join(hitters_last_3, by = c("Name", "PlayerId"), suffix = c("_cur", "_l3")) |>
  mutate(
    AVG_diff = AVG_cur - AVG_l3,
    OBP_diff = OBP_cur - OBP_l3,
    SLG_diff = SLG_cur - SLG_l3,
    K_pct_diff = K_pct_cur - K_pct_l3,
    BB_pct_diff = BB_pct_cur - BB_pct_l3,
    Barrel_pct_diff = Barrel_pct_cur - Barrel_pct_l3,
    BABIP_diff = BABIP_cur - BABIP_l3,
    wOBA_diff = wOBA_cur - wOBA_l3,
    xwOBA_diff = xwOBA_cur - xwOBA_l3,
    xwOBA_wOBA_gap_diff = xwOBA_wOBA_gap_cur - xwOBA_wOBA_gap_l3,
    player_type = "hitter"
  ) |>
  add_league_adjusted_diffs(hitter_metric_cols, hitter_league_diffs) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# =============================================================================
# PITCHERS DATA PROCESSING  
# =============================================================================

# Load pitcher data for weighted baselines from the past 3 years
pitching_stats_last_3 <- read_or_build_baseline(pitcher_baseline_cache_file, build_pitcher_baselines, "pitcher")
pitching_stats_last_3 <- pitching_stats_last_3 |>
  mutate(playerid = as.character(playerid)) |>
  select(-any_of("ld_percent"))
pitcher_league_baseline <- build_pitcher_league_baseline()

fangraphs_pitchers <- fetch_fangraphs_pitcher_current_contract(current_year)
mlb_pitcher_basics <- fetch_mlb_pitcher_basics(current_year)
pitching_stats_current <- combine_pitcher_current_stats(fangraphs_pitchers, mlb_pitcher_basics)
pitcher_league_current <- build_pitcher_league_current(pitching_stats_current)
pitcher_league_diffs <- build_league_diffs(pitcher_league_current, pitcher_league_baseline, pitcher_metric_cols)

# Combine pitcher data and compute differences
full_stats_pitchers <-
  pitching_stats_current |>
  left_join(pitching_stats_last_3, by = c("playerid", "name", "position"), suffix = c("_cur", "_l3")) |>
  mutate(
    era_diff = era_cur - era_l3,
    k_percent_diff = k_percent_cur - k_percent_l3,
    bb_percent_diff = bb_percent_cur - bb_percent_l3,
    k_minus_bb_percent_diff = k_minus_bb_percent_cur - k_minus_bb_percent_l3,
    xera_diff = xera_cur - xera_l3,
    o_swing_percent_diff = o_swing_percent_cur - o_swing_percent_l3,
    csw_percent_diff = csw_percent_cur - csw_percent_l3,
    barrel_percent_diff = barrel_percent_cur - barrel_percent_l3,
    lob_percent_diff = lob_percent_cur - lob_percent_l3,
    babip_diff = babip_cur - babip_l3,
    player_type = "pitcher"
  ) |>
  add_league_adjusted_diffs(pitcher_metric_cols, pitcher_league_diffs) |>
  select(-matches("_mlb$"), -any_of(c("team", "primary_position", "r", "ab", "sf", "lob_denominator"))) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  rename(Name = name, PlayerId = playerid, Age = age)

# =============================================================================
# CREATE CANDIDATE LOOKUP AND VALIDATE BEFORE PROMOTION
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
