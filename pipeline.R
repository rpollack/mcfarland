# Transformations, baselines, frozen player ids and the gates that guard the published CSVs.
#
# Sourced by refresh_data.R and by the R tests; defining things here has no side effects. Needs data_sources.R
# loaded first (for safe_divide, require_columns and the season tables). Run from the repository root.

# =============================================================================
# SHARED AGGREGATION
# =============================================================================
# The same hitter and pitcher metrics are needed three ways: per player over the baseline seasons, league-wide
# over the baseline seasons, and (hitters) per player and league-wide for the current season. Each way feeds the
# same summary function so the formulas cannot drift apart. Input rows carry a weight column `w`: the season
# weight for baselines, 1 for the current season.

# The metrics compared against each player's own baseline. Each gets <metric>_diff (current minus baseline) and
# <metric>_lg_adj_diff (that difference minus how far the whole league moved).
hitter_metric_cols <- c("AVG", "OBP", "SLG", "K_pct", "BB_pct", "Barrel_pct", "BABIP", "wOBA", "xwOBA", "xwOBA_wOBA_gap")
pitcher_metric_cols <- c("era", "k_percent", "bb_percent", "k_minus_bb_percent", "xera", "o_swing_percent", "whiff_percent", "barrel_percent", "lob_percent", "babip")

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

# The baseline files already hold percents (0-100), so nothing is rescaled here.
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
    barrel_percent = weighted.mean(barrel_percent, w = tbf * w, na.rm = TRUE),
    o_swing_percent = weighted.mean(o_swing_percent, w = tbf * w, na.rm = TRUE),
    babip = weighted.mean(babip, w = tbf * w, na.rm = TRUE),
    lob_percent = weighted.mean(lob_percent, w = tbf * w, na.rm = TRUE),
    whiff_percent = weighted.mean(whiff_percent, w = tbf * w, na.rm = TRUE),
    .groups = "drop"
  )
}

# =============================================================================
# BASELINES (weighted history)
# =============================================================================
# The baseline for a player is the weighted history of the three seasons before the current one (weights 1, 3, 5,
# oldest first). Each season is a baseline-hitters-<year>.csv / baseline-pitchers-<year>.csv written by
# Rscript refresh_data.R build-baseline-season <year>, keyed by mlbamid.

baseline_seasons_for <- function(current_year) {
  data.frame(year = current_year - 3:1, weight = c(1, 3, 5))
}

# Every season file must carry the columns the summaries use, so a file in another shape fails here instead of
# binding in as all-NA rows and quietly dropping that season.
hitter_baseline_columns <- c("mlbamid", "Name", "AB", "PA", "H", "2B", "3B", "HR", "BB", "HBP", "SF", "SO", "TB", "Barrels", "wOBA", "xwOBA")
pitcher_baseline_columns <- c("mlbamid", "name", "position", "er", "ip", "so", "bb", "tbf", "x_era", "fip", "barrel_percent", "o_swing_percent", "babip", "lob_percent", "whiff_percent")

read_baseline_seasons <- function(kind, baseline_seasons, required_columns, dir = ".") {
  seasons <- lapply(seq_len(nrow(baseline_seasons)), function(i) {
    file <- baseline_file(kind, baseline_seasons$year[i], dir)
    if (!file.exists(file)) {
      stop("Baseline season file ", file, " is missing; run Rscript refresh_data.R build-baseline-season ", baseline_seasons$year[i], " first.", call. = FALSE)
    }
    read_csv(file, show_col_types = FALSE, col_types = cols(mlbamid = col_character())) |>
      require_columns(required_columns, file) |>
      mutate(year = baseline_seasons$year[i], w = baseline_seasons$weight[i])
  })
  bind_rows(seasons)
}

read_hitter_baseline_seasons <- function(baseline_seasons, dir = ".") {
  read_baseline_seasons("hitters", baseline_seasons, hitter_baseline_columns, dir)
}

read_pitcher_baseline_seasons <- function(baseline_seasons, dir = ".") {
  read_baseline_seasons("pitchers", baseline_seasons, pitcher_baseline_columns, dir)
}

# =============================================================================
# CURRENT SEASON
# =============================================================================

# One row per hitter (the season table already has one), with the same metrics as the baseline summaries.
combine_hitter_current_stats <- function(hitters) {
  hitters |>
    mutate(w = 1) |>
    group_by(mlbamid, Name) |>
    summarize_hitter_metrics(Age = first(Age))
}

combine_pitcher_current_stats <- function(pitchers, current_year) {
  pitchers |>
    mutate(
      season = current_year,
      xera = x_era,
      k_percent = 100 * safe_divide(so, tbf),
      bb_percent = 100 * safe_divide(bb, tbf),
      k_minus_bb_percent = k_percent - bb_percent,
      era = 9 * safe_divide(er, ip),
      xera_gap = era - x_era,
      fip_era_gap = era - fip
    ) |>
    select(-r, -ab, -sf)
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
      whiff_percent = weighted.mean(whiff_percent, w = tbf, na.rm = TRUE),
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
#
# accept_source_migration is for the one run that switches the data source (FanGraphs to MLB + Savant): every value
# and the player list change, so the value-parity and player-set checks are skipped and the structural checks stay.
# Pass it on purpose, on the command line (--accept-source-migration); it is logged and recorded in the report.
# `extra_failures` carries failures from other checks (see check_output_sanity).
validate_migration_parity <- function(locked_hitters, locked_pitchers, locked_lookup, candidate_hitters, candidate_pitchers, candidate_lookup, locked_data_through_date, candidate_data_through_date, accept_source_migration = FALSE, extra_failures = character()) {
  locked_all <- bind_rows(locked_hitters, locked_pitchers)
  candidate_all <- bind_rows(candidate_hitters, candidate_pitchers)
  locked_keys <- row_keys(locked_all)
  candidate_keys <- row_keys(candidate_all)
  same_cutoff <- !is.na(locked_data_through_date) && locked_data_through_date == candidate_data_through_date
  # Whether values and the player list must match production exactly.
  check_values <- same_cutoff && !accept_source_migration

  missing_players <- setdiff(locked_keys, candidate_keys)
  added_players <- setdiff(candidate_keys, locked_keys)
  missing_mlbamids <- candidate_all |> filter(is.na(mlbamid) | mlbamid == "") |> select(PlayerId, player_type, Name)
  duplicate_compound_ids <- candidate_lookup |> count(compound_id) |> filter(n > 1)

  locked_compounds <- locked_lookup$compound_id
  candidate_compounds <- candidate_lookup$compound_id
  missing_compounds <- setdiff(locked_compounds, candidate_compounds)
  added_compounds <- setdiff(candidate_compounds, locked_compounds)

  stat_mismatches <- if (check_values) {
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
    source_migration = accept_source_migration,
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
    stat_mismatches = stat_mismatches,
    warnings = list()
  )

  failures <- c()
  if (check_values && nrow(locked_hitters) != nrow(candidate_hitters)) failures <- c(failures, "hitter row count changed")
  if (check_values && nrow(locked_pitchers) != nrow(candidate_pitchers)) failures <- c(failures, "pitcher row count changed")
  if (check_values && length(missing_players) > 0) failures <- c(failures, "locked players missing from candidates")
  if (check_values && length(added_players) > 0) failures <- c(failures, "candidate contains non-locked players")
  if (check_values && (length(missing_compounds) > 0 || length(added_compounds) > 0)) failures <- c(failures, "lookup compound IDs changed")
  if (nrow(duplicate_compound_ids) > 0) failures <- c(failures, "duplicate compound IDs")
  if (nrow(missing_mlbamids) > 0) failures <- c(failures, "served players missing mlbamid")
  if (check_values && nrow(stat_mismatches) > 0) failures <- c(failures, "stat parity mismatches")
  failures <- c(failures, extra_failures)

  if (accept_source_migration) {
    report$warnings <- list(paste0(
      "Source migration accepted: value parity and the player-set check were skipped. ",
      length(missing_players), " served rows left the lists and ", length(added_players), " were added."
    ))
  }

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
# OUTPUT CONTRACT
# =============================================================================
# The columns of the published CSVs, in order. The server reads them by name, so a rename or a drop here is a
# change to the app too. Pitchers: the FanGraphs-only columns (x_fip, hr_fb, f_bv, c_tv, s_fv, ev90, barrels) are gone
# and csw_percent_* became whiff_percent_*.

hitter_output_columns <- c(
  "Name", "PlayerId", "AVG_cur", "OBP_cur", "SLG_cur", "K_pct_cur", "BB_pct_cur", "Barrel_pct_cur", "BABIP_cur",
  "wOBA_cur", "xwOBA_cur", "xwOBA_wOBA_gap_cur", "PA_cur", "Age", "mlbamid", "AVG_l3", "OBP_l3", "SLG_l3",
  "K_pct_l3", "BB_pct_l3", "Barrel_pct_l3", "BABIP_l3", "wOBA_l3", "xwOBA_l3", "xwOBA_wOBA_gap_l3", "PA_l3",
  "AVG_diff", "OBP_diff", "SLG_diff", "K_pct_diff", "BB_pct_diff", "Barrel_pct_diff", "BABIP_diff", "wOBA_diff",
  "xwOBA_diff", "xwOBA_wOBA_gap_diff", "player_type", "AVG_lg_adj_diff", "OBP_lg_adj_diff", "SLG_lg_adj_diff",
  "K_pct_lg_adj_diff", "BB_pct_lg_adj_diff", "Barrel_pct_lg_adj_diff", "BABIP_lg_adj_diff", "wOBA_lg_adj_diff",
  "xwOBA_lg_adj_diff", "xwOBA_wOBA_gap_lg_adj_diff"
)

pitcher_output_columns <- c(
  "PlayerId", "mlbamid", "Name", "season", "Age", "g", "gs", "ip", "tbf", "h", "er", "hr", "so", "bb", "ibb", "hbp",
  "babip_cur", "lob_percent_cur", "fip_cur", "o_swing_percent_cur", "whiff_percent_cur", "x_era", "position",
  "barrel_percent_cur", "xera_cur", "k_percent_cur", "bb_percent_cur", "k_minus_bb_percent_cur", "era_cur",
  "xera_gap", "fip_era_gap", "era_l3", "k_percent_l3", "bb_percent_l3", "k_minus_bb_percent_l3", "xera_l3",
  "era_xera_gap", "fip_l3", "era_fip_gap", "barrel_percent_l3", "o_swing_percent_l3", "babip_l3", "lob_percent_l3",
  "whiff_percent_l3", "era_diff", "k_percent_diff", "bb_percent_diff", "k_minus_bb_percent_diff", "xera_diff",
  "o_swing_percent_diff", "whiff_percent_diff", "barrel_percent_diff", "lob_percent_diff", "babip_diff",
  "player_type", "era_lg_adj_diff", "k_percent_lg_adj_diff", "bb_percent_lg_adj_diff",
  "k_minus_bb_percent_lg_adj_diff", "xera_lg_adj_diff", "o_swing_percent_lg_adj_diff", "whiff_percent_lg_adj_diff",
  "barrel_percent_lg_adj_diff", "lob_percent_lg_adj_diff", "babip_lg_adj_diff"
)

# =============================================================================
# FROZEN PLAYER IDS
# =============================================================================
# PlayerId is the app's key for a player (URLs, votes, trend rows, cached analyses, stars). The first ids were
# FanGraphs ids, which this pipeline no longer has, so they are frozen in player_id_map.csv (mlbamid, PlayerId), built
# once from the committed player_lookup.csv and never regenerated: losing or rebuilding it would change every
# existing player's id. A player in the map keeps the PlayerId; anyone else gets PlayerId = mlbamid. So the column
# holds two eras of ids forever: 5-digit legacy ones and 6-digit MLB ones. The two ranges cannot overlap (legacy ids
# top out at 37,238; MLB ids start at 434,378), and check_player_id_collisions enforces that.

read_player_id_map <- function(file = "player_id_map.csv") {
  id_map <- read_csv(file, show_col_types = FALSE, col_types = cols(.default = col_character())) |>
    require_columns(c("mlbamid", "PlayerId"), file)
  if (anyDuplicated(id_map$mlbamid) > 0 || anyDuplicated(id_map$PlayerId) > 0) {
    stop(file, " maps a player or an id more than once", call. = FALSE)
  }
  id_map
}

add_player_ids <- function(data, id_map) {
  data |>
    mutate(mlbamid = as.character(mlbamid)) |>
    left_join(id_map, by = "mlbamid") |>
    mutate(PlayerId = if_else(is.na(PlayerId), mlbamid, PlayerId))
}

# A new player's PlayerId is the mlbamid; that must never equal a mapped legacy id, or two players would share a key.
check_player_id_collisions <- function(data, id_map) {
  unmapped <- data$mlbamid[!data$mlbamid %in% id_map$mlbamid]
  clashes <- intersect(unmapped, id_map$PlayerId)
  if (length(clashes) > 0) {
    stop("New players have an mlbamid equal to a frozen legacy PlayerId: ", paste(clashes, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

# =============================================================================
# SANITY CHECKS
# =============================================================================
# Cheap checks on the candidate output that name the source when a feed has quietly gone empty. Returns failure
# messages (character(0) when fine); validate_migration_parity turns them into a failed run, which leaves the
# published CSVs alone.

required_output_sources <- list(
  hitters = c(
    Age = "MLB stats=season", PA_cur = "MLB stats=season", wOBA_cur = "MLB stats=sabermetrics", xwOBA_cur = "Savant expected_statistics",
    Barrel_pct_cur = "Savant statcast", AVG_cur = "MLB stats=season"
  ),
  pitchers = c(
    Age = "MLB stats=season", tbf = "MLB stats=season", era_cur = "MLB stats=season", fip_cur = "MLB stats=season (computed)",
    xera_cur = "Savant expected_statistics", barrel_percent_cur = "Savant custom", o_swing_percent_cur = "Savant custom",
    whiff_percent_cur = "Savant custom", babip_cur = "MLB stats=season (computed)", lob_percent_cur = "MLB stats=season (computed)"
  )
)

check_output_sanity <- function(hitters, pitchers, locked_hitters, locked_pitchers, locked_data_through_date, current_year, accept_source_migration = FALSE) {
  failures <- character()
  groups <- list(hitters = hitters, pitchers = pitchers)
  locked <- list(hitters = locked_hitters, pitchers = locked_pitchers)

  for (group in names(groups)) {
    if (nrow(groups[[group]]) == 0) {
      failures <- c(failures, paste0("no ", group, " in the new output"))
      next
    }
    for (column in names(required_output_sources[[group]])) {
      if (!column %in% colnames(groups[[group]]) || all(is.na(groups[[group]][[column]]))) {
        failures <- c(failures, paste0(column, " is empty for every ", sub("s$", "", group), " (source: ", required_output_sources[[group]][[column]], ")"))
      }
    }
  }

  # Within one season the player lists only grow, so a smaller list means a feed returned too little. A new season
  # starts small, so the comparison only applies when the previous output is from the same season. The one
  # source-migration run changes the lists on purpose and gets a wider band.
  same_season <- !is.na(locked_data_through_date) && substr(locked_data_through_date, 1, 4) == as.character(current_year)
  if (same_season) {
    minimum_ratio <- if (accept_source_migration) 0.85 else 0.98
    for (group in names(groups)) {
      if (nrow(groups[[group]]) > 0 && nrow(locked[[group]]) > 0 && nrow(groups[[group]]) < minimum_ratio * nrow(locked[[group]])) {
        failures <- c(failures, paste0(
          "the ", sub("s$", "", group), " list shrank from ", nrow(locked[[group]]), " to ", nrow(groups[[group]]),
          " rows within the ", current_year, " season (a source returned too little)"
        ))
      }
    }
  }
  failures
}
