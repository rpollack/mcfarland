library(dplyr)
library(readr)
library(jsonlite)

# Sources of the data (MLB Stats API, Baseball Savant) and the season tables, then the transformations and the gates
# on the published CSVs. Both are shared with the R tests.
source("data_sources.R")
source("pipeline.R")

# =============================================================================
# CONFIGURATION
# =============================================================================
# Yearly rollover, in November: build the baseline files for the season that just finished,
#   Rscript refresh_data.R build-baseline-season <that year>
# and in March, once the new season has games (before that the stats APIs return no rows and this script stops), bump
# current_year. The three seasons before current_year are the baseline, oldest first; the weights make last season
# count five times as much as the oldest one.
#
# One-off runs:
#   Rscript refresh_data.R build-baseline-season <year> [--force]   write the two baseline files for a finished season
#   Rscript refresh_data.R --accept-source-migration                the single run that switched the data source
#                                                                   (skips value parity; see validate_migration_parity)
current_year <- 2026
baseline_seasons <- baseline_seasons_for(current_year)

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
# ENTRY POINT
# =============================================================================

script_args <- commandArgs(trailingOnly = TRUE)
# Rscript refresh_data.R build-baseline-season <year> [--force]
# Writes baseline-hitters-<year>.csv and baseline-pitchers-<year>.csv from the MLB Stats API and Baseball Savant.
# Refuses to overwrite existing files without --force.
if (length(script_args) >= 2 && script_args[1] == "build-baseline-season") {
  build_baseline_season(as.integer(script_args[2]), force = "--force" %in% script_args)
  quit(save = "no")
}
accept_source_migration <- "--accept-source-migration" %in% script_args
if (accept_source_migration) {
  cat("*** --accept-source-migration: value parity and the player-set check are skipped for this run. ***\n")
}

# =============================================================================
# HITTERS
# =============================================================================

player_id_map <- read_player_id_map()

hitter_baseline_history <- read_hitter_baseline_seasons(baseline_seasons)
hitters_last_3 <- hitter_baseline_history |>
  group_by(mlbamid) |>
  summarize_hitter_metrics(na.rm = FALSE)
hitter_league_baseline <- summarize_hitter_metrics(hitter_baseline_history)

# The season in progress: fewer rows than a full season is normal, so only an empty response is refused here;
# check_output_sanity below catches a feed that went thin or empty.
# (The refresh_http_get option lets the tests run this script against fixtures instead of the network.)
season_stats <- build_season_stats(current_year, min_rows = any_season_min_rows, http_get = getOption("refresh_http_get", default_http_get))

hitters_this_year <- combine_hitter_current_stats(season_stats$hitters)
hitter_league_current <- summarize_hitter_metrics(mutate(season_stats$hitters, w = 1))
hitter_league_diffs <- build_league_diffs(hitter_league_current, hitter_league_baseline, hitter_metric_cols)

full_stats_hitters <-
  hitters_this_year |>
  left_join(hitters_last_3, by = "mlbamid", suffix = c("_cur", "_l3")) |>
  add_baseline_diffs(hitter_metric_cols) |>
  mutate(player_type = "hitter") |>
  add_league_adjusted_diffs(hitter_metric_cols, hitter_league_diffs) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  add_player_ids(player_id_map) |>
  select(all_of(hitter_output_columns))

# =============================================================================
# PITCHERS
# =============================================================================

pitcher_baseline_history <- read_pitcher_baseline_seasons(baseline_seasons)
pitching_stats_last_3 <- pitcher_baseline_history |>
  group_by(mlbamid, position) |>
  summarize_pitcher_baseline_metrics(na.rm = FALSE)
pitcher_league_baseline <- summarize_pitcher_baseline_metrics(pitcher_baseline_history)

pitching_stats_current <- combine_pitcher_current_stats(season_stats$pitchers, current_year)
pitcher_league_current <- build_pitcher_league_current(pitching_stats_current)
pitcher_league_diffs <- build_league_diffs(pitcher_league_current, pitcher_league_baseline, pitcher_metric_cols)

# Baselines are role-specific (SP vs RP), so match on mlbamid and position. If a player ever has two baseline rows
# for one role, keep the first so the join cannot duplicate current-season rows.
pitching_stats_last_3_by_role <- pitching_stats_last_3 |>
  distinct(mlbamid, position, .keep_all = TRUE)

full_stats_pitchers <-
  pitching_stats_current |>
  left_join(pitching_stats_last_3_by_role, by = c("mlbamid", "position"), suffix = c("_cur", "_l3")) |>
  add_baseline_diffs(pitcher_metric_cols) |>
  mutate(player_type = "pitcher") |>
  add_league_adjusted_diffs(pitcher_metric_cols, pitcher_league_diffs) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  rename(Name = name, Age = age) |>
  add_player_ids(player_id_map) |>
  select(all_of(pitcher_output_columns))

check_player_id_collisions(bind_rows(full_stats_hitters, full_stats_pitchers), player_id_map)

# =============================================================================
# VALIDATE CANDIDATES, THEN PROMOTE
# =============================================================================

candidate_full_stats_hitters <- full_stats_hitters
candidate_full_stats_pitchers <- full_stats_pitchers
candidate_player_lookup <- build_player_lookup(candidate_full_stats_hitters, candidate_full_stats_pitchers)

locked_full_stats_hitters <- read_csv("full_stats_hitters.csv", show_col_types = FALSE) |>
  mutate(PlayerId = as.character(PlayerId), mlbamid = as.character(mlbamid), player_type = "hitter")
locked_full_stats_pitchers <- read_csv("full_stats_pitchers.csv", show_col_types = FALSE) |>
  mutate(PlayerId = as.character(PlayerId), mlbamid = as.character(mlbamid), player_type = "pitcher")
locked_player_lookup <- read_csv("player_lookup.csv", show_col_types = FALSE) |>
  mutate(PlayerId = as.character(PlayerId), mlbamid = as.character(mlbamid))

served_player_inventory <- build_served_player_inventory(locked_full_stats_hitters, locked_full_stats_pitchers, locked_player_lookup)

locked_data_through_date <- read_data_through_date()
data_through_date <- current_data_through_date()

sanity_failures <- check_output_sanity(
  full_stats_hitters, full_stats_pitchers, locked_full_stats_hitters, locked_full_stats_pitchers,
  locked_data_through_date, current_year, accept_source_migration
)

parity_report <- validate_migration_parity(
  locked_full_stats_hitters,
  locked_full_stats_pitchers,
  locked_player_lookup,
  full_stats_hitters,
  full_stats_pitchers,
  candidate_player_lookup,
  locked_data_through_date,
  data_through_date,
  accept_source_migration = accept_source_migration,
  extra_failures = sanity_failures
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

# A refused promotion must fail the run: otherwise the workflow looks green while the published data stands still.
if (!promoted) {
  quit(save = "no", status = 1)
}
