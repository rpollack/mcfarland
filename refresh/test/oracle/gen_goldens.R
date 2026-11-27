# Oracle for the end-to-end test: runs the real R refresh (refresh/refresh_data.R) on fixtures and records exactly what it read
# and wrote. Run from the repository root:  Rscript refresh/test/oracle/gen_goldens.R
#
# For each scenario it writes refresh/test/golden/<scenario>/inputs/data   (what the refresh found: id map, baselines, "production" files)
#                          and refresh/test/golden/<scenario>/expected/data (everything it left behind, scratch files included)
#                          and refresh/test/golden/<scenario>/exit_status   (the exit status)
# The clock is pinned to 2026-11-26 15:00:00 UTC, so data_through_date is 2026-11-25 and refreshed_at is fixed.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})
repo_root <- normalizePath(".")
golden_root <- file.path(repo_root, "refresh", "test", "golden")
fixtures_dir <- function(name) file.path(repo_root, "refresh", "test", name)

source(file.path(repo_root, "refresh", "data_sources.R"))
source(file.path(repo_root, "refresh", "pipeline.R"))
source(file.path(repo_root, "tests", "testthat", "fixture_http.R"))

scenarios <- list(
  # The ordinary day: the data date moved, everything promoted.
  basic = list(current = "fixtures", baselines = c("fixtures", "fixtures", "fixtures"), locked_date = "2026-11-16"),
  # Hazard fixtures: two-way player, call-up with no baseline, missing Savant rows, zero denominators, role flips, ties, tiny values.
  edge = list(current = "fixtures-edge", baselines = c("fixtures", "fixtures-edge", "fixtures"), locked_date = "2026-11-16"),
  # Same data date as production and a changed player list and stat: refused, production untouched.
  refused = list(current = "fixtures", baselines = c("fixtures", "fixtures", "fixtures"), locked_date = "2026-11-25", locked_judge_pa = 600),
  # A feed that comes back with no whiff% values: sanity failure, production untouched.
  blank_whiff = list(current = "fixtures", baselines = c("fixtures", "fixtures", "fixtures"), locked_date = "2026-11-16", blank_whiff = TRUE)
)

copy_dir <- function(from, to) {
  unlink(to, recursive = TRUE)
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  file.copy(from, dirname(to), recursive = TRUE)
  file.rename(file.path(dirname(to), basename(from)), to)
}

run_scenario <- function(name, spec) {
  dir <- tempfile(paste0("golden-", name, "-"))
  data_dir <- file.path(dir, "data")
  baseline_dir <- file.path(data_dir, "baselines")
  dir.create(baseline_dir, recursive = TRUE)
  dir.create(file.path(dir, "refresh"))
  file.copy(file.path(repo_root, "refresh", c("data_sources.R", "pipeline.R", "refresh_data.R")), file.path(dir, "refresh"))
  file.copy(file.path(repo_root, "tests", "testthat", "fixture_http.R"), dir)
  for (fixtures in unique(c(spec$current, spec$baselines))) file.copy(fixtures_dir(fixtures), dir, recursive = TRUE)

  writeLines(c("mlbamid,PlayerId", "592450,15640", "657277,17995", "111111,11111", "54321,2222"), file.path(data_dir, "player_id_map.csv"))
  # Baseline seasons 2023-2025, each built by R from a fixture set (the same code path as `build-baseline-season`).
  years <- baseline_seasons_for(2026)$year
  for (i in seq_along(years)) {
    Sys.setenv(FIXTURE_DIR = file.path(dir, spec$baselines[i]))
    write_baseline_season(years[i], build_season_stats(2025, any_season_min_rows, fixture_http()), baseline_dir, force = TRUE)
  }
  Sys.unsetenv("FIXTURE_DIR")

  judge_pa <- if (is.null(spec$locked_judge_pa)) 679 else spec$locked_judge_pa
  writeLines(c("Name,PlayerId,PA_cur,Age,mlbamid", "Old Guy,11111,200,35,111111", paste0("Aaron Judge,15640,", judge_pa, ",33,592450")), file.path(data_dir, "full_stats_hitters.csv"))
  writeLines(c("PlayerId,mlbamid,Name,Age,tbf,position", "17995,657277,Logan Webb,28,856,SP"), file.path(data_dir, "full_stats_pitchers.csv"))
  writeLines(c(
    "Name,PlayerId,Age,player_type,mlbamid,compound_id,display_name,position_detail,position",
    "Old Guy,11111,35,hitter,111111,11111_hitter,Old Guy,Hitter,NA", "Aaron Judge,15640,33,hitter,592450,15640_hitter,Aaron Judge,Hitter,NA",
    "Logan Webb,17995,28,pitcher,657277,17995_pitcher,Logan Webb,SP,SP"
  ), file.path(data_dir, "player_lookup.csv"))
  writeLines(sprintf('{"refreshed_at": "x", "refreshed_timezone": "UTC", "data_through_date": "%s"}', spec$locked_date), file.path(data_dir, "data_freshness.json"))

  out <- file.path(golden_root, name)
  copy_dir(data_dir, file.path(out, "inputs", "data"))

  expression <- paste(
    "Sys.time <- function() as.POSIXct('2026-11-26 15:00:00', tz = 'UTC');",
    "source('fixture_http.R'); options(refresh_http_get = fixture_http()); source('refresh/refresh_data.R')"
  )
  env <- c(paste0("FIXTURE_DIR=", file.path(dir, spec$current)), if (isTRUE(spec$blank_whiff)) "FIXTURE_BLANK_WHIFF=1")
  original <- setwd(dir)
  output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(expression)), stdout = TRUE, stderr = TRUE, env = env))
  setwd(original)
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L

  copy_dir(data_dir, file.path(out, "expected", "data"))
  writeLines(as.character(status), file.path(out, "exit_status"))
  cat(sprintf("%-12s exit status %d, %d output lines\n", name, status, length(output)))
}

for (name in names(scenarios)) run_scenario(name, scenarios[[name]])
