# Baselines, frozen player ids, the published-file contract, the gates, and one end-to-end run of refresh_data.R on
# fixtures. No test touches the network.

repo_root <- normalizePath(file.path("..", ".."))
# The refresh script's own current_year, so these tests keep working when it is bumped for a new season.
script_current_year <- as.integer(sub(".*current_year <- ([0-9]+).*", "\\1", grep("^current_year <- ", readLines(file.path(repo_root, "refresh", "refresh_data.R")), value = TRUE)))
data_through_today <- function() as.character(as.Date(Sys.time(), tz = "America/Chicago") - 1)

# --- baselines ----------------------------------------------------------------

test_that("pitcher baseline metrics are weighted by season and keep percents as percents", {
  seasons <- tibble::tibble(
    mlbamid = c("1", "1"), position = c("SP", "SP"), w = c(1, 3),
    er = c(10, 20), ip = c(90, 100), so = c(80, 120), bb = c(20, 30), tbf = c(400, 420),
    x_era = c(3, 4), fip = c(3.2, 3.8), barrel_percent = c(7, 9), o_swing_percent = c(30, 32),
    babip = c(0.29, 0.3), lob_percent = c(75, 80), whiff_percent = c(24, 26)
  )
  out <- summarize_pitcher_baseline_metrics(dplyr::group_by(seasons, mlbamid, position))
  expect_equal(out$era, 9 * (10 * 1 + 20 * 3) / (90 * 1 + 100 * 3))
  expect_equal(out$k_percent, 100 * (80 * 1 + 120 * 3) / (400 * 1 + 420 * 3))
  # Weighted by batters faced times the season weight; the values are already percents, so no x100.
  expect_equal(out$whiff_percent, (24 * 400 * 1 + 26 * 420 * 3) / (400 * 1 + 420 * 3))
  expect_true(out$whiff_percent > 20 && out$whiff_percent < 30)
  expect_equal(out$barrel_percent, (7 * 400 * 1 + 9 * 420 * 3) / (400 * 1 + 420 * 3))
  expect_false("csw_percent" %in% colnames(out))
})

test_that("the three seasons before the current one are weighted 1, 3, 5", {
  expect_equal(baseline_seasons_for(2027), data.frame(year = c(2024, 2025, 2026), weight = c(1, 3, 5)))
})

test_that("a baseline file in another shape is refused by name", {
  dir <- make_temp_dir()
  seasons <- baseline_seasons_for(2026)
  for (year in seasons$year) {
    write_baseline_season(year, build_season_stats(2025, any_season_min_rows, fixture_http()), dir, force = TRUE)
  }
  expect_equal(nrow(read_hitter_baseline_seasons(seasons, dir)), 9)
  expect_equal(unique(read_pitcher_baseline_seasons(seasons, dir)$w), c(1, 3, 5))

  broken <- readr::read_csv(baseline_file("pitchers", 2025, dir), show_col_types = FALSE) |> dplyr::rename(csw_percent = whiff_percent)
  readr::write_csv(broken, baseline_file("pitchers", 2025, dir))
  expect_error(read_pitcher_baseline_seasons(seasons, dir), "baseline-pitchers-2025.csv is missing expected columns: whiff_percent")

  file.remove(baseline_file("hitters", 2024, dir))
  expect_error(read_hitter_baseline_seasons(seasons, dir), "baseline-hitters-2024.csv is missing; run Rscript refresh/refresh_data.R build-baseline-season 2024")
})

test_that("the baseline files for the three seasons before current_year are committed", {
  # Bumping current_year in refresh/refresh_data.R for a new season needs last season's files first
  # (Rscript refresh/refresh_data.R build-baseline-season <year>); this fails the bump's PR if they are missing.
  for (year in baseline_seasons_for(script_current_year)$year) {
    expect_true(file.exists(file.path(repo_root, baseline_file("hitters", year))), info = paste("hitters", year))
    expect_true(file.exists(file.path(repo_root, baseline_file("pitchers", year))), info = paste("pitchers", year))
  }
})

# --- frozen player ids ---------------------------------------------------------

test_that("the frozen id map is exactly as it was built and its two id ranges cannot overlap", {
  map_file <- file.path(repo_root, "data", "player_id_map.csv")
  # Built once from the player_lookup.csv that was live when the pipeline left FanGraphs. Never regenerate it: doing
  # so would change every existing player's id (URLs, votes, trend rows, cached analyses, stars).
  expect_equal(unname(tools::md5sum(map_file)), "49d04183a2e4f47f0ba412549425a6e7")

  id_map <- read_player_id_map(map_file)
  expect_equal(nrow(id_map), 1464)
  expect_true(all(grepl("^[0-9]+$", id_map$PlayerId)))
  expect_lt(max(as.numeric(id_map$PlayerId)), 100000)
  expect_gt(min(as.numeric(id_map$mlbamid)), 400000)
})

test_that("a mapped player keeps the legacy id and a new player gets the mlbamid", {
  id_map <- tibble::tibble(mlbamid = c("592450", "657277"), PlayerId = c("15640", "17995"))
  players <- tibble::tibble(mlbamid = c("592450", "660271", "657277"), Name = c("a", "b", "c"))
  with_ids <- add_player_ids(players, id_map)
  expect_equal(with_ids$PlayerId, c("15640", "660271", "17995"))
  expect_equal(with_ids$Name, c("a", "b", "c"))
})

test_that("a map that repeats a player or an id is refused, and so is an id collision", {
  dir <- make_temp_dir()
  file <- file.path(dir, "map.csv")
  writeLines(c("mlbamid,PlayerId", "1,5", "2,5"), file)
  expect_error(read_player_id_map(file), "maps a player or an id more than once")

  id_map <- tibble::tibble(mlbamid = "592450", PlayerId = "15640")
  expect_no_error(check_player_id_collisions(tibble::tibble(mlbamid = c("592450", "660271")), id_map))
  expect_error(check_player_id_collisions(tibble::tibble(mlbamid = "15640"), id_map), "equal to a frozen legacy PlayerId: 15640")
})

test_that("the committed outputs still carry every mapped player's legacy id", {
  id_map <- read_player_id_map(file.path(repo_root, "data", "player_id_map.csv"))
  for (file in c("full_stats_hitters.csv", "full_stats_pitchers.csv", "player_lookup.csv")) {
    published <- readr::read_csv(file.path(repo_root, "data", file), show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character()))
    joined <- dplyr::inner_join(published, id_map, by = "mlbamid", suffix = c("", "_frozen"))
    expect_true(nrow(joined) > 0, info = file)
    expect_equal(joined$PlayerId, joined$PlayerId_frozen, info = file)
    unmapped <- published$mlbamid[!published$mlbamid %in% id_map$mlbamid]
    expect_equal(published$PlayerId[published$mlbamid %in% unmapped], unmapped, info = file)
  }
})

# --- the published files -------------------------------------------------------

test_that("the committed outputs have exactly the contracted columns", {
  hitters <- readr::read_csv(file.path(repo_root, "data", "full_stats_hitters.csv"), show_col_types = FALSE)
  pitchers <- readr::read_csv(file.path(repo_root, "data", "full_stats_pitchers.csv"), show_col_types = FALSE)
  expect_equal(colnames(hitters), hitter_output_columns)
  expect_equal(colnames(pitchers), pitcher_output_columns)
  expect_false(any(grepl("csw|x_fip|hr_fb|f_bv|c_tv|s_fv|ev90|barrels", colnames(pitchers))))
  expect_true(all(c("whiff_percent_cur", "whiff_percent_l3", "whiff_percent_diff", "whiff_percent_lg_adj_diff") %in% colnames(pitchers)))
})

# --- the gates -----------------------------------------------------------------

tiny_output <- function(type, ids, names = paste("Player", ids)) {
  tibble::tibble(PlayerId = ids, mlbamid = ids, Name = names, Age = 30, player_type = type, stat_cur = 1)
}
tiny_lookup <- function(...) {
  bind_rows(...) |> dplyr::mutate(compound_id = paste0(PlayerId, "_", player_type)) |> dplyr::select(PlayerId, mlbamid, Name, player_type, compound_id)
}

test_that("with the same data date a changed player list fails, unless the source migration was accepted", {
  locked_h <- tiny_output("hitter", c("1", "2", "3"))
  new_h <- tiny_output("hitter", c("1", "2", "4")) |> dplyr::mutate(stat_cur = 5)
  p <- tiny_output("pitcher", c("10", "11"))
  lookup <- function(h) tiny_lookup(h, p)

  strict <- validate_migration_parity(locked_h, p, lookup(locked_h), new_h, p, lookup(new_h), "2026-11-16", "2026-11-16")
  expect_equal(strict$status, "fail")
  expect_true(all(c("locked players missing from candidates", "candidate contains non-locked players", "stat parity mismatches") %in% strict$failures))

  migrated <- validate_migration_parity(locked_h, p, lookup(locked_h), new_h, p, lookup(new_h), "2026-11-16", "2026-11-16", accept_source_migration = TRUE)
  expect_equal(migrated$status, "pass")
  expect_true(migrated$source_migration)
  expect_match(migrated$warnings[[1]], "Source migration accepted")

  # The new files, a day later, are checked for integrity only.
  later <- validate_migration_parity(new_h, p, lookup(new_h), new_h, p, lookup(new_h), "2026-11-16", "2026-11-17")
  expect_equal(later$status, "pass")
})

test_that("accepting the source migration still catches duplicate compound ids, missing mlbamids and sanity failures", {
  p <- tiny_output("pitcher", c("10", "11"))
  h <- tiny_output("hitter", c("1", "2"))
  duplicated_lookup <- tiny_lookup(h, p, h[1, ])
  expect_true("duplicate compound IDs" %in% validate_migration_parity(h, p, tiny_lookup(h, p), h, p, duplicated_lookup, "2026-11-16", "2026-11-16", accept_source_migration = TRUE)$failures)

  no_id <- dplyr::mutate(h, mlbamid = c("1", NA))
  expect_true("served players missing mlbamid" %in% validate_migration_parity(h, p, tiny_lookup(h, p), no_id, p, tiny_lookup(h, p), "2026-11-16", "2026-11-16", accept_source_migration = TRUE)$failures)

  failed <- validate_migration_parity(h, p, tiny_lookup(h, p), h, p, tiny_lookup(h, p), "2026-11-16", "2026-11-16", accept_source_migration = TRUE, extra_failures = "a feed went empty")
  expect_equal(failed$status, "fail")
  expect_equal(failed$failures, "a feed went empty")
})

full_hitters <- function() {
  tibble::tibble(Age = 30, PA_cur = 100, wOBA_cur = 0.3, xwOBA_cur = 0.31, Barrel_pct_cur = 8, AVG_cur = 0.25)
}
full_pitchers <- function() {
  tibble::tibble(Age = 30, tbf = 100, era_cur = 4, fip_cur = 4, xera_cur = 4, barrel_percent_cur = 8, o_swing_percent_cur = 30, whiff_percent_cur = 25, babip_cur = 0.3, lob_percent_cur = 72)
}

test_that("a source that went empty fails the run and the message names it", {
  ok <- check_output_sanity(full_hitters(), full_pitchers(), full_hitters(), full_pitchers(), "2026-11-16", 2026)
  expect_equal(ok, character())

  no_whiff <- dplyr::mutate(full_pitchers(), whiff_percent_cur = NA_real_)
  expect_equal(check_output_sanity(full_hitters(), no_whiff, full_hitters(), full_pitchers(), "2026-11-16", 2026), "whiff_percent_cur is empty for every pitcher (source: Savant custom)")

  no_xwoba <- dplyr::mutate(full_hitters(), xwOBA_cur = NA_real_)
  expect_match(check_output_sanity(no_xwoba, full_pitchers(), full_hitters(), full_pitchers(), "2026-11-16", 2026), "xwOBA_cur is empty for every hitter \\(source: Savant expected_statistics\\)")

  expect_match(check_output_sanity(full_hitters()[0, ], full_pitchers(), full_hitters(), full_pitchers(), "2026-11-16", 2026), "no hitters in the new output")
})

test_that("within a season the player lists may not shrink, but a new season starts small", {
  many <- function(n) dplyr::bind_rows(replicate(n, full_hitters(), simplify = FALSE))
  expect_match(check_output_sanity(many(90), full_pitchers(), many(100), full_pitchers(), "2026-06-01", 2026), "hitter list shrank from 100 to 90 rows within the 2026 season")
  expect_equal(check_output_sanity(many(99), full_pitchers(), many(100), full_pitchers(), "2026-06-01", 2026), character())
  # Last season's final files are much bigger than opening week; that is not a failure.
  expect_equal(check_output_sanity(many(10), full_pitchers(), many(100), full_pitchers(), "2026-10-01", 2027), character())
  # The one source-migration run gets a wider band.
  expect_equal(check_output_sanity(many(90), full_pitchers(), many(100), full_pitchers(), "2026-11-16", 2026, accept_source_migration = TRUE), character())
  expect_match(check_output_sanity(many(80), full_pitchers(), many(100), full_pitchers(), "2026-11-16", 2026, accept_source_migration = TRUE), "shrank")
})

# --- refresh_data.R end to end, on fixtures ------------------------------------

# Copies the scripts, fixtures, a frozen id map and three baseline seasons into a scratch directory laid out like the
# repository (refresh/ and data/), then runs refresh/refresh_data.R there with the network replaced by the fixtures.
run_refresh <- function(args = character(), locked_date = "2026-11-16", blank_whiff = FALSE) {
  dir <- make_temp_dir()
  data_dir <- file.path(dir, "data")
  baseline_dir <- file.path(data_dir, "baselines")
  dir.create(baseline_dir, recursive = TRUE)
  file.copy(file.path(repo_root, "refresh"), dir, recursive = TRUE)
  file.copy(file.path("fixture_http.R"), dir)
  file.copy(test_path("fixtures"), dir, recursive = TRUE)
  writeLines(c("mlbamid,PlayerId", "592450,15640", "657277,17995", "111111,11111"), file.path(data_dir, "player_id_map.csv"))
  for (year in baseline_seasons_for(script_current_year)$year) write_baseline_season(year, build_season_stats(2025, any_season_min_rows, fixture_http()), baseline_dir, force = TRUE)

  # What is "in production": an old hitter who is gone from the new data, Judge, and one pitcher.
  writeLines(c("Name,PlayerId,PA_cur,Age,mlbamid", "Old Guy,11111,200,35,111111", "Aaron Judge,15640,679,33,592450"), file.path(data_dir, "full_stats_hitters.csv"))
  writeLines(c("PlayerId,mlbamid,Name,Age,tbf,position", "17995,657277,Logan Webb,28,856,SP"), file.path(data_dir, "full_stats_pitchers.csv"))
  writeLines(c(
    "Name,PlayerId,Age,player_type,mlbamid,compound_id,display_name,position_detail,position",
    "Old Guy,11111,35,hitter,111111,11111_hitter,Old Guy,Hitter,NA", "Aaron Judge,15640,33,hitter,592450,15640_hitter,Aaron Judge,Hitter,NA",
    "Logan Webb,17995,28,pitcher,657277,17995_pitcher,Logan Webb,SP,SP"
  ), file.path(data_dir, "player_lookup.csv"))
  writeLines(sprintf('{"refreshed_at": "x", "refreshed_timezone": "UTC", "data_through_date": "%s"}', locked_date), file.path(data_dir, "data_freshness.json"))
  before <- tools::md5sum(file.path(data_dir, c("full_stats_hitters.csv", "full_stats_pitchers.csv", "player_lookup.csv")))

  expression <- "source('fixture_http.R'); options(refresh_http_get = fixture_http()); source('refresh/refresh_data.R')"
  original_dir <- setwd(dir)
  on.exit(setwd(original_dir), add = TRUE)
  output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(expression), args),
    stdout = TRUE, stderr = TRUE, env = c(paste0("FIXTURE_DIR=", file.path(dir, "fixtures")), if (blank_whiff) "FIXTURE_BLANK_WHIFF=1")))
  list(dir = dir, status = attr(output, "status") %||% 0L, output = paste(output, collapse = "\n"), before = before)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

read_published <- function(dir, file) {
  readr::read_csv(file.path(dir, "data", file), show_col_types = FALSE, col_types = readr::cols(PlayerId = readr::col_character(), mlbamid = readr::col_character()))
}

test_that("refresh_data.R builds the published files from the sources, keeping legacy ids", {
  run <- run_refresh(locked_date = "2026-11-16")
  expect_equal(run$status, 0L, info = run$output)
  expect_match(run$output, "Production files promoted: TRUE")

  hitters <- read_published(run$dir, "full_stats_hitters.csv")
  pitchers <- read_published(run$dir, "full_stats_pitchers.csv")
  lookup <- read_published(run$dir, "player_lookup.csv")
  expect_equal(colnames(hitters), hitter_output_columns)
  expect_equal(colnames(pitchers), pitcher_output_columns)

  # Judge keeps the frozen id, Ohtani (not in the map) gets the MLB id, and a hitter's baseline is joined by mlbamid.
  expect_equal(hitters$PlayerId[hitters$mlbamid == "592450"], "15640")
  expect_equal(hitters$PlayerId[hitters$mlbamid == "660271"], "660271")
  expect_false(is.na(hitters$wOBA_l3[hitters$mlbamid == "592450"]))
  expect_equal(sort(hitters$Name), c("Aaron Judge", "Jose Altuve", "Shohei Ohtani"))

  # Webb keeps the id and the role-specific baseline; the pitcher columns are whiff%, not CSW%.
  webb <- pitchers[pitchers$mlbamid == "657277", ]
  expect_equal(webb$PlayerId, "17995")
  expect_equal(webb$whiff_percent_cur, 24.4)
  expect_equal(webb$whiff_percent_l3, 24.4)
  expect_equal(webb$whiff_percent_diff, 0)
  expect_equal(webb$season, script_current_year)
  expect_false("Carlos Cortes" %in% pitchers$Name)

  # Ohtani is on both lists, with the same id.
  expect_equal(sort(lookup$compound_id[lookup$mlbamid == "660271"]), c("660271_hitter", "660271_pitcher"))
  expect_false(anyDuplicated(lookup$compound_id) > 0)
  expect_equal(jsonlite::fromJSON(file.path(run$dir, "data", "data_freshness.json"))$data_through_date, data_through_today())
})

test_that("a source migration run needs the flag when the data date has not moved", {
  without <- run_refresh(locked_date = data_through_today())
  expect_equal(without$status, 1L)
  expect_match(without$output, "Migration parity status: fail")
  expect_match(without$output, "locked players missing from candidates")
  expect_equal(unname(tools::md5sum(file.path(without$dir, "data", c("full_stats_hitters.csv", "full_stats_pitchers.csv", "player_lookup.csv")))), unname(without$before))

  with_flag <- run_refresh(args = "--accept-source-migration", locked_date = data_through_today())
  expect_equal(with_flag$status, 0L, info = with_flag$output)
  expect_match(with_flag$output, "--accept-source-migration")
  expect_match(with_flag$output, "Production files promoted: TRUE")
})

test_that("a feed that comes back empty fails the run, names the source and leaves the published files alone", {
  run <- run_refresh(blank_whiff = TRUE)
  expect_equal(run$status, 1L)
  expect_match(run$output, "whiff_percent_cur is empty for every pitcher \\(source: Savant custom\\)")
  expect_match(run$output, "Production files promoted: FALSE")
  expect_equal(unname(tools::md5sum(file.path(run$dir, "data", c("full_stats_hitters.csv", "full_stats_pitchers.csv", "player_lookup.csv")))), unname(run$before))
})
