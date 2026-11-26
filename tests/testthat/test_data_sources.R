# The fetch layer and the season tables, run against small fixtures in the shape of the real responses.
# No test touches the network.

# --- helpers ---------------------------------------------------------------

test_that("MLB innings are read as thirds", {
  expect_equal(parse_mlb_innings(c("207.0", "58.2", "0.1", "1")), c(207, 58 + 2 / 3, 1 / 3, 1))
})

test_that("a transient failure is retried and a persistent one names the source", {
  calls <- 0
  flaky <- function() {
    calls <<- calls + 1
    if (calls < 3) stop("status 503")
    "ok"
  }
  expect_equal(with_retry(flaky, "Savant test", wait_seconds = 0), "ok")
  expect_equal(calls, 3)
  expect_error(
    fetch_text("https://example.invalid", "Savant test", function(url) stop("status 403"), attempts = 1),
    "Savant test failed after 1 attempts: status 403"
  )
})

test_that("require_columns names every missing column", {
  expect_error(require_columns(tibble::tibble(a = 1), c("a", "b", "c"), "Some feed"), "Some feed is missing expected columns: b, c")
})

# --- MLB Stats API -----------------------------------------------------------

test_that("an MLB response keeps only major-league rows and requires the columns it uses", {
  parsed <- parse_mlb_stats(fixture_text("mlb_hitting_season.json"), "hitting")
  expect_equal(nrow(parsed), 5)
  expect_false("999001" %in% as.character(parsed$player.id))

  hitting <- fetch_mlb_hitting_season(2025, fixture_http())
  expect_equal(nrow(hitting), 5)
  expect_equal(hitting$Name[hitting$mlbamid == "514888"], "Jose Altuve")
  expect_equal(hitting$Age[hitting$mlbamid == "592450"], 33)

  expect_error(parse_mlb_stats('{"stats":[{"splits":[]}]}', "hitting"), "returned no rows")
  expect_error(parse_mlb_stats(fixture_text("mlb_hitting_season.json"), "hitting", min_rows = 10), "fewer than the 10 expected")
  no_age <- gsub('"age"', '"ageX"', fixture_text("mlb_hitting_season.json"), fixed = TRUE)
  expect_error(fetch_mlb_hitting_season(2025, function(url) no_age), "missing expected columns: stat.age")
})

test_that("a player repeated in an MLB feed stops the run", {
  repeated <- sub('"id": 607067', '"id": 592450', fixture_text("mlb_hitting_season.json"), fixed = TRUE)
  expect_error(parse_mlb_stats(repeated, "hitting"), "more than one row for a player")
  expect_equal(nrow(parse_mlb_stats(repeated, "hitting", one_row_per_player = FALSE)), 5)
})

# --- Baseball Savant ---------------------------------------------------------

test_that("a Savant CSV is read past its byte-order mark with ids as text", {
  expect_true(startsWith(fixture_text("savant_expected_batter.csv"), "﻿"))
  parsed <- parse_savant_csv(fixture_text("savant_expected_batter.csv"), "est_woba", "Savant test")
  expect_true("last_name, first_name" %in% colnames(parsed))
  expect_equal(parsed$mlbamid, c("592450", "660271", "514888", "607067"))
  expect_type(parsed$est_woba, "double")
})

test_that("a Savant response that is empty, HTML, thin, repeated or reshaped fails loudly", {
  csv <- fixture_text("savant_expected_batter.csv")
  expect_error(parse_savant_csv("", "est_woba", "Savant test"), "Savant test returned an empty response")
  expect_error(parse_savant_csv("<!DOCTYPE html><html><body>Just a moment...</body></html>", "est_woba", "Savant test"), "returned HTML instead of CSV")
  expect_error(parse_savant_csv(csv, "xera", "Savant test"), "Savant test is missing expected columns: xera")
  expect_error(parse_savant_csv(csv, "est_woba", "Savant test", min_rows = 50), "returned 4 rows, fewer than the 50 expected")
  repeated <- sub('"660271"', '"592450"', csv, fixed = TRUE)
  expect_error(parse_savant_csv(repeated, "est_woba", "Savant test"), "missing or repeated player_id")
  expect_error(parse_savant_csv("player_id\n", "est_woba", "Savant test"), "missing expected columns|fewer than")
})

test_that("each Savant fetcher asks for the right leaderboard, season and player type", {
  requested <- new.env()
  requested$urls <- character()
  http <- fixture_http(requested)
  fetch_savant_hitter_xwoba(2025, http)
  fetch_savant_hitter_barrels(2025, http)
  fetch_savant_pitcher_xera(2025, http)
  fetch_savant_pitcher_custom(2025, http)
  expect_match(requested$urls[1], "leaderboard/expected_statistics?type=batter&year=2025", fixed = TRUE)
  expect_match(requested$urls[2], "leaderboard/statcast?type=batter&year=2025", fixed = TRUE)
  expect_match(requested$urls[3], "leaderboard/expected_statistics?type=pitcher&year=2025", fixed = TRUE)
  expect_match(requested$urls[4], "leaderboard/custom?year=2025&type=pitcher", fixed = TRUE)
  expect_match(requested$urls[4], "barrel_batted_rate,oz_swing_percent,whiff_percent", fixed = TRUE)
  expect_true(all(grepl("&csv=true$", requested$urls)))
})

# --- computed fields ---------------------------------------------------------

test_that("FIP counts intentional walks and the constant makes league FIP equal league ERA", {
  pitching <- fetch_mlb_pitching_season(2025, fixture_http())
  constant <- with(pitching, fip_constant(ip, er, hr, bb, hbp, so))
  # League (position players included): 313 2/3 IP, 101 ER, 24 HR, 81 BB, 9 HBP, 354 K.
  # ERA = 9 * 101 / (313 2/3); constant = ERA - (13 * 24 + 3 * (81 + 9) - 2 * 354) / (313 2/3) = ERA + 126 / (313 2/3).
  expect_equal(constant, (9 * 101 + 126) / (313 + 2 / 3), tolerance = 1e-9)

  fip <- compute_fip(pitching$hr, pitching$bb, pitching$hbp, pitching$so, pitching$ip, constant)
  with_batters <- pitching$ip > 0
  expect_equal(sum(fip[with_batters] * pitching$ip[with_batters]) / sum(pitching$ip), 9 * sum(pitching$er) / sum(pitching$ip), tolerance = 1e-9)
  expect_true(is.na(fip[!with_batters]))
  # Logan Webb: (13 * 14 + 3 * (46 + 5) - 2 * 224) / 207 + constant
  expect_equal(fip[pitching$mlbamid == "657277"], -113 / 207 + constant, tolerance = 1e-9)
})

test_that("BABIP and LOB% follow the standard formulas, with LOB% clamped to 0-100", {
  expect_equal(compute_babip(h = 190, hr = 14, ab = 770, so = 224, sf = 5), 176 / 537)
  expect_true(is.na(compute_babip(h = 0, hr = 0, ab = 0, so = 0, sf = 0)))

  expect_equal(compute_lob_percent(h = 190, bb = 46, hbp = 5, hr = 14, r = 80), 100 * 161 / 221.4)
  # One inning, two home runs: the formula gives 500%, which cannot be a rate, so it is clamped.
  expect_equal(compute_lob_percent(h = 3, bb = 0, hbp = 0, hr = 2, r = 2), 100)
  # More runs than baserunners cannot go below zero.
  expect_equal(compute_lob_percent(h = 4, bb = 0, hbp = 0, hr = 0, r = 6), 0)
  # Nobody could have scored: no baserunners, or a lone home run.
  expect_true(is.na(compute_lob_percent(h = 0, bb = 0, hbp = 0, hr = 0, r = 0)))
  expect_true(is.na(compute_lob_percent(h = 1, bb = 0, hbp = 0, hr = 1, r = 1)))
})

# --- season tables -----------------------------------------------------------

test_that("the hitter table drops batting pitchers and zero-PA players but keeps the two-way player", {
  season <- build_season_stats(2025, any_season_min_rows, fixture_http())
  hitters <- season$hitters
  expect_equal(sort(hitters$mlbamid), sort(c("592450", "660271", "514888")))
  expect_false("607067" %in% hitters$mlbamid)
  expect_false("680911" %in% hitters$mlbamid)
  expect_true("660271" %in% hitters$mlbamid)
  expect_equal(colnames(hitters), c("mlbamid", "Name", "Age", "G", "AB", "PA", "H", "2B", "3B", "HR", "BB", "HBP", "SF", "SO", "TB", "Barrels", "wOBA", "xwOBA"))

  judge <- hitters[hitters$mlbamid == "592450", ]
  expect_equal(judge$PA, 679)
  expect_equal(judge$Barrels, 84)
  expect_equal(judge$wOBA, 0.462771)
  expect_equal(judge$xwOBA, 0.449)
})

test_that("a hitter with no Savant batted-ball row has 0 barrels but keeps the other values", {
  altuve <- build_season_stats(2025, any_season_min_rows, fixture_http())$hitters |> dplyr::filter(mlbamid == "514888")
  expect_equal(altuve$Name, "Jose Altuve")
  expect_equal(altuve$Barrels, 0)
  expect_equal(altuve$xwOBA, 0.318)
})

test_that("the pitcher table classifies roles, computes rates and leaves out position players", {
  pitchers <- build_season_stats(2025, any_season_min_rows, fixture_http())$pitchers
  expect_equal(sort(pitchers$mlbamid), sort(c("657277", "695243", "660271")))
  expect_false("555001" %in% pitchers$mlbamid)

  webb <- pitchers[pitchers$mlbamid == "657277", ]
  expect_equal(webb$position, "SP")
  expect_equal(webb$age, 28)
  expect_equal(webb$ip, 207)
  expect_equal(webb$x_era, 3.58)
  expect_equal(webb$barrel_percent, 7.1)
  expect_equal(webb$o_swing_percent, 31.5)
  expect_equal(webb$whiff_percent, 24.4)
  expect_equal(webb$babip, round(176 / 537, 6))

  miller <- pitchers[pitchers$mlbamid == "695243", ]
  expect_equal(miller$position, "RP")
  expect_equal(miller$ip, round(58 + 2 / 3, 6))

  # A position player who pitched is not a pitcher (the FIP still counts in the league constant); Ohtani (TWP) is.
  expect_false("666126" %in% pitchers$mlbamid)
  expect_true("660271" %in% pitchers$mlbamid)
})

test_that("a thin response is refused for a full season and accepted while a season is in progress", {
  expect_error(build_season_stats(2025, full_season_min_rows, fixture_http()), "fewer than the \\d+ expected")
  expect_no_error(build_season_stats(2025, any_season_min_rows, fixture_http()))
})

# --- baseline files ----------------------------------------------------------

test_that("baseline files are named by kind and year", {
  expect_equal(baseline_file("hitters", 2025, "data"), file.path("data", "baseline-hitters-2025.csv"))
  expect_equal(baseline_file("pitchers", 2023), file.path(".", "baseline-pitchers-2023.csv"))
})

test_that("a baseline season is written once and replaced only with force", {
  dir <- make_temp_dir()
  stats <- build_season_stats(2025, any_season_min_rows, fixture_http())
  files <- write_baseline_season(2025, stats, dir)
  expect_true(all(file.exists(files)))

  written <- readr::read_csv(files[["hitters"]], col_types = readr::cols(mlbamid = readr::col_character()), show_col_types = FALSE)
  expect_equal(nrow(written), 3)
  expect_equal(colnames(written), colnames(stats$hitters))

  writeLines("marker", files[["pitchers"]])
  expect_error(write_baseline_season(2025, stats, dir), "Refusing to overwrite")
  expect_error(build_baseline_season(2025, dir = dir, http_get = fixture_http()), "Refusing to overwrite")
  expect_equal(readLines(files[["pitchers"]]), "marker")

  write_baseline_season(2025, stats, dir, force = TRUE)
  expect_equal(nrow(readr::read_csv(files[["pitchers"]], show_col_types = FALSE)), 3)
})
