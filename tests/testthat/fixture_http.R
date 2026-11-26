# A stand-in for the network that answers each source's URL with a fixture file. Plain R with no testthat
# dependency, so the end-to-end tests can load it in the Rscript they launch.

fixture_dir <- function() {
  from_env <- Sys.getenv("FIXTURE_DIR")
  if (nzchar(from_env)) from_env else testthat::test_path("fixtures")
}

# A fixture file's exact text, byte-order mark included (Savant's CSVs start with one).
fixture_text <- function(name) {
  path <- file.path(fixture_dir(), name)
  text <- readChar(path, file.size(path), useBytes = TRUE)
  Encoding(text) <- "UTF-8"
  text
}

fixture_name_for_url <- function(url) {
  if (grepl("stats=sabermetrics&group=hitting", url, fixed = TRUE)) "mlb_sabermetrics_hitting.json"
  else if (grepl("stats=season&group=hitting", url, fixed = TRUE)) "mlb_hitting_season.json"
  else if (grepl("stats=season&group=pitching", url, fixed = TRUE)) "mlb_pitching_season.json"
  else if (grepl("expected_statistics?type=batter", url, fixed = TRUE)) "savant_expected_batter.csv"
  else if (grepl("statcast?type=batter", url, fixed = TRUE)) "savant_statcast_batter.csv"
  else if (grepl("expected_statistics?type=pitcher", url, fixed = TRUE)) "savant_expected_pitcher.csv"
  else if (grepl("/custom?", url, fixed = TRUE)) "savant_custom_pitcher.csv"
  else stop("no fixture for ", url)
}

# Records the URLs asked for in `requested$urls` when an environment is given. With FIXTURE_BLANK_WHIFF set, the Savant
# custom leaderboard comes back with no whiff% values, the way a reshaped feed would.
fixture_http <- function(requested = NULL) {
  function(url) {
    if (!is.null(requested)) requested$urls <- c(requested$urls, url)
    name <- fixture_name_for_url(url)
    if (nzchar(Sys.getenv("FIXTURE_BLANK_WHIFF")) && name == "savant_custom_pitcher.csv") name <- "savant_custom_pitcher_blank_whiff.csv"
    fixture_text(name)
  }
}
