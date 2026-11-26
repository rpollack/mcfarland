source(file.path("..", "..", "data_sources.R"))

# A fixture file's exact text, byte-order mark included (Savant's CSVs start with one).
fixture_text <- function(name) {
  path <- test_path("fixtures", name)
  text <- readChar(path, file.size(path), useBytes = TRUE)
  Encoding(text) <- "UTF-8"
  text
}

# Stands in for the network: answers each source's URL with its fixture and records what was asked for.
fixture_http <- function(requested = NULL) {
  function(url) {
    if (!is.null(requested)) requested$urls <- c(requested$urls, url)
    name <- if (grepl("stats=sabermetrics&group=hitting", url, fixed = TRUE)) "mlb_sabermetrics_hitting.json"
    else if (grepl("stats=season&group=hitting", url, fixed = TRUE)) "mlb_hitting_season.json"
    else if (grepl("stats=season&group=pitching", url, fixed = TRUE)) "mlb_pitching_season.json"
    else if (grepl("expected_statistics?type=batter", url, fixed = TRUE)) "savant_expected_batter.csv"
    else if (grepl("statcast?type=batter", url, fixed = TRUE)) "savant_statcast_batter.csv"
    else if (grepl("expected_statistics?type=pitcher", url, fixed = TRUE)) "savant_expected_pitcher.csv"
    else if (grepl("/custom?", url, fixed = TRUE)) "savant_custom_pitcher.csv"
    else if (grepl("statcast?type=pitcher", url, fixed = TRUE)) "savant_statcast_pitcher.csv"
    else stop("no fixture for ", url)
    fixture_text(name)
  }
}

make_temp_dir <- function() {
  dir <- tempfile("data-sources-test-")
  dir.create(dir)
  dir
}
