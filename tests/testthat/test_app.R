library(testthat)

# Ensure local data path for tests
GITHUB_DATA_URL <<- "./"

# Reset caches before tests
.baseball_data_cache <<- NULL
.cache_timestamp <<- NULL
cache_env$api_responses <- list()

# Helper to load local baseball data
load_local_data <- function() {
  load_baseball_data()
}

# -----------------------------------------------------------------------------
test_that("load_baseball_data reads local CSVs", {
  data <- load_local_data()
  expect_true(all(c("hitters", "pitchers", "lookup") %in% names(data)))
  expect_gt(nrow(data$hitters), 0)
  expect_gt(nrow(data$pitchers), 0)
})

test_that("load_baseball_data_cached uses cached data on subsequent calls", {
  .baseball_data_cache <<- NULL
  .cache_timestamp <<- NULL
  GITHUB_DATA_URL <<- "./"
  first <- load_baseball_data_cached()
  GITHUB_DATA_URL <<- "./nonexistent/"
  second <- load_baseball_data_cached()
  expect_equal(first$hitters, second$hitters)
})

test_that("formatting helpers work as expected", {
  expect_equal(format_stat_value(0.1234), ".123")
  expect_equal(format_stat_value(2), "2")
  expect_equal(format_stat_value(NA), "N/A")
  expect_equal(format_percentage(0.123), "12.3%")
  expect_equal(format_percentage(NA), "N/A")
  expect_equal(format_era(1.234), "1.23")
})

test_that("personas and default persona work", {
  expect_true(grepl("Gen Z", get_analysis_persona("gen_z"), ignore.case = TRUE))
  expect_true(startsWith(get_analysis_persona("unknown"), "Keep it simple"))
  expect_true(grepl("fantasy baseball", get_analysis_persona("fantasy_expert"), ignore.case = TRUE))
})

test_that("call_openai_api handles missing API key", {
  Sys.setenv(OPENAI_API_KEY = "")
  result <- call_openai_api("test prompt", "analytics_dork")
  expect_true(grepl("OpenAI API Key Not Set", as.character(result)))
})

test_that("prompt builders return text when player exists", {
  data <- load_local_data()
  hitter_name <- data$hitters$Name[1]
  pitcher_name <- data$pitchers$Name[1]
  expect_true(grepl(hitter_name, build_hitter_prompt(hitter_name, data$hitters)))
  expect_true(grepl(pitcher_name, build_pitcher_prompt(pitcher_name, data$pitchers)))
  expect_null(build_hitter_prompt("NotAPlayer", data$hitters))
})

test_that("cache key generation produces consistent hashes", {
  key1 <- generate_cache_key("prompt", "mode")
  key2 <- generate_cache_key("prompt", "mode")
  expect_equal(key1, key2)
  expect_equal(nchar(key1), 32)
})

test_that("photo and player info helpers work", {
  data <- load_local_data()
  player_id <- data$lookup$PlayerId[1]
  url <- get_player_photo_url(player_id, data)
  expect_true(grepl("mlbstatic|fangraphs", url))
  info <- get_player_info(player_id, data)
  expect_equal(info$name, data$lookup$Name[1])
})
