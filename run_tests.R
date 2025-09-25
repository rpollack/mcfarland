# Run all automated tests for the McFARLAND app

# Load required packages
suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(httr)
  library(jsonlite)
  library(commonmark)
  library(htmltools)
  library(digest)
  library(tibble)
  library(rlang)
  library(later)
})

# Minimal application environment -------------------------------------------
cache_env <- new.env()
cache_env$api_responses <- list()
cache_env$plots <- list()

# Source application modules
source("app/R/cache.R")
source("app/R/data.R")
source("app/R/analysis.R")
source("app/R/photos.R")
source("app/R/player_info.R")
source("app/R/compare.R")
source("app/R/api_helpers.R")

# Core configuration used by tests
GITHUB_DATA_URL <<- "./"
MLB_PHOTO_BASE_URL <<- "https://img.mlbstatic.com/mlb-photos/image/upload/w_213,d_people:generic:headshot:silo:current.png,q_auto:best,f_auto/v1/people/"
FANGRAPHS_PHOTO_BASE_URL <<- "https://www.fangraphs.com/img/players/"
CURRENT_YEAR <<- 2025

# Execute tests --------------------------------------------------------------
cat("Running test suite...\n")
results <- test_dir("tests/testthat", reporter = "summary")

if (any(results$failed > 0 | results$error > 0)) {
  quit(status = 1)
}
