# ==============================================================================
# McFARLAND: Machine-crafted Forecasting And Reasoning for
#            Luck, Analytics, Narratives, and Data
#
# A professional baseball analytics application with AI-powered analysis
# TIDYVERSE VERSION - Enhanced readability and maintainability + API CACHING
# ==============================================================================

cat("=== MCFARLAND APP STARTING ===\n")
cat("Timestamp:", as.character(Sys.time()), "\n")
cat("Working directory:", getwd(), "\n")
cat("R version:", R.version.string, "\n")

# RENDER.COM COMPATIBILITY CONFIGURATION
if (Sys.getenv("RENDER") == "true") {
  # Running on Render - options are set by the CMD in Dockerfile
  options(shiny.sanitize.errors = FALSE)
  options(shiny.trace = TRUE) # Enable tracing to see errors
  cat("✓ Render config applied (host/port set by CMD)\n")
} else {
  # Local development
  options(shiny.autoreload = TRUE)
  cat("✓ Development mode\n")
}

# Keep-alive options
options(
  shiny.disconnected.timeout = 300000, # 5 minutes in milliseconds
  shiny.autoreload.interval = 500, # Check every 500ms
  shiny.websocket.ping.interval = 25000 # 25 seconds
)

cat("=== CHECKING ENVIRONMENT ===\n")
cat("RENDER env var:", Sys.getenv("RENDER"), "\n")
cat("OPENAI_API_KEY set:", nchar(Sys.getenv("OPENAI_API_KEY")) > 0, "\n")

cat("=== LOADING LIBRARIES ===\n")

# Load libraries with error handling
tryCatch(
  {
    library(shiny)
    cat("✓ shiny loaded\n")
  },
  error = function(e) {
    cat("✗ shiny FAILED:", e$message, "\n")
    stop("Cannot load shiny package")
  }
)

tryCatch(
  {
    library(dplyr)
    cat("✓ dplyr loaded\n")
  },
  error = function(e) {
    cat("✗ dplyr FAILED:", e$message, "\n")
    stop("Cannot load dplyr package")
  }
)

# Load other essential packages
for (pkg in c(
  "readr", "purrr", "stringr", "httr", "jsonlite",
  "bslib", "commonmark", "shinybusy", "shinyWidgets", "ggplot2", "htmltools", "digest"
)) {
  tryCatch(
    {
      library(pkg, character.only = TRUE)
      cat("✓", pkg, "loaded\n")
    },
    error = function(e) {
      cat("✗", pkg, "FAILED:", e$message, "\n")
      stop(paste("Cannot load", pkg, "package"))
    }
  )
}

# Load baseball-specific packages (these might fail)
for (pkg in c("baseballr", "stringi", "janitor")) {
  tryCatch(
    {
      library(pkg, character.only = TRUE)
      cat("✓", pkg, "loaded\n")
    },
    error = function(e) {
      cat("⚠", pkg, "FAILED:", e$message, "\n")
      cat("Continuing without", pkg, "\n")
    }
  )
}

cat("=== LIBRARIES LOADED ===\n")

# Load Required Libraries --------------------------------------------------
library(shiny) # Core Shiny framework
library(dplyr) # Data manipulation (filter, select, case_when, etc.)
library(readr) # Reading CSV files (read_csv)
library(purrr) # Functional programming (map, compact, %||%)
library(stringr) # String manipulation (str_glue, str_detect, etc.)
library(httr) # HTTP requests for OpenAI API
library(jsonlite) # JSON handling for API calls
library(bslib) # Bootstrap theming (page_navbar, card, etc.)
library(commonmark) # Markdown rendering
library(shinybusy) # Loading indicators
library(shinyWidgets) # Enhanced select inputs
library(ggplot2) # Data visualization
library(htmltools) # HTML utilities (HTML, htmlEscape)
library(digest) # CACHE: Added for generating cache keys
library(DBI) # Database interface
library(RSQLite) # SQLite (for development)
library(digest) # For hashing (if using PostgreSQL later)
library(uuid) # For generating user IDs

# Load analytics
tryCatch(
  {
    source("admin.R")
    source("analytics.R")
  },
  error = function(e) {
    # Create dummy functions if analytics fails
    is_admin <- function(session) FALSE
    log_if_not_admin <- function(session, log_function, ...) {}
    init_analytics_db <- function() {}
    generate_user_id <- function(session) "dummy"
    track_user <- function(...) {}
  }
)

# Initialize database
if (exists("init_analytics_db")) {
  init_analytics_db()
}

# Tidyverse-style console logger (no session)
log_analysis_console <- function(player_name, analysis_mode, ...) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  message_parts <- c(
    if (!is.null(player_name)) str_glue("Player = {player_name}"),
    if (!is.null(analysis_mode)) str_glue("Mode = {analysis_mode}"),
    ...
  ) %>%
    compact() %>%
    str_c(collapse = " | ")

  cat("[", timestamp, "] 🎯 ANALYSIS:", message_parts, "\n")
}

# CACHE: Create global environment for API response caching
cache_env <- new.env()
cache_env$api_responses <- list()
cache_env$plots <- list()

# Configuration -------------------------------------------------------------
GITHUB_DATA_URL <- "https://raw.githubusercontent.com/rpollack/mcfarland/master/"
MLB_PHOTO_BASE_URL <- "https://img.mlbstatic.com/mlb-photos/image/upload/w_213,d_people:generic:headshot:silo:current.png,q_auto:best,f_auto/v1/people/"
FANGRAPHS_PHOTO_BASE_URL <- "https://www.fangraphs.com/img/players/"
CURRENT_YEAR <- 2025

