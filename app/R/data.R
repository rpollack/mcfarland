# Data Loading Functions ----------------------------------------------------

# Global cache for baseball data
.baseball_data_cache <- NULL
.cache_timestamp <- NULL

# Cached data loading function
load_baseball_data_cached <- function() {
  # Check if cache exists and is fresh (within 1 hour)
  if (!is.null(.baseball_data_cache) &&
    !is.null(.cache_timestamp) &&
    (Sys.time() - .cache_timestamp) < 3600) {
    cat("✓ Using cached baseball data\n")
    return(.baseball_data_cache)
  }

  # Load fresh data
  cat("Loading fresh baseball data...\n")
  start_time <- Sys.time()

  fresh_data <- tryCatch(
    {
      load_baseball_data() # Your existing function
    },
    error = function(e) {
      cat("Error loading data:", e$message, "\n")
      # Return empty but valid structure
      list(
        hitters = tibble(),
        pitchers = tibble(),
        lookup = tibble(display_name = character(), PlayerId = character(), player_type = character())
      )
    }
  )

  load_time <- Sys.time() - start_time
  cat("✓ Data loaded in", round(as.numeric(load_time), 1), "seconds\n")

  # Cache the data
  .baseball_data_cache <<- fresh_data
  .cache_timestamp <<- Sys.time()

  return(fresh_data)
}

#' Load baseball data from GitHub repository using tidyverse
#' @return List containing hitters, pitchers, and lookup data frames
load_baseball_data <- function() {
  cat("Loading baseball data from GitHub...\n")

  tryCatch(
    {
      # Define data files to load
      data_files <- c(
        hitters = "full_stats_hitters.csv",
        pitchers = "full_stats_pitchers.csv",
        lookup = "player_lookup.csv"
      )

      # Use map to load all datasets efficiently
      data_list <- data_files %>%
        map(~ {
          url <- str_glue("{GITHUB_DATA_URL}{.x}")
          cat("Fetching:", url, "\n")
          read_csv(url, show_col_types = FALSE)
        })

      cat(
        "Successfully loaded from GitHub:",
        nrow(data_list$hitters), "hitters,",
        nrow(data_list$pitchers), "pitchers\n"
      )

      data_list
    },
    error = function(e) {
      cat("Error loading from GitHub:", e$message, "\n")
      # Return empty but valid structure
      list(
        hitters = tibble(),
        pitchers = tibble(),
        lookup = tibble(display_name = character(), PlayerId = character(), player_type = character())
      )
    }
  )
}

