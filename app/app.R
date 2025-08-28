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
  "bslib", "commonmark", "shinybusy", "ggplot2", "htmltools", "digest"
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

# Tidyverse-style analysis logger
log_analysis <- function(player_name, analysis_mode, ...) {
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

# CACHE: API Response Caching Functions ------------------------------------

#' Generate cache key for API responses
#' @param prompt_text Analysis prompt
#' @param analysis_mode Analysis style
#' @return MD5 hash for caching
generate_cache_key <- function(prompt_text, analysis_mode) {
  digest::digest(paste(prompt_text, analysis_mode), algo = "md5")
}

#' Save API response to memory cache
#' @param cache_key Unique identifier
#' @param result API response
#' @param max_cache_size Maximum number of cached responses (default 50)
save_api_response <- function(cache_key, result, max_cache_size = 50) {
  cache_entry <- list(
    result = result,
    timestamp = Sys.time()
  )

  cache_env$api_responses[[cache_key]] <- cache_entry

  # Simple cache size management - keep newest entries
  if (length(cache_env$api_responses) > max_cache_size) {
    timestamps <- map_dbl(cache_env$api_responses, ~ as.numeric(.x$timestamp))
    # Keep the 25 newest entries
    newest_indices <- order(timestamps, decreasing = TRUE)[1:25]
    newest_keys <- names(cache_env$api_responses)[newest_indices]
    old_cache <- cache_env$api_responses
    cache_env$api_responses <- list()
    for (key in newest_keys) {
      cache_env$api_responses[[key]] <- old_cache[[key]]
    }
    cat("Cache trimmed to", length(cache_env$api_responses), "entries\n")
  }

  cat("✓ Cached API response:", substr(cache_key, 1, 8), "... (Total:", length(cache_env$api_responses), ")\n")
}

#' Load API response from memory cache
#' @param cache_key Unique identifier
#' @return Cached result or NULL
load_api_response <- function(cache_key) {
  cached_entry <- cache_env$api_responses[[cache_key]]

  if (is.null(cached_entry)) {
    return(NULL)
  }

  # Check if cache is too old (1 hour expiry)
  cache_age_hours <- as.numeric(difftime(Sys.time(), cached_entry$timestamp, units = "hours"))
  if (cache_age_hours > 1) {
    cache_env$api_responses[[cache_key]] <- NULL
    cat("⏰ Expired cache entry removed:", substr(cache_key, 1, 8), "...\n")
    return(NULL)
  }

  cat("⚡ Cache hit:", substr(cache_key, 1, 8), "...\n")
  return(cached_entry$result)
}

#' Get cache statistics
get_cache_stats <- function() {
  api_count <- length(cache_env$api_responses)
  plot_count <- length(cache_env$plots)

  list(
    api_responses = api_count,
    plots = plot_count,
    total_items = api_count + plot_count
  )
}

#' Clear all caches
clear_all_caches <- function() {
  api_count <- length(cache_env$api_responses)
  plot_count <- length(cache_env$plots)

  cache_env$api_responses <- list()
  cache_env$plots <- list()

  cat("🗑️ Cleared", api_count, "API responses and", plot_count, "plots from cache\n")
}

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

# Photo Management Functions -----------------------------------------------

#' Get MLB headshot URL for a player using tidyverse validation
#' @param mlb_id MLB ID number
#' @return Complete MLB photo URL or NULL
build_mlb_photo_url <- function(mlb_id) {
  if (is.null(mlb_id) || is.na(mlb_id) || mlb_id == "" || mlb_id == 0) {
    return(NULL)
  }
  str_glue("{MLB_PHOTO_BASE_URL}{mlb_id}/headshot/67/current")
}

#' Extract PlayerId from compound ID or return as-is
#' @param compound_id Either a compound ID like "12345_hitter" or a simple PlayerId
#' @return PlayerId as string
extract_player_id <- function(compound_id) {
  if (str_detect(compound_id, "_")) {
    str_split(compound_id, "_")[[1]][1]
  } else {
    compound_id
  }
}

#' Get player's MLB ID from the data using tidyverse
#' @param player_id FanGraphs player ID (can be compound or simple)
#' @param baseball_data Complete baseball data list
#' @return MLB ID if found, NULL otherwise
get_player_mlb_id <- function(player_id, baseball_data) {
  # Handle both compound IDs and backwards compatibility
  player_info <- if ("compound_id" %in% colnames(baseball_data$lookup)) {
    baseball_data$lookup %>% filter(compound_id == player_id)
  } else {
    baseball_data$lookup %>% filter(PlayerId == player_id)
  }

  if (nrow(player_info) == 0) {
    return(NULL)
  }

  # Extract actual player ID and determine dataset
  actual_player_id <- extract_player_id(player_id)
  dataset <- if (player_info$player_type == "hitter") {
    baseball_data$hitters
  } else {
    baseball_data$pitchers
  }

  if (nrow(dataset) == 0) {
    return(NULL)
  }

  # Get MLB ID using tidyverse
  player_data <- dataset %>%
    filter(PlayerId == actual_player_id) %>%
    slice_head(n = 1)

  if (nrow(player_data) == 0 || !"mlbamid" %in% colnames(player_data)) {
    return(NULL)
  }

  mlb_id <- player_data$mlbamid
  if (is.na(mlb_id) || mlb_id == "" || mlb_id == 0) NULL else mlb_id
}

#' Generate complete photo URL with fallback strategy
#' @param player_id FanGraphs player ID
#' @param baseball_data Complete baseball data list
#' @return Photo URL (MLB, FanGraphs, or placeholder)
get_player_photo_url <- function(player_id, baseball_data) {
  # Try MLB photo first
  mlb_id <- get_player_mlb_id(player_id, baseball_data)
  mlb_url <- build_mlb_photo_url(mlb_id)

  # Return MLB URL if available, otherwise fallback to FanGraphs
  mlb_url %||% str_glue("{FANGRAPHS_PHOTO_BASE_URL}{player_id}.jpg")
}

# Player Information Functions ---------------------------------------------

#' Get player basic information using tidyverse
#' @param player_id FanGraphs player ID (can be compound or simple)
#' @param baseball_data Complete baseball data list
#' @return List with name, type, age, position info
get_player_info <- function(player_id, baseball_data) {
  # Handle both compound IDs and backwards compatibility
  player_lookup <- if ("compound_id" %in% colnames(baseball_data$lookup)) {
    baseball_data$lookup %>% filter(compound_id == player_id)
  } else {
    baseball_data$lookup %>% filter(PlayerId == player_id)
  }

  if (nrow(player_lookup) == 0) {
    return(NULL)
  }

  # Extract key information
  player_name <- player_lookup$Name
  player_type <- player_lookup$player_type
  actual_player_id <- if ("compound_id" %in% colnames(baseball_data$lookup)) {
    extract_player_id(player_id)
  } else {
    player_id
  }

  # Get detailed info based on type using tidyverse
  if (player_type == "hitter" && nrow(baseball_data$hitters) > 0) {
    player_data <- baseball_data$hitters %>%
      filter(PlayerId == actual_player_id) %>%
      slice_head(n = 1)

    if (nrow(player_data) > 0) {
      return(list(
        name = player_name,
        type = player_type,
        age = player_data$Age,
        position_info = "Hitter"
      ))
    }
  } else if (player_type == "pitcher" && nrow(baseball_data$pitchers) > 0) {
    player_data <- baseball_data$pitchers %>%
      filter(PlayerId == actual_player_id) %>%
      slice_head(n = 1)

    if (nrow(player_data) > 0) {
      position_detail <- if ("position" %in% colnames(player_data)) {
        str_glue("{player_data$position} • Pitcher")
      } else {
        "Pitcher"
      }

      return(list(
        name = player_name,
        type = player_type,
        age = player_data$Age,
        position_info = position_detail
      ))
    }
  }

  # Fallback
  list(name = player_name, type = player_type, age = NA, position_info = "")
}

# UI Component Functions ---------------------------------------------------

#' Create player information card with photo
#' @param player_id FanGraphs player ID
#' @param baseball_data Complete baseball data list
#' @return Shiny div element
create_player_card <- function(player_id, baseball_data) {
  player_info <- get_player_info(player_id, baseball_data)
  if (is.null(player_info)) {
    return(NULL)
  }

  photo_url <- get_player_photo_url(player_id, baseball_data)
  fallback_url <- "https://via.placeholder.com/200x200/2E86AB/ffffff?text=⚾"

  # Build player details text using tidyverse approach
  details_parts <- c(
    if (!is.na(player_info$age)) str_glue("Age: {player_info$age}"),
    if (player_info$position_info != "") str_glue("• {player_info$position_info}")
  ) %>%
    compact() %>%
    str_c(collapse = " ")

  div(
    class = "text-center player-info-card",
    img(
      src = photo_url,
      alt = str_glue("Photo of {player_info$name}"),
      class = "player-photo",
      onerror = str_glue("this.onerror=null; this.src='{fallback_url}';")
    ),
    h4(player_info$name, class = "player-name"),
    if (details_parts != "") {
      p(class = "player-details", details_parts)
    }
  )
}

# Visualization Functions --------------------------------------------------

#' Get metrics for comparison plots based on player type
#' @param player_type Either "hitter" or "pitcher"
#' @return Vector of metric names
get_comparison_metrics <- function(player_type) {
  metrics_list <- list(
    hitter = c("AVG", "OBP", "SLG", "K_pct", "BB_pct", "Barrel_pct", "BABIP", "wOBA", "xwOBA"),
    pitcher = c("era", "xera", "k_percent", "bb_percent", "k_minus_bb_percent", "barrel_percent", "babip", "lob_percent")
  )

  metrics_list[[player_type]] %||% character(0)
}

#' Clean metric names for display using tidyverse
#' @param metrics Vector of metric names
#' @param player_type Either "hitter" or "pitcher"
#' @return Vector of cleaned metric names
clean_metric_names <- function(metrics, player_type) {
  if (player_type == "hitter") {
    recode(metrics,
      K_pct = "K%",
      BB_pct = "BB%",
      Barrel_pct = "Barrel%",
      .default = metrics
    )
  } else {
    recode(metrics,
      era = "ERA",
      xera = "xERA",
      k_percent = "K%",
      bb_percent = "BB%",
      k_minus_bb_percent = "K-BB%",
      barrel_percent = "Barrel%",
      babip = "BABIP",
      lob_percent = "LOB%",
      .default = metrics
    )
  }
}

#' Create comparison plot for player trends using tidyverse
#' @param player_id FanGraphs player ID (can be compound or simple)
#' @param baseball_data Complete baseball data list
#' @return ggplot object or NULL
create_player_trends_plot <- function(player_id, baseball_data) {
  player_info <- get_player_info(player_id, baseball_data)
  if (is.null(player_info)) {
    return(NULL)
  }

  # Get appropriate dataset
  dataset <- if (player_info$type == "hitter") {
    baseball_data$hitters
  } else {
    baseball_data$pitchers
  }

  if (nrow(dataset) == 0) {
    return(NULL)
  }

  # Extract actual player ID
  actual_player_id <- if ("compound_id" %in% colnames(baseball_data$lookup)) {
    extract_player_id(player_id)
  } else {
    player_id
  }

  player_data <- dataset %>% filter(PlayerId == actual_player_id)
  if (nrow(player_data) == 0) {
    return(NULL)
  }

  # Build comparison data using tidyverse
  metrics <- get_comparison_metrics(player_info$type)

  comparison_data <- metrics %>%
    map_dfr(~ {
      current_col <- str_glue("{.x}_cur")
      avg_col <- str_glue("{.x}_l3")

      if (all(c(current_col, avg_col) %in% colnames(player_data))) {
        current_val <- player_data[[current_col]]
        avg_val <- player_data[[avg_col]]

        if (!is.na(current_val) && !is.na(avg_val)) {
          tibble(
            player = player_info$name,
            metric = .x,
            period = c("Past 3 years", "2025"),
            value = c(avg_val, current_val)
          )
        }
      }
    })

  if (nrow(comparison_data) == 0) {
    return(NULL)
  }

  # Clean metric names and create plot
  comparison_data <- comparison_data %>%
    mutate(
      metric_display = clean_metric_names(metric, player_info$type),
      period = factor(period, levels = c("Past 3 years", "2025"))
    )

  ggplot(comparison_data, aes(x = period, y = value, group = metric_display)) +
    geom_point(size = 3, color = "#2E86AB") +
    geom_line(color = "#2E86AB") +
    facet_wrap(~metric_display, scales = "free_y") +
    labs(
      title = str_glue("Trends: {player_info$name}"),
      x = "",
      y = "Value"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      strip.text = element_text(face = "bold", size = 10),
      plot.title = element_text(size = 14, face = "bold")
    ) +
    expand_limits(y = 0)
}

# Analysis Functions --------------------------------------------------------

#' Safe value formatting for display using tidyverse
#' @param x Numeric value to format
#' @return Formatted string
format_stat_value <- function(x) {
  case_when(
    is.na(x) | is.null(x) ~ "N/A",
    is.numeric(x) ~ as.character(round(x, 3)),
    TRUE ~ as.character(x)
  )
}

#' Get analysis persona prompt using tidyverse
#' @param mode Analysis mode string
#' @return Persona prompt text
get_analysis_persona <- function(mode) {
  personas <- list(
    analytics_dork = "You are a front office nerd, raised on moneyball and new school stats, always at the cutting edge. You favor new school stats, talk in probabilities, and are very dismissive of people who don't believe you. You might be the smartest person in the room, but people would describe you as a real tool. Be ruthless and dismissive!",
    old_coot = "You are a deranged old coot, ranting and raving about everything. Yell a lot. People would describe you as 'off your meds'. Throw in references to people spying on you. Appear confused at times. Get stats wrong occasionally. You know, just -- be insane.",
    gen_z = "You're an over the top Gen Z'er, using lots of slang, referencing hyper modern trends, apps, emojis, and such. But really lay it on thick, in a humorously over-the-top kind of way.",
    seventies = "You prefer 1970s style of baseball, when men were men, stolen bases were high, starting pitchers completed every game, and guys had bushy mustaches and chewed tobacco all game. You strongly prefer old school stats to new school ones. Use lots of comparisons to famous 1970s baseball players: Pete Rose, Johnny Bench, Mike Schmidt, Willie Stargell, Rod Carew, Bobby Grich, Thurman Munson, etc -- but don't limit your comparisons to just these guys.",
    sensationalist = "You report baseball analysis like a carnival barker in the jazz age: always trying to make things larger than life through flowery prose and colorful headlines. You practice sensationalist, ballyhoo sportswriting and yellow-journalism-style copy. Every flaw is a titanic tragedy, and every positive is a starry-eyed bright and shiny future.",
    shakespeare = "You are William Shakespeare. Not just that, but you speak in verse -- preferably iambic pentameter.",
    rose_colored_glasses = "You always find the positive. Cherry pick analysis and narratvies that accentuate the positive trends of the player, even if it means overlooking negative aspects or signs."
  )

  personas[[mode]] %||% "Keep it simple and easy to understand. Use short but friendly sentences. Don't start with asides or extraneous clauses. Start your response with the conclusion/summary takeaways, then underneath, list your evidence for that summary and those conclusions."
}

#' Call OpenAI API for analysis with caching
#' @param prompt_text Analysis prompt
#' @param analysis_mode Analysis style mode
#' @return HTML formatted response
call_openai_api <- function(prompt_text, analysis_mode) {
  # CACHE: Generate cache key and check cache first
  cache_key <- generate_cache_key(prompt_text, analysis_mode)

  # Try to load from cache
  cached_result <- load_api_response(cache_key)
  if (!is.null(cached_result)) {
    return(cached_result)
  }

  # Cache miss - proceed with API call
  cat("🌐 Making OpenAI API call (cache miss)\n")

  api_key <- Sys.getenv("OPENAI_API_KEY")

  if (api_key == "") {
    result <- HTML(str_glue(
      "<div class='alert alert-info'>",
      "<h5>OpenAI API Key Not Set</h5>",
      "<p>Set OPENAI_API_KEY environment variable to enable analysis.</p>",
      "<details><summary>View prompt</summary><pre>{htmlEscape(prompt_text)}</pre></details>",
      "</div>"
    ))
    return(result)
  }

  persona_prompt <- get_analysis_persona(analysis_mode)

  full_prompt <- str_glue(
    "Here is current-year performance data for a player:\n\n{prompt_text}\n\n",
    "General instructions:\n\n",
    "Please analyze how the player is performing this year, what trends stand out, and whether any aspects of the performance appear to be skill- or luck-driven. Incorporate a prediction: will the player improve, decline, or stay the same for the rest of the season? Explain your reasoning.\n\n",
    "The very first element of the response should be a title that encompasses your findings.\n\n",
    "Your analysis must incorporate metric, direction, and magnitude of difference. For example BB% is up, indicate by how much, and what the size of that gap might indicate. You don't need to explicitly call out this framing (e.g. in bullets), just make sure to weave it into your analysis.\n\n",
    "Separate your analysis into core skills and luck/regression indicators.\n\n",
    "Don't repeat yourself. For example, if you say a stat or performance or trend is 'lucky', you don't need to say it's 'not unlucky'.\n\n",
    "Remember that when it comes to stats and trends, you only have knowledge of two things: a player's current-year stats and the average of the same stats for the past 3 years (e.g. not their entire career). So when you say things like a stat is 'up' or 'down', make it clear that this is relative to the last 3 years' average.\n\n",
    "Here is your persona that should inform your writing style and response, even if it means overriding those previous instructions: {persona_prompt}"
  )

  result <- tryCatch(
    {
      response <- POST(
        "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = str_glue("Bearer {api_key}")),
        content_type_json(),
        body = toJSON(list(
          model = "gpt-4.1",
          messages = list(
            list(role = "system", content = "You are a sabermetric baseball analyst tasked with understanding both current in-season performance and getting a sense of future performance for the rest of the season. You need to communicate to a technical audience, but also to the lay audience who just wants to know how excited or concerned they should be about this player."),
            list(role = "user", content = full_prompt)
          ),
          temperature = 0.7
        ), auto_unbox = TRUE)
      )

      parsed_response <- content(response, as = "parsed")

      if (!is.null(parsed_response$error)) {
        return(HTML(str_glue("<div class='alert alert-danger'>API Error: {parsed_response$error$message}</div>")))
      }

      if (is.null(parsed_response$choices) || length(parsed_response$choices) == 0) {
        return(HTML("<div class='alert alert-warning'>No response from API.</div>"))
      }

      analysis_text <- parsed_response$choices[[1]]$message$content
      HTML(commonmark::markdown_html(analysis_text))
    },
    error = function(e) {
      HTML(str_glue("<div class='alert alert-danger'>Error: {e$message}</div>"))
    }
  )

  # CACHE: Save successful results to cache
  if (!is.null(result) && !str_detect(as.character(result), "alert-danger")) {
    save_api_response(cache_key, result)
  }

  return(result)
}

#' Build hitter analysis prompt using tidyverse
#' @param player_name Player name
#' @param hitter_data Hitter statistics data frame
#' @return Analysis prompt text
build_hitter_prompt <- function(player_name, hitter_data) {
  data <- hitter_data %>% filter(Name == player_name)
  if (nrow(data) == 0) {
    return(NULL)
  }

  # Build prompt sections using tidyverse string manipulation
  header_section <- c(
    str_glue("Player: {player_name} (Hitter)"),
    "",
    "--- Key metrics to analyze---",
    str_glue("Age: {format_stat_value(data$Age)}"),
    str_glue("Year: {CURRENT_YEAR}"),
    str_glue("Plate Appearances (PA): {format_stat_value(data$PA_cur)}")
  )

  metrics_section <- c(
    str_glue("AVG: {format_stat_value(data$AVG_cur)}  Last 3 Years: {format_stat_value(data$AVG_l3)}  Diff: {format_stat_value(data$AVG_diff)}"),
    str_glue("OBP: {format_stat_value(data$OBP_cur)}  Last 3 Years: {format_stat_value(data$OBP_l3)}  Diff: {format_stat_value(data$OBP_diff)}"),
    str_glue("SLG: {format_stat_value(data$SLG_cur)}  Last 3 Years: {format_stat_value(data$SLG_l3)}  Diff: {format_stat_value(data$SLG_diff)}"),
    str_glue("K%: {format_stat_value(data$K_pct_cur)}  Last 3 Years: {format_stat_value(data$K_pct_l3)}  Diff: {format_stat_value(data$K_pct_diff)}"),
    str_glue("BB%: {format_stat_value(data$BB_pct_cur)}  Last 3 Years: {format_stat_value(data$BB_pct_l3)}  Diff: {format_stat_value(data$BB_pct_diff)}"),
    str_glue("Barrel%: {format_stat_value(data$Barrel_pct_cur)}  Last 3 Years: {format_stat_value(data$Barrel_pct_l3)}  Diff: {format_stat_value(data$Barrel_pct_diff)}"),
    str_glue("BABIP: {format_stat_value(data$BABIP_cur)}  Last 3 Years: {format_stat_value(data$BABIP_l3)}  Diff: {format_stat_value(data$BABIP_diff)}"),
    str_glue("wOBA: {format_stat_value(data$wOBA_cur)}  Last 3 Years: {format_stat_value(data$wOBA_l3)}  Diff: {format_stat_value(data$wOBA_diff)}"),
    str_glue("xwOBA: {format_stat_value(data$xwOBA_cur)}  Last 3 Years: {format_stat_value(data$xwOBA_l3)}  Diff: {format_stat_value(data$xwOBA_diff)}"),
    str_glue("xwOBA-wOBA gap: {format_stat_value(data$xwOBA_wOBA_gap_cur)}  Last 3 Years: {format_stat_value(data$xwOBA_wOBA_gap_l3)}  Diff: {format_stat_value(data$xwOBA_wOBA_gap_diff)}")
  )

  notes_section <- c(
    "",
    "--- Notes for analysis ---",
    "- Focus on current-year performance compared to the last three years explanations.",
    "- BABIP above/below norms indicates luck.",
    "- Gaps between wOBA and xwOBA signal luck vs skill, unless that gap is close to what it has been historically.",
    "- Remember that xwOBA includes contact quality and plate discipline.",
    "- BB%/K% changes reflect plate discipline skills, which are more sustainable than batted-ball performance generally.",
    "- Take age into account. Older players less likely to improve; younger trend upward. Players generally peak in their early to mid 20's now.",
    "- High Barrel% indicates the player is hitting the ball hard at ideal launch angles. Changes indicate legitimate improvements or declines. Higher Barrel% should mean higher BABIP, higher wOBA, and higher xwOBA -- unless bad luck is a significant factor.",
    "- Incorporate injuries or known context.",
    "- For small samples, be cautious with conclusions. For context, larger samples trend towards hundreds of PA. A full season is ~600 PA."
  )

  c(header_section, metrics_section, notes_section) %>% str_c(collapse = "\n")
}

#' Build pitcher analysis prompt using tidyverse
#' @param player_name Player name
#' @param pitcher_data Pitcher statistics data frame
#' @return Analysis prompt text
build_pitcher_prompt <- function(player_name, pitcher_data) {
  data <- pitcher_data %>% filter(Name == player_name)
  if (nrow(data) == 0) {
    return(NULL)
  }

  # Build prompt sections using tidyverse string manipulation
  header_section <- c(
    str_glue("Player: {player_name} (Pitcher)"),
    "",
    "--- Key metrics to analyze---",
    str_glue("Age: {format_stat_value(data$Age)}"),
    str_glue("Year: {CURRENT_YEAR}"),
    str_glue("Position: {if_else('position' %in% names(data), as.character(data$position), 'Pitcher')}"),
    str_glue("Total Batters Faced: {format_stat_value(data$tbf)}"),
    ""
  )

  metrics_section <- c(
    str_glue("ERA: {format_stat_value(data$era_cur)}  Last 3 Years: {format_stat_value(data$era_l3)}  Diff: {format_stat_value(data$era_diff)}"),
    str_glue("xERA: {format_stat_value(data$xera_cur)}  Last 3 Years: {format_stat_value(data$xera_l3)}  Diff: {format_stat_value(data$xera_diff)}"),
    str_glue("BABIP: {format_stat_value(data$babip_cur)}  Last 3 Years: {format_stat_value(data$babip_l3)}  Diff: {format_stat_value(data$babip_diff)}"),
    str_glue("Barrel Rate: {format_stat_value(data$barrel_percent_cur)}%  Last 3 Years: {format_stat_value(data$barrel_percent_l3)}%  Diff: {format_stat_value(data$barrel_percent_diff)}%"),
    str_glue("Strikeout Rate (K%): {format_stat_value(data$k_percent_cur)}%  Last 3 Years: {format_stat_value(data$k_percent_l3)}%  Diff: {format_stat_value(data$k_percent_diff)}%"),
    str_glue("Called Strike & Whiff Rate (CSW%): {format_stat_value(data$csw_percent_cur)}%  Last 3 Years: {format_stat_value(data$csw_percent_l3)}%  Diff: {format_stat_value(data$csw_percent_diff)}%"),
    str_glue("Outside Zone Swing Rate (O-Swing%): {format_stat_value(data$o_swing_percent_cur)}%  Last 3 Years: {format_stat_value(data$o_swing_percent_l3)}%  Diff: {format_stat_value(data$o_swing_percent_diff)}%"),
    str_glue("Walk Rate (BB%): {format_stat_value(data$bb_percent_cur)}%  Last 3 Years: {format_stat_value(data$bb_percent_l3)}%  Diff: {format_stat_value(data$bb_percent_diff)}%"),
    str_glue("K-BB%: {format_stat_value(data$k_minus_bb_percent_cur)}%  Last 3 Years: {format_stat_value(data$k_minus_bb_percent_l3)}%  Diff: {format_stat_value(data$k_minus_bb_percent_diff)}%"),
    str_glue("LOB% (Left-on-base rate): {format_stat_value(data$lob_percent_cur)}%  Last 3 Years: {format_stat_value(data$lob_percent_l3)}%  Diff: {format_stat_value(data$lob_percent_diff)}%")
  )

  notes_section <- c(
    "",
    "--- Guidelines for analysis ---",
    "Core skill indicators are strikeout rate, walk rate, and K-BB%. Fluctuations here are more indicative of core skillset changes than luck. Barrel Rate is another core skill indicator. Smaller numbers here indicate weaker contact against the pitcher and thus a more believable BABIP.",
    "",
    "Low CSW% but high K% indicates a luck issue or potential negative regression in the future.",
    "",
    "Similarly, low O-Swing% but high BB% indicates fewer 'deserved' walks, hinting at negative regression in this stat. (The opposite is true.)",
    "",
    "Your primary indicators of luck (good or bad) are BABIP and LOB%. Use these in your analysis. That said, high-strikeout pitchers tend to have higher LOB%. Gap between ERA and xERA (expected ERA) also indicates luck.",
    "",
    "Consider the player's sample size (TBF: total batters faced) and their position (RP: reliever, SP: starter.) Relievers are more volatile because they pitch in smaller samples. Starters with fewer batters faced are more prone to the same volatility. A typical starter will face 700 or so batters in a season. As the pitcher's TBF moves up towards this number, the analysis can become more stable/concrete.",
    "",
    "Take age into account. Give younger players more grace and older players less."
  )

  c(header_section, metrics_section, notes_section) %>% str_c(collapse = "\n")
}

#' Analyze hitter performance using tidyverse
#' @param player_name Player name
#' @param analysis_mode Analysis style mode
#' @param hitter_data Hitter statistics data frame
#' @return HTML analysis result
analyze_hitter_performance <- function(player_name, analysis_mode, hitter_data) {
  if (nrow(hitter_data %>% filter(Name == player_name)) == 0) {
    return(HTML(str_glue("<div class='alert alert-warning'>Hitter not found: {player_name}</div>")))
  }

  prompt <- build_hitter_prompt(player_name, hitter_data)
  if (is.null(prompt)) {
    return(HTML(str_glue("<div class='alert alert-warning'>Unable to build analysis for: {player_name}</div>")))
  }

  call_openai_api(prompt, analysis_mode)
}

#' Analyze pitcher performance using tidyverse
#' @param player_name Player name
#' @param analysis_mode Analysis style mode
#' @param pitcher_data Pitcher statistics data frame
#' @return HTML analysis result
analyze_pitcher_performance <- function(player_name, analysis_mode, pitcher_data) {
  if (nrow(pitcher_data %>% filter(Name == player_name)) == 0) {
    return(HTML(str_glue("<div class='alert alert-warning'>Pitcher not found: {player_name}</div>")))
  }

  prompt <- build_pitcher_prompt(player_name, pitcher_data)
  if (is.null(prompt)) {
    return(HTML(str_glue("<div class='alert alert-warning'>Unable to build analysis for: {player_name}</div>")))
  }

  call_openai_api(prompt, analysis_mode)
}

# Improved generate_quick_insight with better error handling
generate_quick_insight <- function(player_data, player_type) {
  # Safety checks
  if (is.null(player_data) || nrow(player_data) == 0) {
    return("Player data loaded and ready for analysis.")
  }

  if (is.null(player_type) || !player_type %in% c("hitter", "pitcher")) {
    return("Player statistics available for detailed analysis.")
  }

  # Wrap the analysis logic in tryCatch for safety
  tryCatch(
    {
      positive_changes <- c()
      negative_changes <- c()

      if (player_type == "hitter") {
        # Positive changes for hitters - with proper NA checks
        if (!is.na(player_data$AVG_diff) && is.numeric(player_data$AVG_diff) && player_data$AVG_diff > 0.02) {
          positive_changes <- c(positive_changes, "batting average is up")
        }
        if (!is.na(player_data$K_pct_diff) && is.numeric(player_data$K_pct_diff) && player_data$K_pct_diff < -2) {
          positive_changes <- c(positive_changes, "strikeouts are down")
        }
        if (!is.na(player_data$BB_pct_diff) && is.numeric(player_data$BB_pct_diff) && player_data$BB_pct_diff > 1.5) {
          positive_changes <- c(positive_changes, "walks are up")
        }
        if (!is.na(player_data$Barrel_pct_diff) && is.numeric(player_data$Barrel_pct_diff) && player_data$Barrel_pct_diff > 2) {
          positive_changes <- c(positive_changes, "hard contact is up")
        }

        # Negative changes for hitters - with proper NA checks
        if (!is.na(player_data$AVG_diff) && is.numeric(player_data$AVG_diff) && player_data$AVG_diff < -0.02) {
          negative_changes <- c(negative_changes, "batting average is down")
        }
        if (!is.na(player_data$K_pct_diff) && is.numeric(player_data$K_pct_diff) && player_data$K_pct_diff > 2) {
          negative_changes <- c(negative_changes, "strikeouts are up")
        }
        if (!is.na(player_data$BB_pct_diff) && is.numeric(player_data$BB_pct_diff) && player_data$BB_pct_diff < -1.5) {
          negative_changes <- c(negative_changes, "walks are down")
        }
        if (!is.na(player_data$Barrel_pct_diff) && is.numeric(player_data$Barrel_pct_diff) && player_data$Barrel_pct_diff < -2) {
          negative_changes <- c(negative_changes, "hard contact is down")
        }
      } else { # pitcher - with proper NA checks
        # Positive changes for pitchers
        if (!is.na(player_data$era_diff) && is.numeric(player_data$era_diff) && player_data$era_diff < -0.5) {
          positive_changes <- c(positive_changes, "ERA is down")
        }
        if (!is.na(player_data$k_percent_diff) && is.numeric(player_data$k_percent_diff) && player_data$k_percent_diff > 2) {
          positive_changes <- c(positive_changes, "strikeouts are up")
        }
        if (!is.na(player_data$bb_percent_diff) && is.numeric(player_data$bb_percent_diff) && player_data$bb_percent_diff < -1.5) {
          positive_changes <- c(positive_changes, "walks allowed are down")
        }
        if (!is.na(player_data$barrel_percent_diff) && is.numeric(player_data$barrel_percent_diff) && player_data$barrel_percent_diff < -1) {
          positive_changes <- c(positive_changes, "hard contact allowed is down")
        }

        # Negative changes for pitchers
        if (!is.na(player_data$era_diff) && is.numeric(player_data$era_diff) && player_data$era_diff > 0.5) {
          negative_changes <- c(negative_changes, "ERA is up")
        }
        if (!is.na(player_data$k_percent_diff) && is.numeric(player_data$k_percent_diff) && player_data$k_percent_diff < -2) {
          negative_changes <- c(negative_changes, "strikeouts are down")
        }
        if (!is.na(player_data$bb_percent_diff) && is.numeric(player_data$bb_percent_diff) && player_data$bb_percent_diff > 1.5) {
          negative_changes <- c(negative_changes, "walks allowed are up")
        }
        if (!is.na(player_data$barrel_percent_diff) && is.numeric(player_data$barrel_percent_diff) && player_data$barrel_percent_diff > 1) {
          negative_changes <- c(negative_changes, "hard contact allowed is up")
        }
      }

      # Calculate regression likelihood safely
      regression_risk <- assess_regression_likelihood(player_data, player_type)

      # Build the insight message
      performance_text <- ""
      if (length(positive_changes) > 0 && length(negative_changes) > 0) {
        # Mixed performance
        pos_text <- if (length(positive_changes) > 1) {
          str_c(positive_changes[1:min(2, length(positive_changes))], collapse = " and ")
        } else {
          positive_changes[1]
        }
        neg_text <- if (length(negative_changes) > 1) {
          str_c(negative_changes[1:min(2, length(negative_changes))], collapse = " and ")
        } else {
          negative_changes[1]
        }
        performance_text <- str_glue("Mixed performance: {pos_text}, but {neg_text}.")
      } else if (length(positive_changes) > 0) {
        # Only positive changes
        if (length(positive_changes) == 1) {
          performance_text <- str_glue("Improved performance: {positive_changes[1]}.")
        } else if (length(positive_changes) == 2) {
          performance_text <- str_glue("Improved performance: {positive_changes[1]} and {positive_changes[2]}.")
        } else {
          performance_text <- str_glue("Strong improvement: {positive_changes[1]}, {positive_changes[2]}, and more.")
        }
      } else if (length(negative_changes) > 0) {
        # Only negative changes
        if (length(negative_changes) == 1) {
          performance_text <- str_glue("Concerning trend: {negative_changes[1]}.")
        } else if (length(negative_changes) == 2) {
          performance_text <- str_glue("Concerning trends: {negative_changes[1]} and {negative_changes[2]}.")
        } else {
          performance_text <- str_glue("Multiple concerns: {negative_changes[1]}, {negative_changes[2]}, and more.")
        }
      } else {
        # No significant changes
        performance_text <- "Performance is similar to recent seasons."
      }

      # Combine performance text with regression assessment
      return(str_glue("{performance_text} Likelihood of regression: {regression_risk}."))
    },
    error = function(e) {
      cat("❌ Error in generate_quick_insight:", e$message, "\n")
      return("Player data available for detailed analysis.")
    }
  )
}
# Assess regression likelihood based on luck indicators
# Fixed assess_regression_likelihood function

assess_regression_likelihood <- function(player_data, player_type) {
  if (is.null(player_data) || nrow(player_data) == 0) {
    return("low")
  }

  luck_indicators <- c()

  if (player_type == "hitter") {
    # BABIP analysis - FIXED: Proper NA handling
    babip_cur <- player_data$BABIP_cur
    babip_l3 <- player_data$BABIP_l3

    if (!is.null(babip_cur) && !is.null(babip_l3) &&
      !is.na(babip_cur) && !is.na(babip_l3) &&
      is.numeric(babip_cur) && is.numeric(babip_l3)) {
      babip_diff <- babip_cur - babip_l3
      if (!is.na(babip_diff) && abs(babip_diff) > 0.025) {
        if (babip_diff > 0) {
          luck_indicators <- c(luck_indicators, "high_babip")
        } else {
          luck_indicators <- c(luck_indicators, "low_babip")
        }
      }
    }

    # xwOBA-wOBA gap analysis - FIXED: Proper NA handling
    current_gap <- player_data$xwOBA_wOBA_gap_cur
    historical_gap <- player_data$xwOBA_wOBA_gap_l3

    if (!is.null(current_gap) && !is.na(current_gap) && is.numeric(current_gap)) {
      historical_gap <- if (is.null(historical_gap) || is.na(historical_gap)) 0 else historical_gap

      if (abs(current_gap) > 0.015 || abs(current_gap - historical_gap) > 0.020) {
        if (current_gap < -0.015) {
          luck_indicators <- c(luck_indicators, "lucky_woba")
        } else if (current_gap > 0.015) {
          luck_indicators <- c(luck_indicators, "unlucky_woba")
        }
      }
    }

    # Barrel rate vs BABIP inconsistency - FIXED: Proper NA handling
    barrel_diff <- player_data$Barrel_pct_diff
    babip_diff <- player_data$BABIP_diff

    if (!is.null(barrel_diff) && !is.null(babip_diff) &&
      !is.na(barrel_diff) && !is.na(babip_diff) &&
      is.numeric(barrel_diff) && is.numeric(babip_diff)) {
      if ((barrel_diff < -1 && babip_diff > 0.02) ||
        (barrel_diff > 1 && babip_diff < -0.02)) {
        luck_indicators <- c(luck_indicators, "contact_babip_mismatch")
      }
    }
  } else { # pitcher
    # BABIP analysis - FIXED: Proper NA handling
    babip_cur <- player_data$babip_cur
    babip_l3 <- player_data$babip_l3

    if (!is.null(babip_cur) && !is.null(babip_l3) &&
      !is.na(babip_cur) && !is.na(babip_l3) &&
      is.numeric(babip_cur) && is.numeric(babip_l3)) {
      babip_diff <- babip_cur - babip_l3
      if (!is.na(babip_diff) && abs(babip_diff) > 0.025) {
        if (babip_diff < 0) {
          luck_indicators <- c(luck_indicators, "lucky_babip")
        } else {
          luck_indicators <- c(luck_indicators, "unlucky_babip")
        }
      }
    }

    # ERA vs xERA gap - FIXED: Proper NA handling
    era_cur <- player_data$era_cur
    xera_cur <- player_data$xera_cur

    if (!is.null(era_cur) && !is.null(xera_cur) &&
      !is.na(era_cur) && !is.na(xera_cur) &&
      is.numeric(era_cur) && is.numeric(xera_cur)) {
      era_xera_gap <- era_cur - xera_cur
      if (!is.na(era_xera_gap) && abs(era_xera_gap) > 0.30) {
        if (era_xera_gap < -0.30) {
          luck_indicators <- c(luck_indicators, "lucky_era")
        } else if (era_xera_gap > 0.30) {
          luck_indicators <- c(luck_indicators, "unlucky_era")
        }
      }
    }

    # LOB% analysis - FIXED: Proper NA handling
    lob_cur <- player_data$lob_percent_cur
    lob_l3 <- player_data$lob_percent_l3

    if (!is.null(lob_cur) && !is.null(lob_l3) &&
      !is.na(lob_cur) && !is.na(lob_l3) &&
      is.numeric(lob_cur) && is.numeric(lob_l3)) {
      lob_diff <- lob_cur - lob_l3
      if (!is.na(lob_diff) && abs(lob_diff) > 3) {
        if (lob_diff > 3) {
          luck_indicators <- c(luck_indicators, "high_lob")
        } else if (lob_diff < -3) {
          luck_indicators <- c(luck_indicators, "low_lob")
        }
      }
    }

    # Barrel rate vs ERA inconsistency - FIXED: Proper NA handling
    barrel_diff <- player_data$barrel_percent_diff
    era_diff <- player_data$era_diff

    if (!is.null(barrel_diff) && !is.null(era_diff) &&
      !is.na(barrel_diff) && !is.na(era_diff) &&
      is.numeric(barrel_diff) && is.numeric(era_diff)) {
      if ((barrel_diff > 1 && era_diff < -0.5) ||
        (barrel_diff < -1 && era_diff > 0.5)) {
        luck_indicators <- c(luck_indicators, "barrel_era_mismatch")
      }
    }
  }

  # Determine regression likelihood based on luck indicators
  luck_count <- length(luck_indicators)

  if (luck_count == 0) {
    return("low")
  }

  lucky_indicators <- sum(str_detect(luck_indicators, "lucky|high_babip|high_lob"))
  unlucky_indicators <- sum(str_detect(luck_indicators, "unlucky|low_babip|low_lob"))
  mismatch_indicators <- sum(str_detect(luck_indicators, "mismatch"))

  if (luck_count >= 3 || mismatch_indicators >= 2) {
    return("high")
  } else if (luck_count == 2 || (lucky_indicators >= 1 && unlucky_indicators >= 1)) {
    return("medium")
  } else if (luck_count == 1) {
    return("medium")
  } else {
    return("low")
  }
}
#' Main player analysis router using tidyverse
#' @param player_id FanGraphs player ID
#' @param analysis_mode Analysis style mode
#' @param baseball_data Complete baseball data list
#' @return HTML analysis result
analyze_player_performance <- function(player_id, analysis_mode, baseball_data) {
  player_info <- get_player_info(player_id, baseball_data)
  if (is.null(player_info)) {
    return(HTML("<div class='alert alert-warning'>Player not found.</div>"))
  }

  case_when(
    player_info$type == "hitter" ~ analyze_hitter_performance(player_info$name, analysis_mode, baseball_data$hitters),
    player_info$type == "pitcher" ~ analyze_pitcher_performance(player_info$name, analysis_mode, baseball_data$pitchers),
    TRUE ~ HTML("<div class='alert alert-warning'>Unknown player type.</div>")
  )
}

# UI Styling ----------------------------------------------------------------

ui_styles <- HTML("
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');

  /* Global app styling with viewport lock */
  * {
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  }

  html, body {
    margin: 0;
    padding: 0;
    width: 100%;
    height: 100%;
    overflow-x: hidden !important;
    position: fixed !important;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  }

  #shiny-ui {
    width: 100vw !important;
    height: 100vh !important;
    overflow-x: hidden !important;
    overflow-y: auto !important;
  }

  /* Navbar styling with responsive brand */
  .navbar {
    background: rgba(255, 255, 255, 0.95) !important;
    backdrop-filter: blur(20px);
    border-bottom: 1px solid rgba(255, 255, 255, 0.2);
    box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1);
    padding: 0.5rem 1rem !important;
  }

  .navbar-brand {
    font-weight: 700 !important;
    font-size: 1.3rem !important;
    color: #2E86AB !important;
    text-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
    padding: 0.25rem 0 !important;
  }

  .navbar-subtitle {
    font-size: 0.65rem !important;
    color: #6c757d !important;
    font-weight: 400 !important;
    line-height: 1 !important;
    margin-top: 2px !important;
  }

  .nav-link {
    font-weight: 500 !important;
    transition: all 0.3s ease !important;
    border-radius: 8px !important;
    margin: 0 2px !important;
    padding: 0.5rem 0.75rem !important;
    font-size: 0.9rem !important;
  }

  .nav-link:hover {
    background: rgba(46, 134, 171, 0.1) !important;
    transform: translateY(-1px);
  }

  /* Container adjustments for mobile */
  .container-fluid {
    padding-left: 0.75rem !important;
    padding-right: 0.75rem !important;
    max-width: 100% !important;
    overflow-x: hidden !important;
  }

  /* Card styling with glass morphism */
  .card {
    background: rgba(255, 255, 255, 0.95) !important;
    backdrop-filter: blur(20px) !important;
    border: 1px solid rgba(255, 255, 255, 0.2) !important;
    border-radius: 15px !important;
    box-shadow: 0 15px 35px rgba(0, 0, 0, 0.1) !important;
    transition: all 0.3s ease !important;
    overflow: hidden;
    margin-bottom: 1rem !important;
  }

  .card:hover {
    transform: translateY(-3px);
    box-shadow: 0 20px 40px rgba(0, 0, 0, 0.15) !important;
  }

  .card-header {
    background: linear-gradient(135deg, #2E86AB, #4A90E2) !important;
    color: white !important;
    font-weight: 600 !important;
    font-size: 1rem !important;
    border-bottom: none !important;
    padding: 1rem 1.25rem !important;
    text-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
  }

  .card-body {
    padding: 1.25rem !important;
  }

  /* Form controls styling */
  .form-select, .form-control {
    border: 2px solid rgba(46, 134, 171, 0.2) !important;
    border-radius: 10px !important;
    padding: 0.6rem 0.85rem !important;
    font-weight: 500 !important;
    font-size: 0.9rem !important;
    transition: all 0.3s ease !important;
    background: rgba(255, 255, 255, 0.9) !important;
  }

  .form-select:focus, .form-control:focus {
    border-color: #2E86AB !important;
    box-shadow: 0 0 0 3px rgba(46, 134, 171, 0.2) !important;
    transform: scale(1.01);
  }

  /* Button styling */
  .btn {
    border-radius: 10px !important;
    font-weight: 600 !important;
    padding: 0.6rem 1.2rem !important;
    transition: all 0.3s ease !important;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    font-size: 0.8rem !important;
  }

  .btn-primary {
    background: linear-gradient(135deg, #2E86AB, #4A90E2) !important;
    border: none !important;
    box-shadow: 0 4px 15px rgba(46, 134, 171, 0.3) !important;
  }

  .btn-primary:hover {
    transform: translateY(-2px);
    box-shadow: 0 8px 25px rgba(46, 134, 171, 0.4) !important;
  }

  /* CACHE: Cache status styling */
  .cache-status {
    background: rgba(46, 134, 171, 0.1) !important;
    border: 1px solid rgba(46, 134, 171, 0.2) !important;
    border-radius: 8px !important;
    padding: 8px 12px !important;
    font-size: 0.8rem !important;
    color: #2E86AB !important;
  }

  .btn-outline-secondary {
    border-color: rgba(108, 117, 125, 0.3) !important;
    color: #6c757d !important;
  }

  .btn-outline-secondary:hover {
    background: rgba(108, 117, 125, 0.1) !important;
    border-color: #6c757d !important;
  }

  /* Player info card styling */
  .player-info-card {
    background: linear-gradient(135deg, rgba(255, 255, 255, 0.95), rgba(248, 249, 250, 0.95)) !important;
    backdrop-filter: blur(20px) !important;
    border: 1px solid rgba(255, 255, 255, 0.3) !important;
    border-radius: 15px !important;
    padding: 1.5rem !important;
    box-shadow: 0 10px 25px rgba(0, 0, 0, 0.1) !important;
    transition: all 0.4s ease !important;
    margin-bottom: 1rem !important;
  }

  .player-info-card:hover {
    transform: translateY(-2px) scale(1.01);
    box-shadow: 0 15px 35px rgba(0, 0, 0, 0.15) !important;
  }

  .player-photo {
    width: 120px !important;
    height: 120px !important;
    border-radius: 50% !important;
    border: 3px solid #2E86AB !important;
    margin-bottom: 0.75rem !important;
    object-fit: cover !important;
    transition: all 0.3s ease !important;
    box-shadow: 0 8px 20px rgba(46, 134, 171, 0.3) !important;
  }

  .player-photo:hover {
    transform: scale(1.03);
    box-shadow: 0 10px 25px rgba(46, 134, 171, 0.4) !important;
  }

  .player-name {
    margin-bottom: 0.4rem !important;
    color: #2E86AB !important;
    font-weight: 700 !important;
    font-size: 1.2rem !important;
    text-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
  }

  .player-details {
    color: #6c757d !important;
    font-size: 0.85rem !important;
    font-weight: 500 !important;
    margin-bottom: 0 !important;
  }

  /* Alert styling */
  .alert {
    border-radius: 12px !important;
    border: none !important;
    font-weight: 500 !important;
    backdrop-filter: blur(10px) !important;
    font-size: 0.9rem !important;
  }

  .alert-info {
    background: rgba(13, 202, 240, 0.1) !important;
    color: #0c5aa6 !important;
  }

  .alert-warning {
    background: rgba(255, 193, 7, 0.1) !important;
    color: #997404 !important;
  }

  /* Loading animation */
  .progress {
    height: 6px !important;
    border-radius: 10px !important;
    background: rgba(255, 255, 255, 0.2) !important;
  }

  .progress-bar {
    background: linear-gradient(90deg, #2E86AB, #4A90E2) !important;
    border-radius: 10px !important;
  }

  /* Plot container styling */
  .shiny-plot-output {
    border-radius: 12px !important;
    overflow: hidden !important;
    box-shadow: 0 8px 20px rgba(0, 0, 0, 0.1) !important;
  }

  /* Analysis content styling */
  .analysis-content {
    line-height: 1.5 !important;
    font-size: 0.95rem !important;
  }

  .analysis-content h1, .analysis-content h2, .analysis-content h3 {
    color: #2E86AB !important;
    font-weight: 700 !important;
    margin-top: 1.2rem !important;
    margin-bottom: 0.8rem !important;
    font-size: 1.1rem !important;
  }

  .analysis-content p {
    margin-bottom: 0.8rem !important;
    color: #495057 !important;
  }

  /* Mobile-specific optimizations */
  @media (max-width: 768px) {
    .navbar-brand {
      font-size: 1.1rem !important;
      /* Show mobile-specific layout */
.vibe-selector {
  display: none;
}

.vibe-selector-mobile {
  display: flex;
}
    }

    .navbar-subtitle {
      font-size: 0.6rem !important;
    }

    .nav-link {
      font-size: 0.85rem !important;
      padding: 0.4rem 0.6rem !important;
    }

    .card {
      border-radius: 12px !important;
      margin-bottom: 0.75rem !important;
    }

    .card-body {
      padding: 1rem !important;
    }

    .player-info-card {
      padding: 1.2rem !important;
    }

    .player-photo {
      width: 100px !important;
      height: 100px !important;
    }

    .player-name {
      font-size: 1.1rem !important;
    }

    .player-details {
      font-size: 0.8rem !important;
    }

    .analysis-content {
      font-size: 0.9rem !important;
    }

    .analysis-content h1, .analysis-content h2, .analysis-content h3 {
      font-size: 1rem !important;
    }

    /* Ensure columns stack properly on mobile */
    .layout-columns {
      flex-direction: column !important;
    }

    .layout-columns > * {
      width: 100% !important;
      flex: none !important;
    }

    .btn {
      font-size: 0.75rem !important;
      padding: 0.5rem 1rem !important;
    }
  }

  /* AI Status Badge Styling */
.badge {
  font-size: 0.75rem !important;
  padding: 0.35rem 0.65rem !important;
  border-radius: 12px !important;
}

.badge .fa-spinner {
  animation: fa-spin 1s infinite linear;
}

.badge .fa-check-circle {
  color: white !important;
}

/* Status Alert Improvements */
.alert .spinner-border-sm {
  width: 1rem;
  height: 1rem;
  border-width: 0.1em;
}

.btn-outline-success:hover {
  transform: translateY(-1px);
  box-shadow: 0 2px 8px rgba(40, 167, 69, 0.3);
}

/* Mobile responsiveness */
@media (max-width: 768px) {
  .step-header {
    flex-direction: column;
    align-items: flex-start;
    gap: 0.5rem;
  }

  .step-header .badge {
    align-self: flex-start;
    margin-left: 0 !important;
    margin-top: 0.25rem;
  }
}

  /* Extra small screens */
  @media (max-width: 576px) {
    .container-fluid {
      padding-left: 0.5rem !important;
      padding-right: 0.5rem !important;
    }

    .navbar {
      padding: 0.4rem 0.75rem !important;
    }

    .navbar-brand {
      font-size: 1rem !important;
    }

    .navbar-subtitle {
      font-size: 0.55rem !important;
    }

    .card-body {
      padding: 0.85rem !important;
    }

    .player-info-card {
      padding: 1rem !important;
    }

    .form-select, .form-control {
      font-size: 0.85rem !important;
      padding: 0.5rem 0.7rem !important;
    }

    .btn {
      font-size: 0.7rem !important;
      padding: 0.4rem 0.8rem !important;
    }
  }

  /* Smooth scrolling */
  html {
    scroll-behavior: smooth;
  }

  /* Custom scrollbar */
  ::-webkit-scrollbar {
    width: 6px;
  }

  ::-webkit-scrollbar-track {
    background: rgba(255, 255, 255, 0.1);
  }

  ::-webkit-scrollbar-thumb {
    background: rgba(46, 134, 171, 0.5);
    border-radius: 10px;
  }

  ::-webkit-scrollbar-thumb:hover {
    background: rgba(46, 134, 171, 0.7);
  }
  
  /* About Page Styling - Add this to your existing ui_styles HTML block */

/* About Page Hero - Enhanced with proper visual container */
.about-hero-wrapper {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  padding: 2rem 1rem;
  margin: -1rem -1rem 2rem -1rem; /* Extend to edges */
}

.about-hero {
  background: rgba(255, 255, 255, 0.95);
  backdrop-filter: blur(20px);
  border: 1px solid rgba(255, 255, 255, 0.2);
  border-radius: 20px;
  padding: 3rem 2rem;
  box-shadow: 0 20px 40px rgba(0, 0, 0, 0.15);
  max-width: 1200px;
  margin: 0 auto;
}

.hero-content {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 3rem;
}

.hero-text {
  flex: 2;
  text-align: left;
}

.hero-text-container {
  background: rgba(255, 255, 255, 0.8);
  border-radius: 15px;
  padding: 2rem;
  box-shadow: 0 10px 25px rgba(0, 0, 0, 0.1);
  border: 1px solid rgba(255, 255, 255, 0.5);
}

.hero-image {
  flex: 1;
  text-align: center;
}

.about-title {
  font-size: 2.5rem;
  font-weight: 700;
  color: #2E86AB;
  margin-bottom: 0.5rem;
  text-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.about-subtitle {
  font-size: 1.2rem;
  color: #4A90E2;
  margin-bottom: 1rem;
  font-weight: 500;
}

.about-description {
  font-size: 1.1rem;
  color: #495057;
  line-height: 1.6;
  margin-bottom: 0;
}

.mcfarland-photo {
  max-width: 200px;
  width: 100%;
  height: auto;
  border-radius: 15px;
  box-shadow: 0 10px 25px rgba(0,0,0,0.15);
  border: 3px solid #2E86AB;
}

.about-content {
  display: flex;
  flex-direction: column;
  gap: 2rem;
}

/* Feature Cards */
.feature-card .card-header {
  background: linear-gradient(135deg, #2E86AB, #4A90E2);
}

.feature-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
  gap: 1.5rem;
}

.feature-item {
  text-align: center;
  padding: 1rem;
  background: rgba(46, 134, 171, 0.05);
  border-radius: 12px;
  transition: transform 0.3s ease;
}

.feature-item:hover {
  transform: translateY(-3px);
  box-shadow: 0 5px 15px rgba(0,0,0,0.1);
}

.feature-icon {
  font-size: 2rem;
  color: #2E86AB;
  margin-bottom: 0.75rem;
}

.feature-item h5 {
  color: #2E86AB;
  font-weight: 600;
  margin-bottom: 0.5rem;
}

.feature-item p {
  color: #6c757d;
  font-size: 0.9rem;
  line-height: 1.4;
  margin-bottom: 0;
}

/* Story Card */
.story-card .card-header {
  background: linear-gradient(135deg, #28a745, #20c997);
}

.story-card p {
  font-size: 1rem;
  line-height: 1.6;
  color: #495057;
}

/* Updates Card */
.updates-card .card-header {
  background: linear-gradient(135deg, #ffc107, #fd7e14);
}

/* Version Timeline */
.version-timeline {
  position: relative;
  padding-left: 0;
}

.version-item {
  display: flex;
  align-items: flex-start;
  margin-bottom: 1.5rem;
  position: relative;
}

.version-item:not(:last-child):before {
  content: '';
  position: absolute;
  left: 22px;
  top: 45px;
  bottom: -24px;
  width: 2px;
  background: #dee2e6;
}

.version-item.current:not(:last-child):before {
  background: #2E86AB;
}

.version-marker {
  display: flex;
  flex-direction: column;
  align-items: center;
  margin-right: 1rem;
  position: relative;
  z-index: 2;
}

.version-dot {
  width: 12px;
  height: 12px;
  border-radius: 50%;
  background: #dee2e6;
  border: 3px solid white;
  box-shadow: 0 0 0 2px #dee2e6;
}

.version-item.current .version-dot {
  background: #2E86AB;
  box-shadow: 0 0 0 2px #2E86AB;
}

.version-item.major .version-dot {
  background: #dc3545;
  box-shadow: 0 0 0 2px #dc3545;
}

.version-item.beloved .version-dot {
  background: #6f42c1;
  box-shadow: 0 0 0 2px #6f42c1;
}

.version-item.founding .version-dot {
  background: #ffc107;
  box-shadow: 0 0 0 2px #ffc107;
}

.version-number {
  font-size: 0.7rem;
  font-weight: 600;
  color: #6c757d;
  margin-top: 0.25rem;
  text-align: center;
}

.version-content {
  flex: 1;
  min-width: 0;
}

.version-header {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  margin-bottom: 0.5rem;
  flex-wrap: wrap;
}

.version-title {
  font-weight: 600;
  color: #2E86AB;
  margin: 0;
  font-size: 1.1rem;
}

.version-title.founding {
  color: #ffc107;
}

.version-badge {
  font-size: 0.7rem;
  font-weight: 600;
  padding: 0.2rem 0.5rem;
  border-radius: 10px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.version-badge.new {
  background: #28a745;
  color: white;
}

.version-badge.founding {
  background: #ffc107;
  color: #212529;
}

.version-date {
  font-size: 0.8rem;
  color: #6c757d;
  margin-left: auto;
}

.version-description {
  color: #495057;
  line-height: 1.4;
  font-size: 0.9rem;
  margin: 0;
}

.version-description.founding {
  font-style: italic;
}

.version-content h5 {
  color: #2E86AB;
  font-weight: 600;
  margin-bottom: 0.5rem;
  font-size: 1rem;
}

.version-content p {
  color: #6c757d;
  font-size: 0.9rem;
  line-height: 1.4;
  margin: 0;
}

/* Tech Card */
.tech-card .card-header {
  background: linear-gradient(135deg, #6f42c1, #e83e8c);
}

.tech-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 1.5rem;
}

.tech-item h5 {
  color: #2E86AB;
  font-weight: 600;
  margin-bottom: 0.75rem;
  font-size: 1rem;
}

.tech-item ul {
  margin: 0;
  padding-left: 1.2rem;
}

.tech-item li {
  color: #495057;
  font-size: 0.9rem;
  line-height: 1.5;
  margin-bottom: 0.5rem;
}

.tech-item strong {
  color: #2E86AB;
}

/* Connect Card */
.connect-card .card-header {
  background: linear-gradient(135deg, #dc3545, #fd7e14);
}

.connect-buttons {
  display: flex;
  gap: 1rem;
  margin: 1.5rem 0;
}

.connect-btn {
  flex: 1;
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 0.5rem;
  padding: 0.75rem 1rem;
  font-weight: 600;
  text-decoration: none;
}

.connect-btn:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 15px rgba(46, 134, 171, 0.3);
}

.connect-note {
  background: rgba(46, 134, 171, 0.05);
  border: 1px solid rgba(46, 134, 171, 0.2);
  border-radius: 8px;
  padding: 1rem;
  margin-top: 1rem;
}

.connect-note p {
  margin: 0;
  font-size: 0.9rem;
  color: #495057;
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.connect-note .fa-shield-alt {
  color: #28a745;
}

/* About Footer - Enhanced with proper visual container */
.about-footer-wrapper {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  padding: 1.5rem 1rem;
  margin: 2rem -1rem -1rem -1rem; /* Extend to edges, connect to bottom */
}

.about-footer {
  background: rgba(255, 255, 255, 0.95);
  backdrop-filter: blur(20px);
  border: 1px solid rgba(255, 255, 255, 0.2);
  border-radius: 15px;
  padding: 2rem;
  box-shadow: 0 15px 35px rgba(0, 0, 0, 0.15);
  max-width: 1200px;
  margin: 0 auto;
  text-align: center;
  border-top: none; /* Remove the old border */
}

.about-footer p {
  margin: 0.5rem 0;
  color: #495057; /* Darker text for better contrast */
}

.credits {
  font-size: 0.9rem;
}

.credits strong {
  color: #2E86AB;
}

/* About Page Mobile Responsiveness */
@media (max-width: 768px) {
  .about-hero-wrapper {
    padding: 1.5rem 0.5rem;
    margin: -0.75rem -0.75rem 1.5rem -0.75rem;
  }
  
  .about-hero {
    padding: 2rem 1.5rem;
  }
  
  .about-footer-wrapper {
    padding: 1.25rem 0.5rem;
    margin: 2rem -0.75rem -0.75rem -0.75rem;
  }
  
  .about-footer {
    padding: 1.5rem;
  }
  
  .hero-content {
    flex-direction: column;
    text-align: center;
    gap: 2rem;
  }
  
  .hero-text {
    text-align: center;
  }
  
  .hero-text-container {
    padding: 1.5rem;
  }
  
  .about-title {
    font-size: 2rem;
  }
  
  .about-subtitle {
    font-size: 1rem;
  }
  
  .about-description {
    font-size: 1rem;
  }
  
  .mcfarland-photo {
    max-width: 150px;
  }
  
  .feature-grid {
    grid-template-columns: 1fr;
    gap: 1rem;
  }
  
  .tech-grid {
    grid-template-columns: 1fr;
    gap: 1rem;
  }
  
  .connect-buttons {
    flex-direction: column;
  }
  
  .version-header {
    flex-direction: column;
    align-items: flex-start;
    gap: 0.5rem;
  }
  
  .version-date {
    margin-left: 0;
    align-self: flex-start;
  }
  
  .version-timeline {
    padding-left: 0;
  }
  
  .version-item {
    margin-bottom: 2rem;
  }
}

@media (max-width: 576px) {
  .about-hero-wrapper {
    padding: 1rem 0.25rem;
    margin: -0.75rem -0.75rem 1rem -0.75rem;
  }
  
  .about-hero {
    padding: 1.5rem 1rem;
  }
  
  .about-footer-wrapper {
    padding: 1rem 0.25rem;
    margin: 1.5rem -0.75rem -0.75rem -0.75rem;
  }
  
  .about-footer {
    padding: 1.25rem;
  }
  
  .hero-text-container {
    padding: 1.25rem;
  }
  
  .about-title {
    font-size: 1.75rem;
  }
  
  .about-subtitle {
    font-size: 0.9rem;
  }
  
  .feature-item {
    padding: 0.75rem;
  }
  
  .version-description {
    font-size: 0.85rem;
  }
}
  
")

# UI Definition -------------------------------------------------------------

# Replace the UI section in your app.R with this redesigned layout

# UI Definition - Redesigned with Progressive Flow
ui <- page_navbar(
  title = "McFARLAND",
  header = tagList(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0, user-scalable=no, viewport-fit=cover, shrink-to-fit=no"),
    tags$style(ui_styles), # Keep your existing styles
    # Add new styles for the progressive flow
    tags$style(HTML("
      /* Progressive Flow Specific Styles */
      .hero-section {
        text-align: center;
        color: white;
        margin: 2rem 0 3rem 0;
        padding: 0 1rem;
      }

      .hero-title {
        font-size: 2.5rem;
        font-weight: 700;
        margin-bottom: 0.5rem;
        text-shadow: 0 2px 4px rgba(0,0,0,0.3);
      }

      .hero-subtitle {
        font-size: 1.1rem;
        opacity: 0.9;
        margin-bottom: 0;
      }

      .search-card {
        background: rgba(255, 255, 255, 0.95);
        backdrop-filter: blur(20px);
        border-radius: 20px;
        padding: 2rem;
        box-shadow: 0 20px 40px rgba(0, 0, 0, 0.1);
        margin-bottom: 2rem;
      }

      .search-input-container .form-select {
        border: 2px solid rgba(46, 134, 171, 0.2);
        border-radius: 15px;
        padding: 1rem 1.5rem;
        font-size: 1.1rem;
        background: rgba(255, 255, 255, 0.9);
      }

      .quick-filters {
        display: flex;
        gap: 0.5rem;
        flex-wrap: wrap;
        margin-top: 1rem;
      }

      .filter-chip {
        background: rgba(46, 134, 171, 0.1);
        border: 1px solid rgba(46, 134, 171, 0.3);
        border-radius: 25px;
        padding: 0.5rem 1rem;
        font-size: 0.9rem;
        color: #2E86AB;
        cursor: pointer;
        transition: all 0.3s ease;
        text-decoration: none;
      }

      .filter-chip:hover, .filter-chip.active {
        background: #2E86AB;
        color: white;
        transform: translateY(-1px);
      }

      .step-card {
        background: rgba(255, 255, 255, 0.95);
        backdrop-filter: blur(20px);
        border-radius: 15px;
        padding: 1.5rem;
        box-shadow: 0 10px 25px rgba(0, 0, 0, 0.1);
        transition: all 0.3s ease;
        border-left: 4px solid transparent;
        margin-bottom: 1.5rem;
      }

      .step-card.active {
        border-left-color: #2E86AB;
        transform: translateY(-2px);
        box-shadow: 0 15px 35px rgba(0, 0, 0, 0.15);
      }

      .step-card.inactive {
        opacity: 0.6;
        border-left-color: #dee2e6;
      }

      .step-header {
        display: flex;
        align-items: center;
        gap: 1rem;
        margin-bottom: 1rem;
      }

      .step-number {
        background: linear-gradient(135deg, #2E86AB, #4A90E2);
        color: white;
        width: 40px;
        height: 40px;
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        font-weight: 700;
        font-size: 1.1rem;
        flex-shrink: 0;
      }

      .step-number.inactive {
        background: #dee2e6;
        color: #6c757d;
      }

      .step-title {
        font-size: 1.3rem;
        font-weight: 600;
        color: #2E86AB;
        margin: 0;
      }

      .step-title.inactive {
        color: #6c757d;
      }

      .player-preview {
        display: flex;
        align-items: center;
        gap: 1rem;
        padding: 1rem;
        background: rgba(46, 134, 171, 0.05);
        border-radius: 12px;
        margin-bottom: 1rem;
      }

      .player-preview-avatar {
        width: 60px;
        height: 60px;
        border-radius: 50%;
        border: 2px solid #2E86AB;
        object-fit: cover;
      }

      .player-preview-info h4 {
        margin: 0;
        color: #2E86AB;
        font-weight: 600;
      }

      .player-preview-info p {
        margin: 0;
        color: #6c757d;
        font-size: 0.9rem;
      }


/* Compact Vibe Selector - Mobile First Design */

/* Replace the existing .vibe-selector styles with this more compact version */
.vibe-selector {
  display: flex;
  flex-direction: column;
  gap: 0.75rem;
  margin-top: 1rem;
}

.vibe-row {
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
}

.vibe-card-compact {
  background: rgba(255, 255, 255, 0.9);
  border: 2px solid rgba(46, 134, 171, 0.2);
  border-radius: 8px;
  padding: 0.75rem;
  cursor: pointer;
  transition: all 0.3s ease;
  position: relative;
  flex: 1;
  min-width: 120px;
  max-width: 160px;
  text-align: center;
}

.vibe-card-compact:hover {
  border-color: #2E86AB;
  background: rgba(46, 134, 171, 0.1);
  transform: translateY(-1px);
}

.vibe-card-compact.selected {
  border-color: #2E86AB;
  background: rgba(46, 134, 171, 0.15);
  box-shadow: 0 2px 8px rgba(46, 134, 171, 0.3);
}

.vibe-card-compact.selected::after {
  content: '✓';
  position: absolute;
  top: 0.25rem;
  right: 0.25rem;
  background: #2E86AB;
  color: white;
  width: 18px;
  height: 18px;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.7rem;
  font-weight: bold;
}

.vibe-icon-compact {
  font-size: 1.5rem;
  margin-bottom: 0.25rem;
}

.vibe-name-compact {
  font-weight: 600;
  color: #2E86AB;
  font-size: 0.85rem;
  margin: 0;
  line-height: 1.2;
}

/* Mobile: Compact vertical stack - Option A */
.vibe-selector-mobile {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  margin-top: 1rem;
}

.vibe-option-mobile {
  background: rgba(255, 255, 255, 0.9);
  border: 2px solid rgba(46, 134, 171, 0.2);
  border-radius: 8px;
  padding: 0.75rem 1rem;
  cursor: pointer;
  transition: all 0.3s ease;
  display: flex;
  align-items: center;
  gap: 0.75rem;
  position: relative;
}

.vibe-option-mobile:hover {
  border-color: #2E86AB;
  background: rgba(46, 134, 171, 0.1);
}

.vibe-option-mobile.selected {
  border-color: #2E86AB;
  background: rgba(46, 134, 171, 0.15);
  box-shadow: 0 2px 8px rgba(46, 134, 171, 0.3);
}

.vibe-option-mobile.selected::after {
  content: '✓';
  position: absolute;
  top: 50%;
  right: 1rem;
  transform: translateY(-50%);
  background: #2E86AB;
  color: white;
  width: 20px;
  height: 20px;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.75rem;
  font-weight: bold;
}

.vibe-option-icon {
  font-size: 1.5rem;
  flex-shrink: 0;
}

.vibe-option-text {
  display: flex;
  flex-direction: column;
  flex-grow: 1;
}

.vibe-option-name {
  font-weight: 600;
  color: #2E86AB;
  font-size: 0.9rem;
  margin: 0;
  line-height: 1.2;
}

.vibe-option-desc {
  font-size: 0.75rem;
  color: #6c757d;
  margin: 0;
  line-height: 1.2;
}

      .insight-summary {
        background: linear-gradient(135deg, rgba(46, 134, 171, 0.1), rgba(74, 144, 226, 0.1));
        border-left: 4px solid #2E86AB;
        border-radius: 8px;
        padding: 1rem;
        margin-bottom: 1.5rem;
      }

      .insight-summary h5 {
        color: #2E86AB;
        font-weight: 700;
        margin-bottom: 0.5rem;
        display: flex;
        align-items: center;
        gap: 0.5rem;
      }

      .empty-state {
        text-align: center;
        padding: 3rem 1rem;
        color: #6c757d;
      }

      .empty-state .empty-icon {
        font-size: 3rem;
        margin-bottom: 1rem;
        opacity: 0.5;
      }

      .empty-state .empty-title {
        font-size: 1.2rem;
        font-weight: 600;
        margin-bottom: 0.5rem;
        color: #2E86AB;
      }

      .empty-state .empty-subtitle {
        font-size: 0.9rem;
        margin-bottom: 0;
      }

      @media (max-width: 768px) {
        .hero-title {
          font-size: 2rem;
        }

        .search-card {
          padding: 1.5rem;
        }

        .vibe-selector {
          grid-template-columns: 1fr;
        }

        .step-header {
          flex-direction: row;
          align-items: flex-start;
          gap: 0.75rem;
        }

        .step-title {
          font-size: 1.1rem;
        }

        .player-preview {
          flex-direction: column;
          text-align: center;
          gap: 0.75rem;
        }
      }
    ")),
    add_busy_bar(color = "#2E86AB", height = "25px"),

    # Keep your existing JavaScript for keep-alive
    tags$script(HTML("
      $(document).ready(function() {
        console.log('🔄 Keep-alive system initialized');
        var keepAliveInterval = 120000;
        var keepAliveTimer;
        var lastActivity = Date.now();
        var isVisible = true;

        function sendKeepAlivePing() {
          if (!isVisible) {
            console.log('⏸️ Tab not visible, skipping keep-alive ping');
            return;
          }
          console.log('💓 Sending keep-alive ping');
          Shiny.setInputValue('keepalive_ping', Date.now(), {priority: 'event'});
        }

        function updateActivity() {
          lastActivity = Date.now();
          console.log('👤 User activity detected');
          clearInterval(keepAliveTimer);
          startKeepAlive();
        }

        function startKeepAlive() {
          keepAliveTimer = setInterval(function() {
            var timeSinceActivity = Date.now() - lastActivity;
            if (timeSinceActivity < 600000) {
              sendKeepAlivePing();
            } else {
              console.log('💤 No recent activity, pausing keep-alive');
            }
          }, keepAliveInterval);
        }

        document.addEventListener('visibilitychange', function() {
          isVisible = !document.hidden;
          if (isVisible) {
            console.log('👁️ Tab visible, resuming keep-alive');
            updateActivity();
          } else {
            console.log('🙈 Tab hidden, pausing keep-alive');
          }
        });

        ['mousedown', 'mousemove', 'keypress', 'scroll', 'touchstart', 'click'].forEach(function(event) {
          document.addEventListener(event, updateActivity, true);
        });

        setInterval(function() {
          if (Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected()) {
            console.log('💚 Shiny connection healthy');
          } else {
            console.log('💔 Shiny connection lost - attempting reconnect');
            if (Shiny && Shiny.shinyapp && Shiny.shinyapp.reconnect) {
              Shiny.shinyapp.reconnect();
            }
          }
        }, 30000);

        startKeepAlive();
        console.log('✅ Keep-alive system active');
      });
    ")),

    # // Add this JavaScript to your UI header section, after your existing keep-alive script

    tags$script(HTML("
  $(document).ready(function() {
    var lastAnalysisTime = 0;
    var userScrolledUp = false;
    var analysisScrollTimeout;

    // Track user scroll behavior
    $(window).on('scroll', function() {
      var scrollTop = $(window).scrollTop();
      var analysisSection = $('.step-card.active .analysis-content');

      if (analysisSection.length > 0) {
        var analysisTop = analysisSection.offset().top - 100;
        userScrolledUp = scrollTop < analysisTop;
      }
    });

    // Vibe card interaction handler
    $(document).on('click', '.vibe-card', function() {
      var mode = $(this).data('mode');

      // Update visual selection
      $('.vibe-card').removeClass('selected');
      $(this).addClass('selected');

      // Send to Shiny
      Shiny.setInputValue('analysis_mode', mode, {priority: 'event'});

      // Mark that analysis was just triggered
      lastAnalysisTime = Date.now();
      userScrolledUp = false; // Reset scroll tracking

      console.log('🎨 Analysis mode changed to:', mode);
    });

    // Filter chip interaction
    $(document).on('click', '.filter-chip', function() {
      $('.filter-chip').removeClass('active');
      $(this).addClass('active');

      var filterType = $(this).text();
      console.log('🏷️ Filter changed to:', filterType);
    });

    // Smart scroll to analysis - only when analysis is newly generated
    var observer = new MutationObserver(function(mutations) {
      mutations.forEach(function(mutation) {
        // Look for new analysis content
        if (mutation.type === 'childList') {
          var newAnalysisContent = $(mutation.addedNodes).find('.analysis-content');
          if (newAnalysisContent.length > 0) {
            var timeSinceAnalysis = Date.now() - lastAnalysisTime;

            // Only auto-scroll if:
            // 1. Analysis was recently triggered (within 5 seconds)
            // 2. User hasn't deliberately scrolled up
            if (timeSinceAnalysis < 5000 && !userScrolledUp) {
              clearTimeout(analysisScrollTimeout);
              analysisScrollTimeout = setTimeout(function() {
                newAnalysisContent[0].scrollIntoView({
                  behavior: 'smooth',
                  block: 'start'
                });
                console.log('📜 Auto-scrolled to new analysis');
              }, 800); // Slight delay to let content render
            } else {
              console.log('📜 Skipped auto-scroll (user control)');
            }
          }
        }
      });
    });

    // Start observing
    observer.observe(document.body, {
      childList: true,
      subtree: true
    });

    console.log('✅ Smart scroll interactions initialized');
  });
"))
  ),
  nav_panel(
    title = "Analysis",
    icon = icon("chart-line"),

    # Hero Section
    div(
      class = "hero-section",
      h1(class = "hero-title", "⚾ McFARLAND ⚾"),
      p(class = "hero-subtitle", "Advanced baseball analysis. Plain English.")
    ),

    # Player Search Section
    div(
      class = "search-card",
      div(
        class = "search-input-container",
        selectInput(
          inputId = "player_selection",
          label = "Search for a player:",
          choices = NULL,
          width = "100%"
        )
      ),
      div(
        class = "quick-filters",
        span(class = "filter-chip active", "All Players"),
        span(class = "filter-chip", "Hitters"),
        span(class = "filter-chip", "Pitchers")
      )
    ),

    # Step 1: Player Selection
    uiOutput("step_1_player_selection"),

    # Step 2: Analysis Style
    uiOutput("step_2_analysis_style"),

    # Step 3: Analysis Results
    uiOutput("step_3_analysis_results")
  ),

  nav_panel(
    title = "About",
    icon = icon("info-circle"),
    
    # Hero Section with proper visual container
    div(
      class = "about-hero-wrapper",
      div(
        class = "about-hero",
        div(
          class = "hero-content",
          div(
            class = "hero-text",
            div(
              class = "hero-text-container",
              h1(class = "about-title", "Meet McFARLAND"),
              h2(class = "about-subtitle", "🤖⚾ Machine-crafted Forecasting And Reasoning for Luck, Analytics, Narratives, and Data"),
              p(class = "about-description", 
                "Your baseball analysis companion that cuts through the noise to tell you what's really happening -- in plain English! -- with MLB players. We separate skill from luck, trends from flukes, and give you insights that matter."
              )
            )
          ),
          div(
            class = "hero-image",
            img(
              src = "tjmcfarland.png", 
              alt = "T.J. McFarland - The inspiration behind our name",
              class = "mcfarland-photo"
            )
          )
        )
      )
    ),
    
    # Main Content Cards
    div(
      class = "about-content",
      
      # App Features Card
      card(
        class = "feature-card",
        card_header(
          icon("chart-line"), "App Features"
        ),
        card_body(
          div(
            class = "feature-grid",
            div(
              class = "feature-item",
              icon("brain", class = "feature-icon"),
              h5("Smart Analysis"),
              p("Insights that explain not just what's happening, but why it matters for the rest of the season.")
            ),
            div(
              class = "feature-item",
              icon("dice", class = "feature-icon"),
              h5("Skill vs Luck"),
              p("We separate sustainable performance changes from statistical noise using advanced metrics like xwOBA and BABIP.")
            ),
            div(
              class = "feature-item",
              icon("users", class = "feature-icon"),
              h5("Both Hitters & Pitchers"),
              p("Comprehensive analysis for position players and pitchers with metrics tailored to each role.")
            ),
            div(
              class = "feature-item",
              icon("palette", class = "feature-icon"),
              h5("Your Style"),
              p("Choose from 8 analysis personalities - from Shakespeare to Gen Z to Old School wisdom.")
            )
          )
        )
      ),
      
      # Get Connected Card - MOVED UP for better visibility
      card(
        class = "connect-card",
        card_header(
          icon("heart"), "Get Connected"
        ),
        card_body(
          p("McFARLAND is a passion project that gets better with your feedback. Whether you've found a bug, have an idea for a feature, or just want to say hello, we'd love to hear from you!"),
          div(
            class = "connect-buttons",
            tags$a(
              href = "https://docs.google.com/forms/d/e/1FAIpQLScPiHfO2XxwCXd2V-7pNsUKs-mMaqzzsH2ohA_kBflk_n8AQw/viewform",
              target = "_blank", 
              class = "btn btn-primary connect-btn",
              icon("bell"), "Get Updates"
            ),
            tags$a(
              href = "https://forms.gle/NDJJKj7XrsnFH6m16",
              target = "_blank", 
              class = "btn btn-primary connect-btn",
              icon("comment"), "Send Feedback"
            )
          ),
          div(
            class = "connect-note",
            p(
              icon("shield-alt"), 
              tags$strong("Privacy-First:"), 
              " We only collect anonymous usage data to improve the app. No personal information is stored or shared."
            )
          )
        )
      ),
      
      
      # The Story Card
      card(
        class = "story-card",
        card_header(
          icon("lightbulb"), "The Story"
        ),
        card_body(
          p("Named after ", tags$strong("T.J. McFarland"), " - the utility pitcher who embodied versatility and adaptation - our app does what great utility players do: it adapts to what you need."),
          p("Whether you're trying to figure out if your favorite player's hot streak is real, wondering if that pitcher's suddenly great season will continue, or just want to understand baseball better, McFARLAND gives you the insights that matter."),
          p("We built this because baseball analysis shouldn't require a statistics degree. Everyone deserves to understand what's really happening beyond the basic stats.")
        )
      ),
      
      
      
      # What's New Section
      card(
        class = "updates-card",
        card_header(
          icon("sparkles"), "What's New"
        ),
        card_body(
          div(
            class = "version-timeline",
            
            # Latest Version - Highlighted
            div(
              class = "version-item current",
              div(
                class = "version-marker current",
                span(class = "version-dot"),
                span(class = "version-number", "v1.0")
              ),
              div(
                class = "version-content",
                div(
                  class = "version-header",
                  h4("Complete Experience Redesign", class = "version-title"),
                  span(class = "version-badge new", "LATEST")
                ),
                p(class = "version-description",
                  "We rebuilt the entire app with a guided, step-by-step experience. Now you get instant player insights, smooth interactions, and a much more intuitive flow from player selection to AI analysis."
                )
              )
            ),
            
            # Recent Updates
            div(
              class = "version-item",
              div(
                class = "version-marker",
                span(class = "version-dot"),
                span(class = "version-number", "v0.9")
              ),
              div(
                class = "version-content",
                h5("Smart Caching & Performance"),
                p("Lightning-fast responses with intelligent caching. Previously analyzed players load instantly.")
              )
            ),
            
            div(
              class = "version-item",
              div(
                class = "version-marker",
                span(class = "version-dot"),
                span(class = "version-number", "v0.8")
              ),
              div(
                class = "version-content",
                h5("Player Photos"),
                p("Official MLB headshots for every player. Now you can put a face to the stats.")
              )
            ),
            
            div(
              class = "version-item major",
              div(
                class = "version-marker major",
                span(class = "version-dot"),
                span(class = "version-number", "v0.7")
              ),
              div(
                class = "version-content",
                h5("Pitcher Analysis"),
                p("Major expansion! Added complete pitcher analysis covering all of MLB.")
              )
            ),
            
            div(
              class = "version-item",
              div(
                class = "version-marker",
                span(class = "version-dot"),
                span(class = "version-number", "v0.6")
              ),
              div(
                class = "version-content",
                h5("Stay Connected"),
                p("Notification signup and feedback systems to keep you updated.")
              )
            ),
            
            div(
              class = "version-item",
              div(
                class = "version-marker",
                span(class = "version-dot"),
                span(class = "version-number", "v0.5")
              ),
              div(
                class = "version-content",
                h5("Advanced Metrics"),
                p("Enhanced analysis with Barrel% and xwOBA to better distinguish skill from luck.")
              )
            ),
            
            div(
              class = "version-item",
              div(
                class = "version-marker",
                span(class = "version-dot"),
                span(class = "version-number", "v0.4")
              ),
              div(
                class = "version-content",
                h5("Visual Trends"),
                p("Beautiful charts showing how current season compares to recent history.")
              )
            ),
            
            div(
              class = "version-item beloved",
              div(
                class = "version-marker beloved",
                span(class = "version-dot"),
                span(class = "version-number", "v0.3")
              ),
              div(
                class = "version-content",
                h5("Shakespeare Mode"),
                p("The fan-favorite feature: baseball analysis in iambic pentameter!")
              )
            ),
            
            div(
              class = "version-item",
              div(
                class = "version-marker",
                span(class = "version-dot"),
                span(class = "version-number", "v0.2")
              ),
              div(
                class = "version-content",
                h5("Analysis Personalities"),
                p("Multiple analysis vibes from Analytics Dork to Old Coot.")
              )
            ),
            
            div(
              class = "version-item founding",
              div(
                class = "version-marker founding",
                span(class = "version-dot"),
                span(class = "version-number", "v0.1")
              ),
              div(
                class = "version-content",
                div(
                  class = "version-header",
                  h5("The Beginning", class = "version-title founding"),
                  span(class = "version-badge founding", "FOUNDING")
                ),
                p(class = "version-description founding",
                  "The first version we weren't horrendously ashamed of! Basic hitter analysis comparing 2025 stats to recent history."
                )
              )
            )
          )
        )
      ),
      
    
      
      # Data & Tech Card
      card(
        class = "tech-card",
        card_header(
          icon("database"), "Data & Technology"
        ),
        card_body(
          div(
            class = "tech-grid",
            div(
              class = "tech-item",
              h5("📊 Data Sources"),
              tags$ul(
                tags$li(tags$strong("FanGraphs:"), " Comprehensive baseball statistics and advanced metrics"),
                tags$li(tags$strong("MLB:"), " Official player photos and league data"),
                tags$li(tags$strong("Daily Updates:"), " Fresh statistics refreshed every morning")
              )
            ),
            div(
              class = "tech-item",
              h5("🤖 AI & Analysis"),
              tags$ul(
                tags$li(tags$strong("GPT-4.1:"), " Advanced language model for intelligent analysis"),
                tags$li(tags$strong("Smart Caching:"), " Faster responses and cost optimization"),
                tags$li(tags$strong("Context Aware:"), " Analysis considers age, position, and sample size")
              )
            ),
            div(
              class = "tech-item",
              h5("⚙️ Built With"),
              tags$ul(
                tags$li(tags$strong("R & Shiny:"), " Robust statistical computing and web framework"),
                tags$li(tags$strong("Tidyverse:"), " Clean, maintainable data processing"),
                tags$li(tags$strong("Bootstrap & Custom CSS:"), " Modern, responsive design")
              )
            ),
            div(
              class = "tech-item",
              h5("📈 What We Compare"),
              tags$ul(
                tags$li(tags$strong("2025 Season:"), " Current year performance"),
                tags$li(tags$strong("vs 2022-2024:"), " Three-year historical average"),
                tags$li(tags$strong("500+ Players:"), " Both hitters and pitchers")
              )
            )
          )
        )
      )
    ),
    
    # Footer with proper visual container
    div(
      class = "about-footer-wrapper",
      div(
        class = "about-footer",
        p("Built with ❤️ and ⚾ by Ryan Pollack"),
        p(
          class = "credits",
          "Player data courtesy of ", tags$strong("FanGraphs"), " • Photos courtesy of ", tags$strong("MLB"), 
          " • Powered by ", tags$strong("OpenAI GPT-4.1")
        )
      )
    )
  )
  
  
)

# Server
# Complete Server Logic with Internal UI Functions and Trends in Step 1
server <- function(input, output, session) {
  # Generate user ID on session start
  user_id <- generate_user_id(session)
  
  # Keep-alive ping handler
  observeEvent(input$keepalive_ping,
               {
                 if (!is.null(input$keepalive_ping)) {
                   timestamp <- as.POSIXct(input$keepalive_ping / 1000, origin = "1970-01-01")
                   cat(
                     "💓 Keep-alive ping received at:", format(timestamp, "%H:%M:%S"),
                     "- User:", substr(user_id, 1, 8), "...\n"
                   )
                   session$userData$last_keepalive <- Sys.time()
                 }
               },
               ignoreInit = TRUE,
               ignoreNULL = TRUE
  )
  
  # Session ended handler
  session$onSessionEnded(function() {
    cat("📤 Session ended for user:", substr(user_id, 1, 8), "...\n")
  })
  
  # Load data on startup
  baseball_data <- load_baseball_data_cached()
  
  # Initialize reactive values with safe defaults
  values <- reactiveValues(
    selected_player_info = NULL,
    analysis_mode = "default", # Start with default
    trends_plot = NULL,
    ai_analysis_result = NULL,
    ai_analysis_loading = FALSE,
    current_analysis_key = "",
    last_logged_key = ""
  )
  
  # UI update trigger for forcing refreshes
  ui_update_trigger <- reactiveVal(0)
  
  # ============================================================================
  # INTERNAL UI GENERATION FUNCTIONS (moved inside server for proper scoping)
  # ============================================================================
  
  # Generate Step 1: Player Selection UI (with trends plot)
  generate_step_1_ui <- function(player_selected = FALSE, player_info = NULL, trends_plot = NULL, 
                                 ai_loading = FALSE, ai_result = NULL, analysis_mode = "default") {
    step_class <- if (player_selected) "step-card active" else "step-card inactive"
    
    div(
      class = step_class,
      div(
        class = "step-header",
        div(class = if (player_selected) "step-number" else "step-number inactive", "1"),
        h3(class = if (player_selected) "step-title" else "step-title inactive", "Player Selected"),
        # AI Analysis status badge in header
        if (player_selected) {
          if (ai_loading) {
            span(
              class = "badge bg-primary ms-3",
              tags$i(class = "fas fa-spinner fa-spin me-1"),
              "Analyzing..."
            )
          } else if (!is.null(ai_result)) {
            span(
              class = "badge bg-success ms-3",
              tags$i(class = "fas fa-check-circle me-1"),
              "Analysis Ready"
            )
          }
        }
      ),
      if (player_selected && !is.null(player_info)) {
        tagList(
          # INSTANT: Player card with photo
          div(
            class = "player-preview",
            img(
              src = player_info$photo_url %||% "https://via.placeholder.com/60x60/2E86AB/ffffff?text=⚾",
              alt = str_glue("Photo of {player_info$name}"),
              class = "player-preview-avatar",
              onerror = "this.src='https://via.placeholder.com/60x60/2E86AB/ffffff?text=⚾';"
            ),
            div(
              class = "player-preview-info",
              h4(player_info$name),
              p(str_glue("Age: {player_info$age %||% 'N/A'} • {player_info$position_info}"))
            )
          ),
          
          # INSTANT: Quick statistical insight
          div(
            class = "insight-summary",
            h5(icon("lightbulb"), "Summary Assessment"),
            p(player_info$quick_insight)
          ),
          
          # INSTANT: Performance trends plot (MOVED FROM STEP 3)
          if (!is.null(trends_plot)) {
            div(
              style = "margin-top: 1rem;",
              h5("Performance Trends", style = "color: #2E86AB; margin-bottom: 1rem;"),
              renderPlot(trends_plot, height = 300)
            )
          },
          
          # AI Analysis status section
          if (ai_loading) {
            div(
              class = "alert alert-info mt-3",
              div(
                class = "d-flex align-items-center",
                div(class = "spinner-border spinner-border-sm text-primary me-3", role = "status"),
                div(
                  tags$strong("Detailed AI analysis in progress..."),
                  tags$br(),
                  tags$small(
                    class = "text-muted",
                    str_glue("Generating {analysis_mode} analysis. Usually takes 5-15 seconds.")
                  )
                )
              )
            )
          } else if (!is.null(ai_result)) {
            div(
              class = "alert alert-success mt-3",
              div(
                class = "d-flex align-items-center justify-content-between",
                div(
                  class = "d-flex align-items-center",
                  tags$i(class = "fas fa-check-circle text-success me-2"),
                  div(
                    tags$strong("AI analysis complete!"),
                    tags$br(),
                    tags$small(class = "text-muted", "Scroll down to see detailed AI insights.")
                  )
                ),
                tags$button(
                  class = "btn btn-sm btn-outline-success",
                  onclick = "document.querySelector('.analysis-content').scrollIntoView({behavior: 'smooth', block: 'start'});",
                  tags$i(class = "fas fa-arrow-down me-1"),
                  "View AI Analysis"
                )
              )
            )
          }
        )
      } else {
        div(
          class = "empty-state",
          icon("search", class = "empty-icon"),
          h4(class = "empty-title", "Select a player above"),
          p(class = "empty-subtitle", "Choose from over 500 MLB players to get started")
        )
      }
    )
  }
  
  # Generate Step 2: Analysis Style UI
  # Generate Step 2: Analysis Style UI - COMPACT VERSION
  generate_step_2_ui <- function(player_selected = FALSE, current_mode = "default") {
    step_class <- if (player_selected) "step-card active" else "step-card inactive"
    number_class <- if (player_selected) "step-number" else "step-number inactive"
    title_class <- if (player_selected) "step-title" else "step-title inactive"
    
    vibe_options <- list(
      list(mode = "default", icon = "📊", name = "Straightforward"),
      list(mode = "analytics_dork", icon = "🤓", name = "Analytics Dork"),
      list(mode = "old_coot", icon = "👴", name = "Old Coot"),
      list(mode = "gen_z", icon = "🔥", name = "Gen Z"),
      list(mode = "seventies", icon = "🥸", name = "1970s Fan"),
      list(mode = "sensationalist", icon = "📰", name = "Sensationalist"),
      list(mode = "shakespeare", icon = "🎭", name = "Shakespeare"),
      list(mode = "rose_colored_glasses", icon = "🌹", name = "Rose-colored")
    )
    
    div(
      class = step_class,
      div(
        class = "step-header",
        div(class = number_class, "2"),
        h3(class = title_class, "Choose Analysis Vibe")
      ),
      if (player_selected) {
        tagList(
          # Desktop/Tablet: Compact 2x4 grid
          div(
            class = "vibe-selector d-none d-md-flex",
            div(
              class = "vibe-row",
              map(vibe_options[1:4], ~ {
                card_class <- if (.x$mode == current_mode) "vibe-card-compact selected" else "vibe-card-compact"
                div(
                  class = card_class,
                  `data-mode` = .x$mode,
                  onclick = str_glue("Shiny.setInputValue('analysis_mode', '{.x$mode}', {{priority: 'event'}});"),
                  div(class = "vibe-icon-compact", .x$icon),
                  div(class = "vibe-name-compact", .x$name)
                )
              })
            ),
            div(
              class = "vibe-row",
              map(vibe_options[5:8], ~ {
                card_class <- if (.x$mode == current_mode) "vibe-card-compact selected" else "vibe-card-compact"
                div(
                  class = card_class,
                  `data-mode` = .x$mode,
                  onclick = str_glue("Shiny.setInputValue('analysis_mode', '{.x$mode}', {{priority: 'event'}});"),
                  div(class = "vibe-icon-compact", .x$icon),
                  div(class = "vibe-name-compact", .x$name)
                )
              })
            )
          ),
          
          # Mobile Option A: Compact vertical stack
          div(
            class = "vibe-selector-mobile d-md-none",
            map(vibe_options, ~ {
              option_class <- if (.x$mode == current_mode) "vibe-option-mobile selected" else "vibe-option-mobile"
              div(
                class = option_class,
                `data-mode` = .x$mode,
                onclick = str_glue("Shiny.setInputValue('analysis_mode', '{.x$mode}', {{priority: 'event'}});"),
                span(class = "vibe-option-icon", .x$icon),
                div(
                  class = "vibe-option-text",
                  div(class = "vibe-option-name", .x$name),
                  div(class = "vibe-option-desc", 
                      case_when(
                        .x$mode == "default" ~ "Clear, data-driven analysis",
                        .x$mode == "analytics_dork" ~ "Modern stats, dismissive vibes",
                        .x$mode == "old_coot" ~ "Grumpy old-school wisdom",
                        .x$mode == "gen_z" ~ "Modern slang and trends",
                        .x$mode == "seventies" ~ "Retro baseball perspective",
                        .x$mode == "sensationalist" ~ "Dramatic sports journalism",
                        .x$mode == "shakespeare" ~ "Iambic pentameter analysis",
                        .x$mode == "rose_colored_glasses" ~ "Always positive",
                        TRUE ~ ""
                      )
                  )
                )
              )
            })
          )
          
      
        )
      } else {
        div(
          class = "empty-state",
          icon("palette", class = "empty-icon"),
          h4(class = "empty-title", "Analysis styles will appear here"),
          p(class = "empty-subtitle", "First select a player to continue")
        )
      }
    )
  }
  
  # Generate Step 3: Analysis Results UI (without trends plot)
  generate_step_3_ui <- function(player_selected = FALSE, analysis_mode = NULL,
                                 ai_loading = FALSE, ai_result = NULL) {
    both_selected <- player_selected && !is.null(analysis_mode)
    
    div(
      class = if (both_selected) "step-card active" else "step-card inactive",
      div(
        class = "step-header",
        div(class = if (both_selected) "step-number" else "step-number inactive", "3"),
        h3(class = if (both_selected) "step-title" else "step-title inactive", "Detailed Analysis")
      ),
      if (both_selected) {
        # DYNAMIC: AI Analysis section only (trends plot now in Step 1)
        if (!is.null(ai_result)) {
          # COMPLETE: Show AI analysis
          tagList(
            div(class = "analysis-content", ai_result)
          )
        } else if (ai_loading) {
          # LOADING: Show progress with context
          tagList(
            div(
              class = "loading-state",
              div(
                class = "d-flex align-items-center",
                div(class = "spinner-border text-primary me-3", role = "status"),
                div(
                  h5("Analyzing with AI..."),
                  p(
                    class = "text-muted mb-0",
                    str_glue("Generating {analysis_mode} analysis. This typically takes 5-15 seconds.")
                  )
                )
              )
            )
          )
        } else {
          # READY: Analysis will start automatically
          tagList(
            div(
              class = "empty-state",
              icon("robot", class = "empty-icon"),
              h4(class = "empty-title", "Analyzing player..."),
              p(class = "empty-subtitle", "AI analysis will be available shortly")
            )
          )
        }
      } else {
        div(
          class = "empty-state",
          icon("robot", class = "empty-icon"),
          h4(class = "empty-title", "AI analysis will appear here"),
          p(class = "empty-subtitle", "Complete the steps above to get started")
        )
      }
    )
  }
  
  # ============================================================================
  # HELPER FUNCTIONS (moved inside server for proper scoping)
  # ============================================================================
  
  # Generate quick insight based on player data
  generate_quick_insight <- function(player_data, player_type) {
    # Safety checks
    if (is.null(player_data) || nrow(player_data) == 0) {
      return("Player data loaded and ready for analysis.")
    }
    
    if (is.null(player_type) || !player_type %in% c("hitter", "pitcher")) {
      return("Player statistics available for detailed analysis.")
    }
    
    # Wrap the analysis logic in tryCatch for safety
    tryCatch(
      {
        positive_changes <- c()
        negative_changes <- c()
        
        if (player_type == "hitter") {
          # Positive changes for hitters - with proper NA checks
          if (!is.na(player_data$AVG_diff) && is.numeric(player_data$AVG_diff) && player_data$AVG_diff > 0.02) {
            positive_changes <- c(positive_changes, "batting average is up")
          }
          if (!is.na(player_data$K_pct_diff) && is.numeric(player_data$K_pct_diff) && player_data$K_pct_diff < -2) {
            positive_changes <- c(positive_changes, "strikeouts are down")
          }
          if (!is.na(player_data$BB_pct_diff) && is.numeric(player_data$BB_pct_diff) && player_data$BB_pct_diff > 1.5) {
            positive_changes <- c(positive_changes, "walks are up")
          }
          if (!is.na(player_data$Barrel_pct_diff) && is.numeric(player_data$Barrel_pct_diff) && player_data$Barrel_pct_diff > 2) {
            positive_changes <- c(positive_changes, "hard contact is up")
          }
          
          # Negative changes for hitters - with proper NA checks
          if (!is.na(player_data$AVG_diff) && is.numeric(player_data$AVG_diff) && player_data$AVG_diff < -0.02) {
            negative_changes <- c(negative_changes, "batting average is down")
          }
          if (!is.na(player_data$K_pct_diff) && is.numeric(player_data$K_pct_diff) && player_data$K_pct_diff > 2) {
            negative_changes <- c(negative_changes, "strikeouts are up")
          }
          if (!is.na(player_data$BB_pct_diff) && is.numeric(player_data$BB_pct_diff) && player_data$BB_pct_diff < -1.5) {
            negative_changes <- c(negative_changes, "walks are down")
          }
          if (!is.na(player_data$Barrel_pct_diff) && is.numeric(player_data$Barrel_pct_diff) && player_data$Barrel_pct_diff < -2) {
            negative_changes <- c(negative_changes, "hard contact is down")
          }
        } else { # pitcher - with proper NA checks
          # Positive changes for pitchers
          if (!is.na(player_data$era_diff) && is.numeric(player_data$era_diff) && player_data$era_diff < -0.5) {
            positive_changes <- c(positive_changes, "ERA is down")
          }
          if (!is.na(player_data$k_percent_diff) && is.numeric(player_data$k_percent_diff) && player_data$k_percent_diff > 2) {
            positive_changes <- c(positive_changes, "strikeouts are up")
          }
          if (!is.na(player_data$bb_percent_diff) && is.numeric(player_data$bb_percent_diff) && player_data$bb_percent_diff < -1.5) {
            positive_changes <- c(positive_changes, "walks allowed are down")
          }
          if (!is.na(player_data$barrel_percent_diff) && is.numeric(player_data$barrel_percent_diff) && player_data$barrel_percent_diff < -1) {
            positive_changes <- c(positive_changes, "hard contact allowed is down")
          }
          
          # Negative changes for pitchers
          if (!is.na(player_data$era_diff) && is.numeric(player_data$era_diff) && player_data$era_diff > 0.5) {
            negative_changes <- c(negative_changes, "ERA is up")
          }
          if (!is.na(player_data$k_percent_diff) && is.numeric(player_data$k_percent_diff) && player_data$k_percent_diff < -2) {
            negative_changes <- c(negative_changes, "strikeouts are down")
          }
          if (!is.na(player_data$bb_percent_diff) && is.numeric(player_data$bb_percent_diff) && player_data$bb_percent_diff > 1.5) {
            negative_changes <- c(negative_changes, "walks allowed are up")
          }
          if (!is.na(player_data$barrel_percent_diff) && is.numeric(player_data$barrel_percent_diff) && player_data$barrel_percent_diff > 1) {
            negative_changes <- c(negative_changes, "hard contact allowed is up")
          }
        }
        
        # Calculate regression likelihood safely
        regression_risk <- assess_regression_likelihood(player_data, player_type)
        
        # Build the insight message
        performance_text <- ""
        if (length(positive_changes) > 0 && length(negative_changes) > 0) {
          # Mixed performance
          pos_text <- if (length(positive_changes) > 1) {
            str_c(positive_changes[1:min(2, length(positive_changes))], collapse = " and ")
          } else {
            positive_changes[1]
          }
          neg_text <- if (length(negative_changes) > 1) {
            str_c(negative_changes[1:min(2, length(negative_changes))], collapse = " and ")
          } else {
            negative_changes[1]
          }
          performance_text <- str_glue("Mixed performance: {pos_text}, but {neg_text}.")
        } else if (length(positive_changes) > 0) {
          # Only positive changes
          if (length(positive_changes) == 1) {
            performance_text <- str_glue("Improved performance: {positive_changes[1]}.")
          } else if (length(positive_changes) == 2) {
            performance_text <- str_glue("Improved performance: {positive_changes[1]} and {positive_changes[2]}.")
          } else {
            performance_text <- str_glue("Strong improvement: {positive_changes[1]}, {positive_changes[2]}, and more.")
          }
        } else if (length(negative_changes) > 0) {
          # Only negative changes
          if (length(negative_changes) == 1) {
            performance_text <- str_glue("Concerning trend: {negative_changes[1]}.")
          } else if (length(negative_changes) == 2) {
            performance_text <- str_glue("Concerning trends: {negative_changes[1]} and {negative_changes[2]}.")
          } else {
            performance_text <- str_glue("Multiple concerns: {negative_changes[1]}, {negative_changes[2]}, and more.")
          }
        } else {
          # No significant changes
          performance_text <- "Performance is similar to recent seasons."
        }
        
        # Combine performance text with regression assessment
        return(str_glue("{performance_text} Likelihood of regression: {regression_risk}."))
      },
      error = function(e) {
        cat("❌ Error in generate_quick_insight:", e$message, "\n")
        return("Player data available for detailed analysis.")
      }
    )
  }
  
  # Assess regression likelihood based on luck indicators
  assess_regression_likelihood <- function(player_data, player_type) {
    if (is.null(player_data) || nrow(player_data) == 0) {
      return("low")
    }
    
    luck_indicators <- c()
    
    if (player_type == "hitter") {
      # BABIP analysis - Proper NA handling
      babip_cur <- player_data$BABIP_cur
      babip_l3 <- player_data$BABIP_l3
      
      if (!is.null(babip_cur) && !is.null(babip_l3) &&
          !is.na(babip_cur) && !is.na(babip_l3) &&
          is.numeric(babip_cur) && is.numeric(babip_l3)) {
        babip_diff <- babip_cur - babip_l3
        if (!is.na(babip_diff) && abs(babip_diff) > 0.025) {
          if (babip_diff > 0) {
            luck_indicators <- c(luck_indicators, "high_babip")
          } else {
            luck_indicators <- c(luck_indicators, "low_babip")
          }
        }
      }
      
      # xwOBA-wOBA gap analysis - Proper NA handling
      current_gap <- player_data$xwOBA_wOBA_gap_cur
      historical_gap <- player_data$xwOBA_wOBA_gap_l3
      
      if (!is.null(current_gap) && !is.na(current_gap) && is.numeric(current_gap)) {
        historical_gap <- if (is.null(historical_gap) || is.na(historical_gap)) 0 else historical_gap
        
        if (abs(current_gap) > 0.015 || abs(current_gap - historical_gap) > 0.020) {
          if (current_gap < -0.015) {
            luck_indicators <- c(luck_indicators, "lucky_woba")
          } else if (current_gap > 0.015) {
            luck_indicators <- c(luck_indicators, "unlucky_woba")
          }
        }
      }
      
      # Barrel rate vs BABIP inconsistency - Proper NA handling
      barrel_diff <- player_data$Barrel_pct_diff
      babip_diff <- player_data$BABIP_diff
      
      if (!is.null(barrel_diff) && !is.null(babip_diff) &&
          !is.na(barrel_diff) && !is.na(babip_diff) &&
          is.numeric(barrel_diff) && is.numeric(babip_diff)) {
        if ((barrel_diff < -1 && babip_diff > 0.02) ||
            (barrel_diff > 1 && babip_diff < -0.02)) {
          luck_indicators <- c(luck_indicators, "contact_babip_mismatch")
        }
      }
    } else { # pitcher
      # BABIP analysis - Proper NA handling
      babip_cur <- player_data$babip_cur
      babip_l3 <- player_data$babip_l3
      
      if (!is.null(babip_cur) && !is.null(babip_l3) &&
          !is.na(babip_cur) && !is.na(babip_l3) &&
          is.numeric(babip_cur) && is.numeric(babip_l3)) {
        babip_diff <- babip_cur - babip_l3
        if (!is.na(babip_diff) && abs(babip_diff) > 0.025) {
          if (babip_diff < 0) {
            luck_indicators <- c(luck_indicators, "lucky_babip")
          } else {
            luck_indicators <- c(luck_indicators, "unlucky_babip")
          }
        }
      }
      
      # ERA vs xERA gap - Proper NA handling
      era_cur <- player_data$era_cur
      xera_cur <- player_data$xera_cur
      
      if (!is.null(era_cur) && !is.null(xera_cur) &&
          !is.na(era_cur) && !is.na(xera_cur) &&
          is.numeric(era_cur) && is.numeric(xera_cur)) {
        era_xera_gap <- era_cur - xera_cur
        if (!is.na(era_xera_gap) && abs(era_xera_gap) > 0.30) {
          if (era_xera_gap < -0.30) {
            luck_indicators <- c(luck_indicators, "lucky_era")
          } else if (era_xera_gap > 0.30) {
            luck_indicators <- c(luck_indicators, "unlucky_era")
          }
        }
      }
      
      # LOB% analysis - Proper NA handling
      lob_cur <- player_data$lob_percent_cur
      lob_l3 <- player_data$lob_percent_l3
      
      if (!is.null(lob_cur) && !is.null(lob_l3) &&
          !is.na(lob_cur) && !is.na(lob_l3) &&
          is.numeric(lob_cur) && is.numeric(lob_l3)) {
        lob_diff <- lob_cur - lob_l3
        if (!is.na(lob_diff) && abs(lob_diff) > 3) {
          if (lob_diff > 3) {
            luck_indicators <- c(luck_indicators, "high_lob")
          } else if (lob_diff < -3) {
            luck_indicators <- c(luck_indicators, "low_lob")
          }
        }
      }
      
      # Barrel rate vs ERA inconsistency - Proper NA handling
      barrel_diff <- player_data$barrel_percent_diff
      era_diff <- player_data$era_diff
      
      if (!is.null(barrel_diff) && !is.null(era_diff) &&
          !is.na(barrel_diff) && !is.na(era_diff) &&
          is.numeric(barrel_diff) && is.numeric(era_diff)) {
        if ((barrel_diff > 1 && era_diff < -0.5) ||
            (barrel_diff < -1 && era_diff > 0.5)) {
          luck_indicators <- c(luck_indicators, "barrel_era_mismatch")
        }
      }
    }
    
    # Determine regression likelihood based on luck indicators
    luck_count <- length(luck_indicators)
    
    if (luck_count == 0) {
      return("low")
    }
    
    lucky_indicators <- sum(str_detect(luck_indicators, "lucky|high_babip|high_lob"))
    unlucky_indicators <- sum(str_detect(luck_indicators, "unlucky|low_babip|low_lob"))
    mismatch_indicators <- sum(str_detect(luck_indicators, "mismatch"))
    
    if (luck_count >= 3 || mismatch_indicators >= 2) {
      return("high")
    } else if (luck_count == 2 || (lucky_indicators >= 1 && unlucky_indicators >= 1)) {
      return("medium")
    } else if (luck_count == 1) {
      return("medium")
    } else {
      return("low")
    }
  }
  
  # ============================================================================
  # DATA AND REACTIVE LOGIC
  # ============================================================================
  
  # Update player choices using tidyverse
  observe({
    if (nrow(baseball_data$lookup) > 0) {
      player_choices <- if ("compound_id" %in% colnames(baseball_data$lookup)) {
        setNames(baseball_data$lookup$compound_id, baseball_data$lookup$display_name)
      } else {
        setNames(baseball_data$lookup$PlayerId, baseball_data$lookup$display_name)
      }
      
      updateSelectInput(session, "player_selection",
                        choices = c("Select a player..." = "", player_choices)
      )
    } else {
      updateSelectInput(session, "player_selection",
                        choices = c("⚠️ Data not loaded - check logs" = "")
      )
    }
  })
  
  # IMMEDIATE: React to player selection - UPDATE UI INSTANTLY
  observeEvent(input$player_selection,
               {
                 if (!is.null(input$player_selection) && input$player_selection != "") {
                   player_info <- get_player_info(input$player_selection, baseball_data)
                   
                   if (!is.null(player_info)) {
                     # Get player data for quick insight
                     actual_player_id <- if ("compound_id" %in% colnames(baseball_data$lookup)) {
                       extract_player_id(input$player_selection)
                     } else {
                       input$player_selection
                     }
                     
                     if (player_info$type == "hitter" && nrow(baseball_data$hitters) > 0) {
                       player_data <- baseball_data$hitters %>% filter(PlayerId == actual_player_id)
                     } else if (player_info$type == "pitcher" && nrow(baseball_data$pitchers) > 0) {
                       player_data <- baseball_data$pitchers %>% filter(PlayerId == actual_player_id)
                     } else {
                       player_data <- NULL
                     }
                     
                     # Generate quick insight safely
                     quick_insight <- if (!is.null(player_data) && nrow(player_data) > 0) {
                       generate_quick_insight(player_data, player_info$type)
                     } else {
                       "Player data available for analysis."
                     }
                     
                     # INSTANT UPDATE: Store player info + quick insight
                     values$selected_player_info <- list(
                       name = player_info$name,
                       type = player_info$type,
                       age = player_info$age,
                       position_info = player_info$position_info,
                       photo_url = get_player_photo_url(input$player_selection, baseball_data),
                       quick_insight = quick_insight
                     )
                     
                     # INSTANT: Generate and store trends plot (fast, no API needed)
                     values$trends_plot <- create_player_trends_plot(input$player_selection, baseball_data)
                     
                     # Clear AI analysis state
                     values$ai_analysis_result <- NULL
                     values$ai_analysis_loading <- FALSE
                     
                     # Force UI update immediately
                     ui_update_trigger(ui_update_trigger() + 1)
                     
                     cat("✅ INSTANT: Player info loaded for:", player_info$name, "\n")
                     
                     # IMMEDIATE LOGGING AND AI TRIGGER - Use default mode if none selected
                     current_mode <- if (is.null(values$analysis_mode) || values$analysis_mode == "") {
                       "default"
                     } else {
                       values$analysis_mode
                     }
                     
                     analysis_key <- paste(player_info$name, current_mode, sep = "_")
                     if (analysis_key != values$last_logged_key) {
                       log_if_not_admin(session, player_info$name, current_mode)
                       values$last_logged_key <- analysis_key
                       cat("📊 IMMEDIATE LOG: Player selected, triggering analysis with mode:", current_mode, "\n")
                     }
                   }
                 } else {
                   values$selected_player_info <- NULL
                   values$trends_plot <- NULL
                   values$ai_analysis_result <- NULL
                   values$ai_analysis_loading <- FALSE
                   ui_update_trigger(ui_update_trigger() + 1)
                   cat("🗑️ Player selection cleared\n")
                 }
               },
               ignoreInit = TRUE
  )
  
  # IMMEDIATE: React to analysis mode selection
  observeEvent(input$analysis_mode,
               {
                 if (!is.null(input$analysis_mode)) {
                   cat("🎨 Analysis mode changed to:", input$analysis_mode, "\n")
                   values$analysis_mode <- input$analysis_mode
                   
                   # Clear previous AI analysis when mode changes
                   values$ai_analysis_result <- NULL
                   values$ai_analysis_loading <- FALSE
                   
                   # Force UI update immediately
                   ui_update_trigger(ui_update_trigger() + 1)
                   
                   # IMMEDIATE LOGGING: If player is already selected, log now
                   if (!is.null(values$selected_player_info)) {
                     analysis_key <- paste(values$selected_player_info$name, input$analysis_mode, sep = "_")
                     if (analysis_key != values$last_logged_key) {
                       log_if_not_admin(session, values$selected_player_info$name, input$analysis_mode)
                       values$last_logged_key <- analysis_key
                       cat("📊 IMMEDIATE LOG: Vibe changed with existing player\n")
                     }
                   }
                 }
               },
               ignoreInit = TRUE
  )
  
  # SEPARATE ASYNC OBSERVER - AI Analysis Generation
  observeEvent(
    {
      list(values$selected_player_info, values$analysis_mode)
    },
    {
      # Use isolate() to prevent this from blocking other reactive updates
      selected_info <- isolate(values$selected_player_info)
      analysis_mode <- isolate(values$analysis_mode)
      player_selection <- isolate(input$player_selection)
      
      # Allow default mode to trigger AI analysis
      if (!is.null(selected_info) &&
          !is.null(analysis_mode) &&
          !is.null(player_selection) &&
          player_selection != "") {
        analysis_key <- paste(selected_info$name, analysis_mode, sep = "_")
        current_key <- isolate(values$current_analysis_key)
        
        if (analysis_key != current_key) {
          cat("🎯 ASYNC: Starting AI analysis for:", analysis_key, "\n")
          
          # Set loading state immediately
          values$ai_analysis_loading <- TRUE
          values$ai_analysis_result <- NULL
          values$current_analysis_key <- analysis_key
          
          # Generate AI analysis asynchronously
          later::later(function() {
            tryCatch(
              {
                cat("🤖 Generating AI analysis...\n")
                
                analysis_result <- analyze_player_performance(
                  player_selection,
                  analysis_mode,
                  baseball_data
                )
                
                # Update when complete
                values$ai_analysis_result <- analysis_result
                values$ai_analysis_loading <- FALSE
                
                cat("✅ ASYNC: AI analysis complete for:", analysis_key, "\n")
              },
              error = function(e) {
                cat("❌ ASYNC: Error in AI analysis:", e$message, "\n")
                values$ai_analysis_loading <- FALSE
                values$ai_analysis_result <- HTML(paste0(
                  "<div class='alert alert-danger'>",
                  "Error generating analysis: ", e$message,
                  "</div>"
                ))
              }
            )
          }, delay = 0.1)
        } else {
          cat("♻️ ASYNC: Using existing analysis for:", analysis_key, "\n")
        }
      }
    },
    ignoreInit = TRUE
  )
  
  # ============================================================================
  # UI OUTPUTS USING INTERNAL FUNCTIONS
  # ============================================================================
  
  # Render Step 1: Player Selection (using internal function)
  output$step_1_player_selection <- renderUI({
    ui_update_trigger()  # Access reactive trigger
    
    player_selected <- !is.null(values$selected_player_info)
    
    generate_step_1_ui(
      player_selected = player_selected,
      player_info = values$selected_player_info,
      trends_plot = values$trends_plot,
      ai_loading = isTRUE(values$ai_analysis_loading),
      ai_result = values$ai_analysis_result,
      analysis_mode = values$analysis_mode %||% "default"
    )
  })
  
  # Render Step 2: Analysis Style (using internal function)
  output$step_2_analysis_style <- renderUI({
    ui_update_trigger()
    
    player_selected <- !is.null(values$selected_player_info)
    
    generate_step_2_ui(
      player_selected = player_selected, 
      current_mode = values$analysis_mode %||% "default"
    )
  })
  
  # Render Step 3: Analysis Results (using internal function)
  output$step_3_analysis_results <- renderUI({
    ui_update_trigger()
    
    player_selected <- !is.null(values$selected_player_info)
    
    generate_step_3_ui(
      player_selected = player_selected,
      analysis_mode = values$analysis_mode,
      ai_loading = isTRUE(values$ai_analysis_loading),
      ai_result = values$ai_analysis_result
    )
  })
  
  # Force UI outputs to not suspend when hidden
  outputOptions(output, "step_1_player_selection", suspendWhenHidden = FALSE)
  outputOptions(output, "step_2_analysis_style", suspendWhenHidden = FALSE)
  outputOptions(output, "step_3_analysis_results", suspendWhenHidden = FALSE)
}


# Application Initialization -----------------------------------------------

shinyApp(ui = ui, server = server)
