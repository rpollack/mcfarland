# ==============================================================================
# McFARLAND: Machine-crafted Forecasting And Reasoning for 
#            Luck, Analytics, Narratives, and Data
# 
# A professional baseball analytics application with AI-powered analysis
# TIDYVERSE VERSION - Enhanced readability and maintainability + API CACHING
# ==============================================================================

# Updated configuration for the top of your app/app.R file
# Replace the existing Render configuration block with this:

cat("=== MCFARLAND APP STARTING ===\n")
cat("Timestamp:", as.character(Sys.time()), "\n")
cat("Working directory:", getwd(), "\n")
cat("R version:", R.version.string, "\n")

# RENDER.COM COMPATIBILITY CONFIGURATION
if (Sys.getenv("RENDER") == "true") {
  # Running on Render - options are set by the CMD in Dockerfile
  options(shiny.sanitize.errors = FALSE)
  options(shiny.trace = TRUE)  # Enable tracing to see errors
  cat("✓ Render config applied (host/port set by CMD)\n")
} else {
  # Local development
  options(shiny.autoreload = TRUE)
  cat("✓ Development mode\n")
}

cat("=== CHECKING ENVIRONMENT ===\n")
cat("RENDER env var:", Sys.getenv("RENDER"), "\n")
cat("OPENAI_API_KEY set:", nchar(Sys.getenv("OPENAI_API_KEY")) > 0, "\n")

cat("=== LOADING LIBRARIES ===\n")

# Load libraries with error handling
tryCatch({
  library(shiny)
  cat("✓ shiny loaded\n")
}, error = function(e) {
  cat("✗ shiny FAILED:", e$message, "\n")
  stop("Cannot load shiny package")
})

tryCatch({
  library(dplyr)
  cat("✓ dplyr loaded\n")
}, error = function(e) {
  cat("✗ dplyr FAILED:", e$message, "\n")
  stop("Cannot load dplyr package")
})

# Load other essential packages
for (pkg in c("readr", "purrr", "stringr", "httr", "jsonlite", 
              "bslib", "commonmark", "shinybusy", "ggplot2", "htmltools", "digest")) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat("✓", pkg, "loaded\n")
  }, error = function(e) {
    cat("✗", pkg, "FAILED:", e$message, "\n")
    stop(paste("Cannot load", pkg, "package"))
  })
}

# Load baseball-specific packages (these might fail)
for (pkg in c("baseballr", "stringi", "janitor")) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat("✓", pkg, "loaded\n")
  }, error = function(e) {
    cat("⚠", pkg, "FAILED:", e$message, "\n")
    cat("Continuing without", pkg, "\n")
  })
}

cat("=== LIBRARIES LOADED ===\n")

# Rest of your existing app.R code continues here...
# (All your existing functions, UI, server, etc.)

# Load Required Libraries --------------------------------------------------
library(shiny)          # Core Shiny framework
library(dplyr)          # Data manipulation (filter, select, case_when, etc.)
library(readr)          # Reading CSV files (read_csv)
library(purrr)          # Functional programming (map, compact, %||%)
library(stringr)        # String manipulation (str_glue, str_detect, etc.)
library(httr)           # HTTP requests for OpenAI API
library(jsonlite)       # JSON handling for API calls
library(bslib)          # Bootstrap theming (page_navbar, card, etc.)
library(commonmark)     # Markdown rendering
library(shinybusy)      # Loading indicators
library(ggplot2)        # Data visualization
library(htmltools)      # HTML utilities (HTML, htmlEscape)
library(digest)         # CACHE: Added for generating cache keys

# CACHE: Create global environment for API response caching
cache_env <- new.env()
cache_env$api_responses <- list()
cache_env$plots <- list()

# Configuration -------------------------------------------------------------
GITHUB_DATA_URL <- "https://raw.githubusercontent.com/rpollack/leadRboard/master/"
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

# Add this data caching system to your app.R
# Replace your existing load_baseball_data() calls with this cached version

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
  
  fresh_data <- tryCatch({
    load_baseball_data()  # Your existing function
  }, error = function(e) {
    cat("Error loading data:", e$message, "\n")
    # Return empty but valid structure
    list(
      hitters = tibble(),
      pitchers = tibble(),
      lookup = tibble(display_name = character(), PlayerId = character(), player_type = character())
    )
  })
  
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
  
  tryCatch({
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
    
    cat("Successfully loaded from GitHub:", 
        nrow(data_list$hitters), "hitters,", 
        nrow(data_list$pitchers), "pitchers\n")
    
    data_list
    
  }, error = function(e) {
    cat("Error loading from GitHub:", e$message, "\n")
    # Return empty but valid structure
    list(
      hitters = tibble(),
      pitchers = tibble(),
      lookup = tibble(display_name = character(), PlayerId = character(), player_type = character())
    )
  })
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
  
  if (nrow(player_info) == 0) return(NULL)
  
  # Extract actual player ID and determine dataset
  actual_player_id <- extract_player_id(player_id)
  dataset <- if (player_info$player_type == "hitter") {
    baseball_data$hitters
  } else {
    baseball_data$pitchers
  }
  
  if (nrow(dataset) == 0) return(NULL)
  
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
  
  if (nrow(player_lookup) == 0) return(NULL)
  
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
  if (is.null(player_info)) return(NULL)
  
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
           .default = metrics)
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
           .default = metrics)
  }
}

#' Create comparison plot for player trends using tidyverse
#' @param player_id FanGraphs player ID (can be compound or simple)
#' @param baseball_data Complete baseball data list
#' @return ggplot object or NULL
create_player_trends_plot <- function(player_id, baseball_data) {
  player_info <- get_player_info(player_id, baseball_data)
  if (is.null(player_info)) return(NULL)
  
  # Get appropriate dataset
  dataset <- if (player_info$type == "hitter") {
    baseball_data$hitters
  } else {
    baseball_data$pitchers
  }
  
  if (nrow(dataset) == 0) return(NULL)
  
  # Extract actual player ID
  actual_player_id <- if ("compound_id" %in% colnames(baseball_data$lookup)) {
    extract_player_id(player_id)
  } else {
    player_id
  }
  
  player_data <- dataset %>% filter(PlayerId == actual_player_id)
  if (nrow(player_data) == 0) return(NULL)
  
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
  
  if (nrow(comparison_data) == 0) return(NULL)
  
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
    shakespeare = "You are William Shakespeare. Not just that, but you speak in verse -- preferably iambic pentameter."
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
  
  result <- tryCatch({
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
    
  }, error = function(e) {
    HTML(str_glue("<div class='alert alert-danger'>Error: {e$message}</div>"))
  })
  
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
  if (nrow(data) == 0) return(NULL)
  
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
  if (nrow(data) == 0) return(NULL)
  
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
")

# UI Definition -------------------------------------------------------------

ui <- page_navbar(
  title = "McFARLAND: AI-Powered Baseball Analysis",  # Clean title for browser tab
  
  header = tagList(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0, user-scalable=no, viewport-fit=cover, shrink-to-fit=no"),
    tags$style(ui_styles),
    add_busy_bar(color = "#2E86AB", height = "25px"),
    
    # Custom JavaScript to update navbar brand with responsive stacked version
    tags$script(HTML("
      $(document).ready(function() {
        $('.navbar-brand').html(`
          <div class='d-flex flex-column'>
            <div class='fw-bold'>McFARLAND</div>
            <div class='navbar-subtitle'>🤖⚾ AI-powered baseball analysis</div>
          </div>
        `);
      });
    "))
  ),
  
  nav_panel(
    title = "Home",
    icon = icon("home"),
    
    layout_columns(
      col_widths = c(4, 8),
      
      card(
        card_body(
          selectInput(
            inputId = "player_selection",
            label = "Player:",
            choices = NULL,
            width = "100%"
          ),
          selectInput(
            inputId = "analysis_mode",
            label = "Vibe:",
            choices = c(
              "Straightforward" = "default",
              "Analytics Dork" = "analytics_dork",
              "Old Coot" = "old_coot",
              "Gen Z" = "gen_z",
              "1970s Fan" = "seventies",
              "Sensationalist" = "sensationalist",
              "Shakespeare" = "shakespeare"
            ),
            width = "100%"
          ),
          
          uiOutput("player_info_card")
        )
      ),
      
      card(
        card_header("Analysis"),
        card_body(
          uiOutput("result_output")
        )
      )
    )
  ),
  
  nav_panel(
    title = "About",
    icon = icon("info-circle"),
    card(
      card_body(
        p("McFARLAND: Machine-crafted Forecasting And Reasoning for Luck, Analytics, Narratives, and Data"),
        img(src = "tjmcfarland.png", style = "width: 100%; max-width: 400px; height: auto;"),
        p("Now supports both hitters and pitchers with MLB player headshots!"),
        p("Data from FanGraphs. Comparing 2025 stats (refreshed daily) to 2022-2024 averages."),
        p("Player photos courtesy of MLB."),
        p("Built with R, shiny, tidyverse, baseballr, bslib, shinyWidgets, and shinybusy."),
        p("Powered by GPT-4.1."),
        
        h4("Get In Touch"),
        tags$a(
          href = "https://docs.google.com/forms/d/e/1FAIpQLScPiHfO2XxwCXd2V-7pNsUKs-mMaqzzsH2ohA_kBflk_n8AQw/viewform",
          target = "_blank", class = "btn btn-sm btn-primary", style = "margin-left: 1rem;",
          icon("envelope"), "Get updated when we add new features"
        ),
        tags$a(
          href = "https://forms.gle/NDJJKj7XrsnFH6m16",
          target = "_blank", class = "btn btn-sm btn-primary", style = "margin-left: 1rem;",
          icon("envelope"), "Submit feedback"
        ),
        
        h4("Version History"),
        tags$ul(
          tags$li("0.8 - Added intelligent API caching for improved performance and cost savings"),
          tags$li("0.7 - Added player headshots from MLB! Photos now display with player info."),
          tags$li("0.6 - Added pitcher analysis! Now supports both hitters and pitchers."),
          tags$li("0.5 - Added ability to sign up for notifications."),
          tags$li("0.4 - Added Barrels/PA and historical xwOBA/wOBA gap to stats that are analyzed"),
          tags$li("0.3 - Added player stat graphs below analysis."),
          tags$li("0.2 - Added Shakespeare vibe."),
          tags$li("0.1 - First version I wasn't horrendously ashamed of.")
        )
      )
    )
  )
)

# Server Logic with Caching ------------------------------------------------

server <- function(input, output, session) {
  
  # Load data on startup
  baseball_data <- load_baseball_data_cached()
  
  # Update player choices using tidyverse
  observe({
    if (nrow(baseball_data$lookup) > 0) {
      # Use compound_id if available, fallback to PlayerId for backwards compatibility
      player_choices <- if ("compound_id" %in% colnames(baseball_data$lookup)) {
        setNames(baseball_data$lookup$compound_id, baseball_data$lookup$display_name)
      } else {
        setNames(baseball_data$lookup$PlayerId, baseball_data$lookup$display_name)
      }
      
      updateSelectInput(session, "player_selection", 
                        choices = c("Select a player..." = "", player_choices))
    } else {
      updateSelectInput(session, "player_selection", 
                        choices = c("⚠️ Data not loaded - check logs" = ""))
    }
  })
  
  # Player info card display
  output$player_info_card <- renderUI({
    if (input$player_selection == "") return(NULL)
    create_player_card(input$player_selection, baseball_data)
  })
  
  # Main analysis output with caching
  output$result_output <- renderUI({
    if (input$player_selection == "") {
      return(div(
        class = "text-center text-muted p-4",
        style = "background: rgba(255, 255, 255, 0.9); border-radius: 20px; backdrop-filter: blur(20px);",
        h5("⚾ Select a player to begin analysis", style = "color: #2E86AB; font-weight: 600;")
      ))
    }
    
    if (nrow(baseball_data$lookup) == 0) {
      return(div(
        class = "alert alert-warning",
        "No player data available. Make sure the GitHub Action has run successfully."
      ))
    }
    
    # CACHE: Track performance and cache effectiveness
    start_time <- Sys.time()
    
    # Perform analysis with progress indicator
    withProgress(message = 'Analyzing player performance...', value = 0, {
      incProgress(0.3, detail = "Checking cache...")
      analysis_result <- analyze_player_performance(input$player_selection, input$analysis_mode, baseball_data)
      incProgress(0.7, detail = "Creating visualization...")
      trends_plot <- create_player_trends_plot(input$player_selection, baseball_data)
      incProgress(1, detail = "Complete")
    })
    
    # Combine analysis and visualization
    tagList(
      div(class = "analysis-content", analysis_result),
      hr(style = "border-color: rgba(46, 134, 171, 0.3); margin: 2rem 0;"),
      renderPlot({
        if (is.null(trends_plot)) {
          # Empty state plot
          ggplot() + 
            geom_text(aes(x = 1, y = 1, label = "No trend data available"), 
                      size = 6, color = "#6c757d") +
            theme_void() +
            theme(plot.background = element_rect(fill = "white", color = NA))
        } else {
          trends_plot
        }
      }, height = 400)
    )
  })
}

# Application Initialization -----------------------------------------------

shinyApp(ui = ui, server = server)