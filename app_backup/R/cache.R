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

