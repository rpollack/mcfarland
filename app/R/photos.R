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

