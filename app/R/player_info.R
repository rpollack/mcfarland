# Player Information Functions ---------------------------------------------

#' Get player basic information using tidyverse
#' @param player_id FanGraphs player ID (can be compound or simple)
#' @param baseball_data Complete baseball data list
#' @return List with name, type, age, and PA or TBF
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
        pa = player_data$PA_cur
      ))
    }
  } else if (player_type == "pitcher" && nrow(baseball_data$pitchers) > 0) {
    player_data <- baseball_data$pitchers %>%
      filter(PlayerId == actual_player_id) %>%
      slice_head(n = 1)

    if (nrow(player_data) > 0) {
      return(list(
        name = player_name,
        type = player_type,
        age = player_data$Age,
        tbf = player_data$tbf
      ))
    }
  }

  # Fallback
  list(name = player_name, type = player_type, age = NA)
}

