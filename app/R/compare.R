# Player Comparison Functions -----------------------------------------------

#' Recommend the best player out of a group
#'
#' Given a set of player IDs and the loaded baseball data, determine which
#' player is most likely to perform well going forward. For hitters this uses
#' current xwOBA (higher is better); for pitchers it uses current xERA (lower
#' is better).
#'
#' @param player_ids Character vector of player IDs (compound or simple)
#' @param baseball_data List containing hitters, pitchers and lookup tables
#' @return PlayerId of the recommended player or NULL if input is empty
recommend_best_player <- function(player_ids, baseball_data) {
  ids <- player_ids[!is.na(player_ids) & player_ids != ""]
  if (length(ids) == 0) {
    return(NULL)
  }

  first_type <- get_player_info(ids[[1]], baseball_data)$type
  actual_ids <- purrr::map_chr(ids, extract_player_id)

  if (first_type == "hitter") {
    stats <- baseball_data$hitters %>%
      dplyr::filter(PlayerId %in% actual_ids) %>%
      dplyr::select(PlayerId, xwOBA_cur)
    best <- stats %>%
      dplyr::filter(xwOBA_cur == max(xwOBA_cur, na.rm = TRUE)) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::pull(PlayerId)
  } else {
    stats <- baseball_data$pitchers %>%
      dplyr::filter(PlayerId %in% actual_ids) %>%
      dplyr::select(PlayerId, xera_cur)
    best <- stats %>%
      dplyr::filter(xera_cur == min(xera_cur, na.rm = TRUE)) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::pull(PlayerId)
  }

  best
}

