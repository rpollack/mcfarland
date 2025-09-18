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

#' Build an OpenAI prompt for comparing players
#'
#' Construct a text summary of the selected players that can be sent to
#' OpenAI for ranking. The prompt includes core statistics for each player
#' and asks the model to order them by future outlook.
#'
#' @param player_ids Character vector of player IDs
#' @param baseball_data Loaded baseball data list
#' @return Character string prompt or NULL if no players
build_comparison_prompt <- function(player_ids, baseball_data) {
  ids <- player_ids[!is.na(player_ids) & player_ids != ""]
  if (length(ids) == 0) {
    return(NULL)
  }

  first_type <- get_player_info(ids[[1]], baseball_data)$type
  actual_ids <- purrr::map_chr(ids, extract_player_id)

  if (first_type == "hitter") {
    lines <- baseball_data$hitters %>%
      dplyr::filter(PlayerId %in% actual_ids) %>%
      dplyr::mutate(line = stringr::str_glue(
        "{Name}: PA {format_stat_value(PA_cur)}, AVG {format_stat_value(AVG_cur)}, "
        , "OBP {format_stat_value(OBP_cur)}, SLG {format_stat_value(SLG_cur)}, "
        , "wOBA {format_stat_value(wOBA_cur)}, xwOBA {format_stat_value(xwOBA_cur)}, "
        , "K% {format_percentage(K_pct_cur)}, BB% {format_percentage(BB_pct_cur)}"
      )) %>%
      dplyr::pull(line)
  } else {
    lines <- baseball_data$pitchers %>%
      dplyr::filter(PlayerId %in% actual_ids) %>%
      dplyr::mutate(line = stringr::str_glue(
        "{Name}: ERA {format_era(era_cur)}, xERA {format_era(xera_cur)}, "
        , "K% {format_percentage(k_percent_cur)}, BB% {format_percentage(bb_percent_cur)}, "
        , "BABIP {format_stat_value(babip_cur)}, CSW% {format_percentage(csw_percent_cur)}"
      )) %>%
      dplyr::pull(line)
  }

  stringr::str_c(
    "Rank these players by who is most likely to perform best going forward. "
    , "Provide brief analysis explaining the order.",
    "\n\n", paste(lines, collapse = "\n")
  )
}

#' Generate AI analysis for player comparison
#'
#' Uses build_comparison_prompt and call_openai_api to obtain a ranking with
#' narrative context from OpenAI.
#'
#' @param player_ids Character vector of player IDs
#' @param baseball_data Loaded baseball data list
#' @param analysis_mode Persona or style for the analysis
#' @return HTML content with the AI response
analyze_player_comparison <- function(player_ids, baseball_data, analysis_mode) {
  prompt <- build_comparison_prompt(player_ids, baseball_data)
  if (is.null(prompt)) {
    return(htmltools::HTML("<div class='alert alert-warning'>No players to analyze.</div>"))
  }
  call_openai_api(prompt, analysis_mode, context = "comparison")
}

