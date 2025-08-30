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
    if (player_info$type == "pitcher" && !is.na(player_info$tbf)) {
      str_glue("• TBF: {player_info$tbf}")
    } else if (player_info$type == "hitter" && !is.na(player_info$pa)) {
      str_glue("• PA: {player_info$pa}")
    }
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

