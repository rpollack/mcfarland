# API Helper Functions ------------------------------------------------------

#' Convert a POSIXct time to an ISO 8601 string in UTC.
#'
#' @param time POSIXct or Date value
#' @return Character timestamp or NA_character_ if the input is NULL/NA
#' @keywords internal
iso8601_utc <- function(time) {
  if (is.null(time) || is.na(time)) {
    return(NA_character_)
  }

  as.character(format(as.POSIXct(time, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"))
}

# Internal utility to normalise numeric values for JSON responses.
normalize_numeric <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NA_real_)
  }

  numeric_value <- suppressWarnings(as.numeric(value[[1]]))
  if (length(numeric_value) == 0 || is.nan(numeric_value)) {
    return(NA_real_)
  }

  numeric_value
}

# Internal utility to normalise character fields (returning NA when empty).
normalize_character <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NA_character_)
  }

  char_value <- as.character(value[[1]])
  if (is.na(char_value) || char_value == "") {
    return(NA_character_)
  }

  char_value
}

# Safely extract a single value from a one-row tibble/data frame.
extract_row_value <- function(row, column) {
  if (!column %in% colnames(row) || nrow(row) == 0) {
    return(NULL)
  }
  row[[1, column, drop = TRUE]]
}

# Build a single metric entry for stat line payloads.
create_metric <- function(label, current, previous = NULL, diff = NULL,
                          formatter = format_stat_value, scale = "value") {
  current_value <- normalize_numeric(current)
  previous_value <- if (is.null(previous)) NULL else normalize_numeric(previous)
  diff_value <- if (is.null(diff)) NULL else normalize_numeric(diff)

  metric <- list(
    label = label,
    scale = scale,
    current = current_value,
    previous = if (is.null(previous)) NA_real_ else previous_value,
    diff = if (is.null(diff)) NA_real_ else diff_value,
    display = list(
      current = formatter(current_value)
    )
  )

  if (!is.null(previous)) {
    metric$display$previous <- formatter(previous_value)
  }

  if (!is.null(diff)) {
    metric$display$diff <- formatter(diff_value)
  }

  metric
}

# Resolve the lookup, info, and detailed stats for a given player ID.
resolve_player_record <- function(player_id, baseball_data) {
  if (is.null(player_id) || player_id == "" || is.null(baseball_data)) {
    return(NULL)
  }

  lookup <- baseball_data$lookup
  if (nrow(lookup) == 0) {
    return(NULL)
  }

  lookup_row <- if ("compound_id" %in% colnames(lookup)) {
    lookup %>% dplyr::filter(compound_id == player_id)
  } else {
    lookup %>% dplyr::filter(PlayerId == player_id)
  }

  if (nrow(lookup_row) == 0) {
    return(NULL)
  }

  info <- get_player_info(player_id, baseball_data)
  if (is.null(info)) {
    return(NULL)
  }

  actual_id <- if ("compound_id" %in% colnames(lookup)) {
    extract_player_id(player_id)
  } else {
    player_id
  }

  dataset <- dplyr::case_when(
    info$type == "hitter" ~ list(baseball_data$hitters),
    info$type == "pitcher" ~ list(baseball_data$pitchers),
    TRUE ~ list(tibble())
  )[[1]]

  if (nrow(dataset) == 0) {
    return(NULL)
  }

  stats_row <- dataset %>%
    dplyr::filter(PlayerId == actual_id) %>%
    dplyr::slice_head(n = 1)

  if (nrow(stats_row) == 0) {
    return(NULL)
  }

  list(
    lookup = lookup_row,
    info = info,
    data = stats_row,
    player_id = player_id,
    actual_id = actual_id
  )
}

# Compose a player payload shared across endpoints.
build_player_payload <- function(record, baseball_data, include_photo = TRUE) {
  lookup <- record$lookup
  info <- record$info
  stats <- record$data

  id <- if ("compound_id" %in% colnames(lookup)) {
    lookup$compound_id[[1]]
  } else {
    lookup$PlayerId[[1]]
  }

  sample_label <- if (identical(info$type, "pitcher")) "TBF" else "PA"
  sample_value <- if (identical(info$type, "pitcher")) {
    extract_row_value(stats, "tbf")
  } else {
    extract_row_value(stats, "PA_cur")
  }

  mlbam_source <- normalize_numeric(lookup$mlbamid)
  if (is.na(mlbam_source) && "mlbamid" %in% colnames(stats)) {
    mlbam_source <- normalize_numeric(extract_row_value(stats, "mlbamid"))
  }

  photo <- if (isTRUE(include_photo)) {
    get_player_photo_url(record$player_id, baseball_data)
  } else {
    NULL
  }

  display_name <- normalize_character(lookup$display_name)
  if (is.na(display_name)) {
    display_name <- normalize_character(lookup$Name)
  }

  list(
    id = id,
    playerId = record$actual_id,
    name = normalize_character(lookup$Name),
    displayName = display_name,
    type = info$type,
    age = normalize_numeric(info$age),
    mlbamId = mlbam_source,
    position = normalize_character(lookup$position) %||% normalize_character(extract_row_value(stats, "position")),
    positionDetail = normalize_character(lookup$position_detail),
    sample = list(
      label = sample_label,
      value = normalize_numeric(sample_value)
    ),
    photoUrl = if (isTRUE(include_photo)) photo %||% NA_character_ else NA_character_
  )
}

# Build the stat metrics for a hitter.
build_hitter_metrics <- function(stats_row) {
  list(
    create_metric("AVG", extract_row_value(stats_row, "AVG_cur"),
                  extract_row_value(stats_row, "AVG_l3"),
                  extract_row_value(stats_row, "AVG_diff")),
    create_metric("OBP", extract_row_value(stats_row, "OBP_cur"),
                  extract_row_value(stats_row, "OBP_l3"),
                  extract_row_value(stats_row, "OBP_diff")),
    create_metric("SLG", extract_row_value(stats_row, "SLG_cur"),
                  extract_row_value(stats_row, "SLG_l3"),
                  extract_row_value(stats_row, "SLG_diff")),
    create_metric("K%", extract_row_value(stats_row, "K_pct_cur"),
                  extract_row_value(stats_row, "K_pct_l3"),
                  extract_row_value(stats_row, "K_pct_diff"),
                  formatter = format_percentage, scale = "percent"),
    create_metric("BB%", extract_row_value(stats_row, "BB_pct_cur"),
                  extract_row_value(stats_row, "BB_pct_l3"),
                  extract_row_value(stats_row, "BB_pct_diff"),
                  formatter = format_percentage, scale = "percent"),
    create_metric("Barrel%", extract_row_value(stats_row, "Barrel_pct_cur"),
                  extract_row_value(stats_row, "Barrel_pct_l3"),
                  extract_row_value(stats_row, "Barrel_pct_diff"),
                  formatter = format_percentage, scale = "percent"),
    create_metric("BABIP", extract_row_value(stats_row, "BABIP_cur"),
                  extract_row_value(stats_row, "BABIP_l3"),
                  extract_row_value(stats_row, "BABIP_diff")),
    create_metric("wOBA", extract_row_value(stats_row, "wOBA_cur"),
                  extract_row_value(stats_row, "wOBA_l3"),
                  extract_row_value(stats_row, "wOBA_diff")),
    create_metric("xwOBA", extract_row_value(stats_row, "xwOBA_cur"),
                  extract_row_value(stats_row, "xwOBA_l3"),
                  extract_row_value(stats_row, "xwOBA_diff")),
    create_metric("xwOBA - wOBA", extract_row_value(stats_row, "xwOBA_wOBA_gap_cur"),
                  extract_row_value(stats_row, "xwOBA_wOBA_gap_l3"),
                  extract_row_value(stats_row, "xwOBA_wOBA_gap_diff"))
  )
}

# Build the stat metrics for a pitcher.
build_pitcher_metrics <- function(stats_row) {
  list(
    create_metric("ERA", extract_row_value(stats_row, "era_cur"),
                  extract_row_value(stats_row, "era_l3"),
                  extract_row_value(stats_row, "era_diff"),
                  formatter = format_era, scale = "era"),
    create_metric("xERA", extract_row_value(stats_row, "xera_cur"),
                  extract_row_value(stats_row, "xera_l3"),
                  extract_row_value(stats_row, "xera_diff"),
                  formatter = format_era, scale = "era"),
    create_metric("K%", extract_row_value(stats_row, "k_percent_cur"),
                  extract_row_value(stats_row, "k_percent_l3"),
                  extract_row_value(stats_row, "k_percent_diff"),
                  formatter = format_percentage, scale = "percent"),
    create_metric("BB%", extract_row_value(stats_row, "bb_percent_cur"),
                  extract_row_value(stats_row, "bb_percent_l3"),
                  extract_row_value(stats_row, "bb_percent_diff"),
                  formatter = format_percentage, scale = "percent"),
    create_metric("K-BB%", extract_row_value(stats_row, "k_minus_bb_percent_cur"),
                  extract_row_value(stats_row, "k_minus_bb_percent_l3"),
                  extract_row_value(stats_row, "k_minus_bb_percent_diff"),
                  formatter = format_percentage, scale = "percent"),
    create_metric("CSW%", extract_row_value(stats_row, "csw_percent_cur"),
                  extract_row_value(stats_row, "csw_percent_l3"),
                  extract_row_value(stats_row, "csw_percent_diff"),
                  formatter = format_percentage, scale = "percent"),
    create_metric("O-Swing%", extract_row_value(stats_row, "o_swing_percent_cur"),
                  extract_row_value(stats_row, "o_swing_percent_l3"),
                  extract_row_value(stats_row, "o_swing_percent_diff"),
                  formatter = format_percentage, scale = "percent"),
    create_metric("Barrel%", extract_row_value(stats_row, "barrel_percent_cur"),
                  extract_row_value(stats_row, "barrel_percent_l3"),
                  extract_row_value(stats_row, "barrel_percent_diff"),
                  formatter = format_percentage, scale = "percent"),
    create_metric("BABIP", extract_row_value(stats_row, "babip_cur"),
                  extract_row_value(stats_row, "babip_l3"),
                  extract_row_value(stats_row, "babip_diff")),
    create_metric("LOB%", extract_row_value(stats_row, "lob_percent_cur"),
                  extract_row_value(stats_row, "lob_percent_l3"),
                  extract_row_value(stats_row, "lob_percent_diff"),
                  formatter = format_percentage, scale = "percent")
  )
}

# Build the statline payload for a single player.
build_statline_payload <- function(player_id, baseball_data, include_photo = TRUE) {
  record <- resolve_player_record(player_id, baseball_data)
  if (is.null(record)) {
    return(NULL)
  }

  metrics <- if (identical(record$info$type, "hitter")) {
    build_hitter_metrics(record$data)
  } else {
    build_pitcher_metrics(record$data)
  }

  list(
    player = build_player_payload(record, baseball_data, include_photo = include_photo),
    metrics = metrics
  )
}

# Build an index of players for list endpoints.
build_players_index <- function(baseball_data, type = NULL, search = NULL, include_photo = FALSE) {
  players <- baseball_data$lookup

  if (!is.null(type) && type != "") {
    players <- players %>% dplyr::filter(player_type == type)
  }

  if (!is.null(search) && stringr::str_trim(search) != "") {
    pattern <- stringr::regex(search, ignore_case = TRUE)
    players <- players %>%
      dplyr::mutate(
        search_name = dplyr::coalesce(display_name, Name)
      ) %>%
      dplyr::filter(
        stringr::str_detect(search_name, pattern) |
          stringr::str_detect(Name, pattern)
      )
  }

  players <- players %>%
    dplyr::mutate(sort_name = dplyr::coalesce(display_name, Name)) %>%
    dplyr::arrange(sort_name)

  if (nrow(players) == 0) {
    return(list())
  }

  hitter_pa <- baseball_data$hitters$PA_cur
  names(hitter_pa) <- baseball_data$hitters$PlayerId

  pitcher_tbf <- baseball_data$pitchers$tbf
  names(pitcher_tbf) <- baseball_data$pitchers$PlayerId

  lapply(seq_len(nrow(players)), function(i) {
    row <- players[i, , drop = FALSE]
    id <- if ("compound_id" %in% colnames(row)) row$compound_id[[1]] else row$PlayerId[[1]]
    type_value <- row$player_type[[1]]
    sample_label <- if (identical(type_value, "pitcher")) "TBF" else "PA"
    sample_value <- if (identical(type_value, "pitcher")) {
      pitcher_tbf[[as.character(row$PlayerId[[1]])]]
    } else {
      hitter_pa[[as.character(row$PlayerId[[1]])]]
    }

    record <- list(
      lookup = row,
      info = list(
        name = row$Name[[1]],
        type = type_value,
        age = row$Age[[1]]
      ),
      data = tibble::tibble(),
      player_id = id,
      actual_id = row$PlayerId[[1]]
    )

    payload <- build_player_payload(record, baseball_data, include_photo = include_photo)
    payload$sample <- list(label = sample_label, value = normalize_numeric(sample_value))
    payload
  })
}

# Lightweight helper to build a recommendation payload from a PlayerId.
build_recommended_player <- function(player_id, baseball_data) {
  if (is.null(player_id) || player_id == "") {
    return(NULL)
  }

  lookup <- baseball_data$lookup %>% dplyr::filter(PlayerId == player_id)
  if (nrow(lookup) == 0) {
    return(NULL)
  }

  id <- if ("compound_id" %in% colnames(lookup)) lookup$compound_id[[1]] else lookup$PlayerId[[1]]

  display_name <- lookup$display_name[[1]]
  if (is.na(display_name) || display_name == "") {
    display_name <- lookup$Name[[1]]
  }

  list(
    id = id,
    playerId = player_id,
    name = lookup$Name[[1]],
    displayName = display_name,
    type = lookup$player_type[[1]]
  )
}

