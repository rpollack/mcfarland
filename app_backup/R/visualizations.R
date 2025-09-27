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

