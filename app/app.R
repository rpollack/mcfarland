# ==============================================================================
# McFARLAND: Machine-crafted Forecasting And Reasoning for 
#            Luck, Analytics, Narratives, and Data
# 
# A professional baseball analytics application with AI-powered analysis
# ==============================================================================

# Load Required Libraries --------------------------------------------------
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(httr)
library(jsonlite)
library(shinyWidgets)
library(bslib)
library(commonmark)
library(shinybusy)
library(ggplot2)

# Configuration -------------------------------------------------------------
GITHUB_DATA_URL <- "https://raw.githubusercontent.com/rpollack/leadRboard/master/"
MLB_PHOTO_BASE_URL <- "https://img.mlbstatic.com/mlb-photos/image/upload/w_213,d_people:generic:headshot:silo:current.png,q_auto:best,f_auto/v1/people/"
FANGRAPHS_PHOTO_BASE_URL <- "https://www.fangraphs.com/img/players/"
CURRENT_YEAR <- 2025

# Data Loading Functions ----------------------------------------------------

#' Load baseball data from GitHub repository or local files
#' @return List containing hitters, pitchers, and lookup data frames
load_baseball_data <- function() {
  cat("Loading baseball data...\n")
  
  # Try local files first (for shinyapps.io deployment)
  local_files <- c("full_stats_hitters.csv", "full_stats_pitchers.csv", "player_lookup.csv")
  
  if (all(file.exists(local_files))) {
    cat("Loading from local files...\n")
    tryCatch({
      data_list <- map(local_files, ~ read_csv(.x, show_col_types = FALSE))
      names(data_list) <- c("hitters", "pitchers", "lookup")
      
      cat("Successfully loaded from local:", nrow(data_list$hitters), "hitters,", nrow(data_list$pitchers), "pitchers\n")
      return(data_list)
      
    }, error = function(e) {
      cat("Error loading local files:", e$message, "\n")
    })
  }
  
  # Fallback to GitHub (for local development)
  cat("Loading from GitHub...\n")
  tryCatch({
    data_files <- c("full_stats_hitters.csv", "full_stats_pitchers.csv", "player_lookup.csv")
    data_list <- map(data_files, ~ {
      url <- paste0(GITHUB_DATA_URL, .x)
      cat("Fetching:", url, "\n")
      read_csv(url, show_col_types = FALSE)
    })
    names(data_list) <- c("hitters", "pitchers", "lookup")
    
    cat("Successfully loaded from GitHub:", nrow(data_list$hitters), "hitters,", nrow(data_list$pitchers), "pitchers\n")
    data_list
    
  }, error = function(e) {
    cat("Error loading from GitHub:", e$message, "\n")
    # Return empty but valid data structure
    list(
      hitters = data.frame(),
      pitchers = data.frame(),
      lookup = data.frame(display_name = character(0), PlayerId = character(0), player_type = character(0))
    )
  })
}

# Photo Management Functions -----------------------------------------------

#' Get MLB headshot URL for a player
#' @param mlb_id MLB ID number
#' @return Complete MLB photo URL
build_mlb_photo_url <- function(mlb_id) {
  if (is.null(mlb_id) || is.na(mlb_id) || mlb_id == "" || mlb_id == 0) {
    return(NULL)
  }
  paste0(MLB_PHOTO_BASE_URL, mlb_id, "/headshot/67/current")
}

#' Extract PlayerId from compound ID or return as-is
#' @param compound_id Either a compound ID like "12345_hitter" or a simple PlayerId
#' @return PlayerId as string
extract_player_id <- function(compound_id) {
  if (grepl("_", compound_id)) {
    # Extract PlayerId from compound format "PlayerId_playertype"
    strsplit(compound_id, "_")[[1]][1]
  } else {
    # Backwards compatibility - already a simple PlayerId
    compound_id
  }
}

#' Get player's MLB ID from the data
#' @param player_id FanGraphs player ID (can be compound or simple)
#' @param baseball_data Complete baseball data list
#' @return MLB ID if found, NULL otherwise
get_player_mlb_id <- function(player_id, baseball_data) {
  # Handle both compound IDs and backwards compatibility
  if ("compound_id" %in% colnames(baseball_data$lookup)) {
    # New format with compound IDs
    player_info <- filter(baseball_data$lookup, compound_id == player_id)
    actual_player_id <- extract_player_id(player_id)
  } else {
    # Old format - player_id is just a simple PlayerId
    player_info <- filter(baseball_data$lookup, PlayerId == player_id)
    actual_player_id <- player_id
  }
  
  if (nrow(player_info) == 0) return(NULL)
  
  player_type <- player_info$player_type
  dataset <- if (player_type == "hitter") baseball_data$hitters else baseball_data$pitchers
  
  if (nrow(dataset) == 0) return(NULL)
  
  player_data <- filter(dataset, PlayerId == actual_player_id)
  if (nrow(player_data) == 0 || !"mlbamid" %in% colnames(player_data)) return(NULL)
  
  mlb_id <- player_data$mlbamid[1]
  if (is.na(mlb_id) || mlb_id == "" || mlb_id == 0) return(NULL)
  
  mlb_id
}

#' Generate complete photo URL with fallback strategy
#' @param player_id FanGraphs player ID
#' @param baseball_data Complete baseball data list
#' @return Photo URL (MLB, FanGraphs, or placeholder)
get_player_photo_url <- function(player_id, baseball_data) {
  # Try MLB photo first
  mlb_id <- get_player_mlb_id(player_id, baseball_data)
  mlb_url <- build_mlb_photo_url(mlb_id)
  
  if (!is.null(mlb_url)) {
    return(mlb_url)
  }
  
  # Fallback to FanGraphs
  paste0(FANGRAPHS_PHOTO_BASE_URL, player_id, ".jpg")
}

# Player Information Functions ---------------------------------------------

#' Get player basic information
#' @param player_id FanGraphs player ID (can be compound or simple)
#' @param baseball_data Complete baseball data list
#' @return List with name, type, age, position info
get_player_info <- function(player_id, baseball_data) {
  # Handle both compound IDs and backwards compatibility
  if ("compound_id" %in% colnames(baseball_data$lookup)) {
    # New format with compound IDs
    player_lookup <- filter(baseball_data$lookup, compound_id == player_id)
  } else {
    # Old format - player_id is just a simple PlayerId
    player_lookup <- filter(baseball_data$lookup, PlayerId == player_id)
  }
  
  if (nrow(player_lookup) == 0) return(NULL)
  
  player_name <- player_lookup$Name
  player_type <- player_lookup$player_type
  
  # For old format, use the player_id directly; for new format, extract it
  actual_player_id <- if ("compound_id" %in% colnames(baseball_data$lookup)) {
    extract_player_id(player_id)
  } else {
    player_id
  }
  
  # Get detailed info based on type
  if (player_type == "hitter" && nrow(baseball_data$hitters) > 0) {
    player_data <- filter(baseball_data$hitters, PlayerId == actual_player_id)
    if (nrow(player_data) > 0) {
      return(list(
        name = player_name,
        type = player_type,
        age = player_data$Age[1],
        position_info = "Hitter"
      ))
    }
  } else if (player_type == "pitcher" && nrow(baseball_data$pitchers) > 0) {
    player_data <- filter(baseball_data$pitchers, PlayerId == actual_player_id)
    if (nrow(player_data) > 0) {
      position_detail <- if ("position" %in% colnames(player_data)) {
        paste(player_data$position[1], "• Pitcher")
      } else {
        "Pitcher"
      }
      return(list(
        name = player_name,
        type = player_type,
        age = player_data$Age[1],
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
  
  div(
    class = "text-center player-info-card",
    
    img(
      src = photo_url,
      alt = paste("Photo of", player_info$name),
      class = "player-photo",
      onerror = paste0("this.onerror=null; this.src='", fallback_url, "';")
    ),
    
    h4(player_info$name, class = "player-name"),
    
    p(
      class = "player-details",
      if (!is.na(player_info$age)) paste("Age:", player_info$age),
      if (player_info$position_info != "") paste("•", player_info$position_info)
    )
  )
}

# Visualization Functions --------------------------------------------------

#' Get metrics for comparison plots based on player type
#' @param player_type Either "hitter" or "pitcher"
#' @return Vector of metric names
get_comparison_metrics <- function(player_type) {
  if (player_type == "hitter") {
    c("AVG", "OBP", "SLG", "K_pct", "BB_pct", "Barrel_pct", "BABIP", "wOBA", "xwOBA")
  } else {
    c("era", "xera", "k_percent", "bb_percent", "k_minus_bb_percent", "barrel_percent", "babip", "lob_percent")
  }
}

#' Clean metric names for display
#' @param metrics Vector of metric names
#' @param player_type Either "hitter" or "pitcher"
#' @return Vector of cleaned metric names
clean_metric_names <- function(metrics, player_type) {
  if (player_type == "hitter") {
    case_when(
      metrics == "K_pct" ~ "K%",
      metrics == "BB_pct" ~ "BB%",
      metrics == "Barrel_pct" ~ "Barrel%",
      TRUE ~ metrics
    )
  } else {
    case_when(
      metrics == "era" ~ "ERA",
      metrics == "xera" ~ "xERA",
      metrics == "k_percent" ~ "K%",
      metrics == "bb_percent" ~ "BB%",
      metrics == "k_minus_bb_percent" ~ "K-BB%",
      metrics == "barrel_percent" ~ "Barrel%",
      metrics == "babip" ~ "BABIP",
      metrics == "lob_percent" ~ "LOB%",
      TRUE ~ metrics
    )
  }
}

#' Create comparison plot for player trends
#' @param player_id FanGraphs player ID (can be compound or simple)
#' @param baseball_data Complete baseball data list
#' @return ggplot object or NULL
create_player_trends_plot <- function(player_id, baseball_data) {
  player_info <- get_player_info(player_id, baseball_data)
  if (is.null(player_info)) return(NULL)
  
  # Get appropriate dataset
  dataset <- if (player_info$type == "hitter") baseball_data$hitters else baseball_data$pitchers
  if (nrow(dataset) == 0) return(NULL)
  
  # For old format, use the player_id directly; for new format, extract it
  actual_player_id <- if ("compound_id" %in% colnames(baseball_data$lookup)) {
    extract_player_id(player_id)
  } else {
    player_id
  }
  
  player_data <- filter(dataset, PlayerId == actual_player_id)
  if (nrow(player_data) == 0) return(NULL)
  
  # Build comparison data
  metrics <- get_comparison_metrics(player_info$type)
  comparison_data <- map_dfr(metrics, ~ {
    current_col <- paste0(.x, "_cur")
    avg_col <- paste0(.x, "_l3")
    
    if (current_col %in% colnames(player_data) && avg_col %in% colnames(player_data)) {
      current_val <- player_data[[current_col]]
      avg_val <- player_data[[avg_col]]
      
      if (!is.na(current_val) && !is.na(avg_val)) {
        tibble(
          player = rep(player_info$name, 2),
          metric = rep(.x, 2),
          period = c("Past 3 years", "2025"),
          value = c(avg_val, current_val)
        )
      }
    }
  })
  
  if (nrow(comparison_data) == 0) return(NULL)
  
  # Clean metric names
  comparison_data$metric_display <- clean_metric_names(comparison_data$metric, player_info$type)
  
  # Create plot
  ggplot(comparison_data, aes(
    x = factor(period, levels = c("Past 3 years", "2025")),
    y = value, 
    group = metric_display
  )) +
    geom_point(size = 3, color = "#2E86AB") +
    geom_line(color = "#2E86AB") +
    facet_wrap(~metric_display, scales = "free_y") +
    labs(
      title = paste("Trends:", player_info$name),
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

#' Safe value formatting for display
#' @param x Numeric value to format
#' @return Formatted string
format_stat_value <- function(x) {
  if (is.na(x) || is.null(x)) {
    return("N/A")
  }
  if (is.numeric(x)) {
    return(round(x, 3))
  }
  as.character(x)
}

#' Get analysis persona prompt
#' @param mode Analysis mode string
#' @return Persona prompt text
get_analysis_persona <- function(mode) {
  personas <- list(
    "analytics_dork" = "You are a front office nerd, raised on moneyball and new school stats, always at the cutting edge. You favor new school stats, talk in probabilities, and are very dismissive of people who don't believe you. You might be the smartest person in the room, but people would describe you as a real tool. Be ruthless and dismissive!",
    "old_coot" = "You are a deranged old coot, ranting and raving about everything. Yell a lot. People would describe you as 'off your meds'. Throw in references to people spying on you. Appear confused at times. Get stats wrong occasionally. You know, just -- be insane.",
    "gen_z" = "You're an over the top Gen Z'er, using lots of slang, referencing hyper modern trends, apps, emojis, and such. But really lay it on thick, in a humorously over-the-top kind of way.",
    "seventies" = "You prefer 1970s style of baseball, when men were men, stolen bases were high, starting pitchers completed every game, and guys had bushy mustaches and chewed tobacco all game. You strongly prefer old school stats to new school ones. Use lots of comparisons to famous 1970s baseball players: Pete Rose, Johnny Bench, Mike Schmidt, Willie Stargell, Rod Carew, Bobby Grich, Thurman Munson, etc -- but don't limit your comparisons to just these guys.",
    "sensationalist" = "You report baseball analysis like a carnival barker in the jazz age: always trying to make things larger than life through flowery prose and colorful headlines. You practice sensationalist, ballyhoo sportswriting and yellow-journalism-style copy. Every flaw is a titanic tragedy, and every positive is a starry-eyed bright and shiny future.",
    "shakespeare" = "You are William Shakespeare. Not just that, but you speak in verse -- preferably iambic pentameter."
  )
  
  personas[[mode]] %||% "Keep it simple and easy to understand. Use short but friendly sentences. Don't start with asides or extraneous clauses. Start your response with the conclusion/summary takeaways, then underneath, list your evidence for that summary and those conclusions."
}

#' Call OpenAI API for analysis
#' @param prompt_text Analysis prompt
#' @param analysis_mode Analysis style mode
#' @return HTML formatted response
call_openai_api <- function(prompt_text, analysis_mode) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  
  if (api_key == "") {
    return(HTML(paste0(
      "<div class='alert alert-info'>",
      "<h5>OpenAI API Key Not Set</h5>",
      "<p>Set OPENAI_API_KEY environment variable to enable analysis.</p>",
      "<details><summary>View prompt</summary><pre>", 
      htmltools::htmlEscape(prompt_text), 
      "</pre></details></div>"
    )))
  }
  
  persona_prompt <- get_analysis_persona(analysis_mode)
  
  full_prompt <- paste0(
    "Here is current-year performance data for a player:\n\n",
    prompt_text,
    "\n\nGeneral instructions:\n\n",
    "Please analyze how the player is performing this year, what trends stand out, and whether any aspects of the performance appear to be skill- or luck-driven. Incorporate a prediction: will the player improve, decline, or stay the same for the rest of the season? Explain your reasoning.\n\n",
    "The very first element of the response should be a title that encompasses your findings.\n\n",
    "Your analysis must incorporate metric, direction, and magnitude of difference. For example BB% is up, indicate by how much, and what the size of that gap might indicate. You don't need to explicitly call out this framing (e.g. in bullets), just make sure to weave it into your analysis.\n\n",
    "Separate your analysis into core skills and luck/regression indicators.\n\n",
    "Don't repeat yourself. For example, if you say a stat or performance or trend is 'lucky', you don't need to say it's 'not unlucky'.\n\n",
    "Remember that when it comes to stats and trends, you only have knowledge of two things: a player's current-year stats and the average of the same stats for the past 3 years (e.g. not their entire career). So when you say things like a stat is 'up' or 'down', make it clear that this is relative to the last 3 years' average.\n\n",
    "Here is your persona that should inform your writing style and response, even if it means overriding those previous instructions: ",
    persona_prompt
  )
  
  tryCatch({
    response <- POST(
      "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", api_key)),
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
      return(HTML(paste0("<div class='alert alert-danger'>API Error: ", parsed_response$error$message, "</div>")))
    }
    
    if (is.null(parsed_response$choices) || length(parsed_response$choices) == 0) {
      return(HTML("<div class='alert alert-warning'>No response from API.</div>"))
    }
    
    analysis_text <- parsed_response$choices[[1]]$message$content
    HTML(commonmark::markdown_html(analysis_text))
    
  }, error = function(e) {
    HTML(paste0("<div class='alert alert-danger'>Error: ", e$message, "</div>"))
  })
}

#' Build hitter analysis prompt
#' @param player_name Player name
#' @param hitter_data Hitter statistics data frame
#' @return Analysis prompt text
build_hitter_prompt <- function(player_name, hitter_data) {
  data <- filter(hitter_data, Name == player_name)
  if (nrow(data) == 0) return(NULL)
  
  prompt_sections <- c(
    paste0("Player: ", player_name, " (Hitter)"),
    "",
    "--- Key metrics to analyze---",
    paste0("Age: ", format_stat_value(data$Age)),
    paste0("Year: ", CURRENT_YEAR),
    paste0("Plate Appearances (PA): ", format_stat_value(data$PA_cur)),
    paste0("AVG: ", format_stat_value(data$AVG_cur), "  Last 3 Years: ", format_stat_value(data$AVG_l3), "  Diff: ", format_stat_value(data$AVG_diff)),
    paste0("OBP: ", format_stat_value(data$OBP_cur), "  Last 3 Years: ", format_stat_value(data$OBP_l3), "  Diff: ", format_stat_value(data$OBP_diff)),
    paste0("SLG: ", format_stat_value(data$SLG_cur), "  Last 3 Years: ", format_stat_value(data$SLG_l3), "  Diff: ", format_stat_value(data$SLG_diff)),
    paste0("K%: ", format_stat_value(data$K_pct_cur), "  Last 3 Years: ", format_stat_value(data$K_pct_l3), "  Diff: ", format_stat_value(data$K_pct_diff)),
    paste0("BB%: ", format_stat_value(data$BB_pct_cur), "  Last 3 Years: ", format_stat_value(data$BB_pct_l3), "  Diff: ", format_stat_value(data$BB_pct_diff)),
    paste0("Barrel%: ", format_stat_value(data$Barrel_pct_cur), "  Last 3 Years: ", format_stat_value(data$Barrel_pct_l3), "  Diff: ", format_stat_value(data$Barrel_pct_diff)),
    paste0("BABIP: ", format_stat_value(data$BABIP_cur), "  Last 3 Years: ", format_stat_value(data$BABIP_l3), "  Diff: ", format_stat_value(data$BABIP_diff)),
    paste0("wOBA: ", format_stat_value(data$wOBA_cur), "  Last 3 Years: ", format_stat_value(data$wOBA_l3), "  Diff: ", format_stat_value(data$wOBA_diff)),
    paste0("xwOBA: ", format_stat_value(data$xwOBA_cur), "  Last 3 Years: ", format_stat_value(data$xwOBA_l3), "  Diff: ", format_stat_value(data$xwOBA_diff)),
    paste0("xwOBA-wOBA gap: ", format_stat_value(data$xwOBA_wOBA_gap_cur), "  Last 3 Years: ", format_stat_value(data$xwOBA_wOBA_gap_l3), "  Diff: ", format_stat_value(data$xwOBA_wOBA_gap_diff)),
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
  
  paste(prompt_sections, collapse = "\n")
}

#' Build pitcher analysis prompt
#' @param player_name Player name
#' @param pitcher_data Pitcher statistics data frame
#' @return Analysis prompt text
build_pitcher_prompt <- function(player_name, pitcher_data) {
  data <- filter(pitcher_data, Name == player_name)
  if (nrow(data) == 0) return(NULL)
  
  prompt_sections <- c(
    paste0("Player: ", player_name, " (Pitcher)"),
    "",
    "--- Key metrics to analyze---",
    paste0("Age: ", format_stat_value(data$Age)),
    paste0("Year: ", CURRENT_YEAR),
    paste0("Position: ", ifelse("position" %in% names(data), format_stat_value(data$position), "Pitcher")),
    paste0("Total Batters Faced: ", format_stat_value(data$tbf)),
    "",
    paste0("ERA: ", format_stat_value(data$era_cur), "  Last 3 Years: ", format_stat_value(data$era_l3), "  Diff: ", format_stat_value(data$era_diff)),
    paste0("xERA: ", format_stat_value(data$xera_cur), "  Last 3 Years: ", format_stat_value(data$xera_l3), "  Diff: ", format_stat_value(data$xera_diff)),
    paste0("BABIP: ", format_stat_value(data$babip_cur), "  Last 3 Years: ", format_stat_value(data$babip_l3), "  Diff: ", format_stat_value(data$babip_diff)),
    paste0("Barrel Rate: ", format_stat_value(data$barrel_percent_cur), "%  Last 3 Years: ", format_stat_value(data$barrel_percent_l3), "%  Diff: ", format_stat_value(data$barrel_percent_diff), "%"),
    paste0("Strikeout Rate (K%): ", format_stat_value(data$k_percent_cur), "%  Last 3 Years: ", format_stat_value(data$k_percent_l3), "%  Diff: ", format_stat_value(data$k_percent_diff), "%"),
    paste0("Called Strike & Whiff Rate (CSW%): ", format_stat_value(data$csw_percent_cur), "%  Last 3 Years: ", format_stat_value(data$csw_percent_l3), "%  Diff: ", format_stat_value(data$csw_percent_diff), "%"),
    paste0("Outside Zone Swing Rate (O-Swing%): ", format_stat_value(data$o_swing_percent_cur), "%  Last 3 Years: ", format_stat_value(data$o_swing_percent_l3), "%  Diff: ", format_stat_value(data$o_swing_percent_diff), "%"),
    paste0("Walk Rate (BB%): ", format_stat_value(data$bb_percent_cur), "%  Last 3 Years: ", format_stat_value(data$bb_percent_l3), "%  Diff: ", format_stat_value(data$bb_percent_diff), "%"),
    paste0("K-BB%: ", format_stat_value(data$k_minus_bb_percent_cur), "%  Last 3 Years: ", format_stat_value(data$k_minus_bb_percent_l3), "%  Diff: ", format_stat_value(data$k_minus_bb_percent_diff), "%"),
    paste0("LOB% (Left-on-base rate): ", format_stat_value(data$lob_percent_cur), "%  Last 3 Years: ", format_stat_value(data$lob_percent_l3), "%  Diff: ", format_stat_value(data$lob_percent_diff), "%"),
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
  
  paste(prompt_sections, collapse = "\n")
}

#' Analyze hitter performance
#' @param player_name Player name
#' @param analysis_mode Analysis style mode
#' @param hitter_data Hitter statistics data frame
#' @return HTML analysis result
analyze_hitter_performance <- function(player_name, analysis_mode, hitter_data) {
  if (nrow(filter(hitter_data, Name == player_name)) == 0) {
    return(HTML(paste0("<div class='alert alert-warning'>Hitter not found: ", player_name, "</div>")))
  }
  
  prompt <- build_hitter_prompt(player_name, hitter_data)
  if (is.null(prompt)) {
    return(HTML(paste0("<div class='alert alert-warning'>Unable to build analysis for: ", player_name, "</div>")))
  }
  
  call_openai_api(prompt, analysis_mode)
}

#' Analyze pitcher performance
#' @param player_name Player name
#' @param analysis_mode Analysis style mode
#' @param pitcher_data Pitcher statistics data frame
#' @return HTML analysis result
analyze_pitcher_performance <- function(player_name, analysis_mode, pitcher_data) {
  if (nrow(filter(pitcher_data, Name == player_name)) == 0) {
    return(HTML(paste0("<div class='alert alert-warning'>Pitcher not found: ", player_name, "</div>")))
  }
  
  prompt <- build_pitcher_prompt(player_name, pitcher_data)
  if (is.null(prompt)) {
    return(HTML(paste0("<div class='alert alert-warning'>Unable to build analysis for: ", player_name, "</div>")))
  }
  
  call_openai_api(prompt, analysis_mode)
}

#' Main player analysis router
#' @param player_id FanGraphs player ID
#' @param analysis_mode Analysis style mode
#' @param baseball_data Complete baseball data list
#' @return HTML analysis result
analyze_player_performance <- function(player_id, analysis_mode, baseball_data) {
  player_info <- get_player_info(player_id, baseball_data)
  if (is.null(player_info)) {
    return(HTML("<div class='alert alert-warning'>Player not found.</div>"))
  }
  
  if (player_info$type == "hitter") {
    analyze_hitter_performance(player_info$name, analysis_mode, baseball_data$hitters)
  } else if (player_info$type == "pitcher") {
    analyze_pitcher_performance(player_info$name, analysis_mode, baseball_data$pitchers)
  } else {
    HTML("<div class='alert alert-warning'>Unknown player type.</div>")
  }
}

# Statistics Functions ------------------------------------------------------

#' Calculate MLB photo coverage statistics
#' @param baseball_data Complete baseball data list
#' @return List with coverage statistics
calculate_photo_coverage <- function(baseball_data) {
  hitters_total <- nrow(baseball_data$hitters)
  pitchers_total <- nrow(baseball_data$pitchers)
  
  hitters_with_mlb <- 0
  pitchers_with_mlb <- 0
  
  if (hitters_total > 0 && "mlbamid" %in% colnames(baseball_data$hitters)) {
    hitters_with_mlb <- sum(!is.na(baseball_data$hitters$mlbamid) & 
                              baseball_data$hitters$mlbamid != "" & 
                              baseball_data$hitters$mlbamid != 0, na.rm = TRUE)
  }
  
  if (pitchers_total > 0 && "mlbamid" %in% colnames(baseball_data$pitchers)) {
    pitchers_with_mlb <- sum(!is.na(baseball_data$pitchers$mlbamid) & 
                               baseball_data$pitchers$mlbamid != "" & 
                               baseball_data$pitchers$mlbamid != 0, na.rm = TRUE)
  }
  
  list(
    hitters_total = hitters_total,
    pitchers_total = pitchers_total,
    hitters_with_mlb = hitters_with_mlb,
    pitchers_with_mlb = pitchers_with_mlb
  )
}

# UI Styling ----------------------------------------------------------------

ui_styles <- HTML("
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
  
  /* Global app styling */
  * {
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  }
  
  body {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    min-height: 100vh;
    margin: 0;
    overflow-x: hidden;
  }
  
  /* Navbar styling */
  .navbar {
    background: rgba(255, 255, 255, 0.95) !important;
    backdrop-filter: blur(20px);
    border-bottom: 1px solid rgba(255, 255, 255, 0.2);
    box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1);
  }
  
  .navbar-brand {
    font-weight: 700 !important;
    font-size: 1.5rem !important;
    color: #2E86AB !important;
    text-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
  }
  
  .nav-link {
    font-weight: 500 !important;
    transition: all 0.3s ease !important;
    border-radius: 8px !important;
    margin: 0 4px !important;
  }
  
  .nav-link:hover {
    background: rgba(46, 134, 171, 0.1) !important;
    transform: translateY(-1px);
  }
  
  /* Card styling with glass morphism */
  .card {
    background: rgba(255, 255, 255, 0.95) !important;
    backdrop-filter: blur(20px) !important;
    border: 1px solid rgba(255, 255, 255, 0.2) !important;
    border-radius: 20px !important;
    box-shadow: 0 20px 40px rgba(0, 0, 0, 0.1) !important;
    transition: all 0.3s ease !important;
    overflow: hidden;
  }
  
  .card:hover {
    transform: translateY(-5px);
    box-shadow: 0 25px 50px rgba(0, 0, 0, 0.15) !important;
  }
  
  .card-header {
    background: linear-gradient(135deg, #2E86AB, #4A90E2) !important;
    color: white !important;
    font-weight: 600 !important;
    font-size: 1.1rem !important;
    border-bottom: none !important;
    padding: 1.25rem 1.5rem !important;
    text-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
  }
  
  .card-body {
    padding: 1.5rem !important;
  }
  
  /* Form controls styling */
  .form-select, .form-control {
    border: 2px solid rgba(46, 134, 171, 0.2) !important;
    border-radius: 12px !important;
    padding: 0.75rem 1rem !important;
    font-weight: 500 !important;
    transition: all 0.3s ease !important;
    background: rgba(255, 255, 255, 0.9) !important;
  }
  
  .form-select:focus, .form-control:focus {
    border-color: #2E86AB !important;
    box-shadow: 0 0 0 3px rgba(46, 134, 171, 0.2) !important;
    transform: scale(1.02);
  }
  
  /* Button styling */
  .btn {
    border-radius: 12px !important;
    font-weight: 600 !important;
    padding: 0.75rem 1.5rem !important;
    transition: all 0.3s ease !important;
    text-transform: uppercase;
    letter-spacing: 0.5px;
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
  
  /* Player info card styling */
  .player-info-card {
    background: linear-gradient(135deg, rgba(255, 255, 255, 0.95), rgba(248, 249, 250, 0.95)) !important;
    backdrop-filter: blur(20px) !important;
    border: 1px solid rgba(255, 255, 255, 0.3) !important;
    border-radius: 20px !important;
    padding: 2rem !important;
    box-shadow: 0 15px 35px rgba(0, 0, 0, 0.1) !important;
    transition: all 0.4s ease !important;
    margin-bottom: 1.5rem !important;
  }
  
  .player-info-card:hover {
    transform: translateY(-3px) scale(1.02);
    box-shadow: 0 25px 50px rgba(0, 0, 0, 0.15) !important;
  }
  
  .player-photo {
    width: 160px !important;
    height: 160px !important;
    border-radius: 50% !important;
    border: 4px solid #2E86AB !important;
    margin-bottom: 1rem !important;
    object-fit: cover !important;
    transition: all 0.3s ease !important;
    box-shadow: 0 10px 30px rgba(46, 134, 171, 0.3) !important;
  }
  
  .player-photo:hover {
    transform: scale(1.05);
    box-shadow: 0 15px 40px rgba(46, 134, 171, 0.4) !important;
  }
  
  .player-name {
    margin-bottom: 0.5rem !important;
    color: #2E86AB !important;
    font-weight: 700 !important;
    font-size: 1.5rem !important;
    text-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
  }
  
  .player-details {
    color: #6c757d !important;
    font-size: 0.95rem !important;
    font-weight: 500 !important;
    margin-bottom: 0 !important;
  }
  
  /* Alert styling */
  .alert {
    border-radius: 15px !important;
    border: none !important;
    font-weight: 500 !important;
    backdrop-filter: blur(10px) !important;
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
    height: 8px !important;
    border-radius: 10px !important;
    background: rgba(255, 255, 255, 0.2) !important;
  }
  
  .progress-bar {
    background: linear-gradient(90deg, #2E86AB, #4A90E2) !important;
    border-radius: 10px !important;
  }
  
  /* Plot container styling */
  .shiny-plot-output {
    border-radius: 15px !important;
    overflow: hidden !important;
    box-shadow: 0 10px 30px rgba(0, 0, 0, 0.1) !important;
  }
  
  /* Responsive improvements */
  @media (max-width: 768px) {
    .card {
      margin-bottom: 1rem !important;
      border-radius: 15px !important;
    }
    
    .player-info-card {
      padding: 1.5rem !important;
    }
    
    .player-photo {
      width: 120px !important;
      height: 120px !important;
    }
    
    .player-name {
      font-size: 1.3rem !important;
    }
  }
  
  /* Smooth scrolling */
  html {
    scroll-behavior: smooth;
  }
  
  /* Custom scrollbar */
  ::-webkit-scrollbar {
    width: 8px;
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
  
  /* Analysis content styling */
  .analysis-content {
    line-height: 1.6 !important;
    font-size: 1rem !important;
  }
  
  .analysis-content h1, .analysis-content h2, .analysis-content h3 {
    color: #2E86AB !important;
    font-weight: 700 !important;
    margin-top: 1.5rem !important;
    margin-bottom: 1rem !important;
  }
  
  .analysis-content p {
    margin-bottom: 1rem !important;
    color: #495057 !important;
  }
  
  /* Data status styling */
  .data-status p {
    margin-bottom: 0.75rem !important;
    font-size: 0.95rem !important;
  }
  
  .data-status strong {
    color: #2E86AB !important;
  }
")

# UI Definition -------------------------------------------------------------

ui <- page_navbar(
  title = HTML("McFARLAND <small style='font-size: 0.6em; color: #6c757d; font-weight: 400;'>🤖⚾ AI-powered baseball player analysis</small>"),
  
  header = tagList(
    # Viewport constraints for mobile - prevents zooming and wiggling
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0, user-scalable=no, viewport-fit=cover"),
    
    # Custom CSS for professional, app-like styling
    tags$style(ui_styles),
    
    add_busy_bar(
      color = "#2E86AB",
      height = "25px"
    )
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
          
          # Player info card with headshot
          uiOutput("player_info_card")
        )
      ),
      
      card(
        card_header("Analysis"),
        uiOutput("result_output")
      )
    )
  ),
  
  nav_panel(
    title = "About",
    icon = icon("info-circle"),
    card(
      card_body(
        p("McFARLAND: Machine-crafted Forecasting And Reasoning for Luck, Analytics, Narratives, and Data"),
        img(
          src = "tjmcfarland.png",
          style = "width: 100%; max-width: 400px; height: auto;"
        ),
        p("Now supports both hitters and pitchers with MLB player headshots!"),
        p("Data from FanGraphs. Comparing 2025 stats (refreshed daily) to 2022-2024 averages."),
        p("Player photos courtesy of MLB."),
        p("Built with R, shiny, tidyverse, baseballr, bslib, shinyWidgets, and shinybusy."),
        p("Powered by GPT-4.1."),
        
        # Data status section
        h4("Data Status"),
        uiOutput("data_status"),
        
        h4("Get In Touch"),
        tags$a(
          href    = "https://docs.google.com/forms/d/e/1FAIpQLScPiHfO2XxwCXd2V-7pNsUKs-mMaqzzsH2ohA_kBflk_n8AQw/viewform",
          target  = "_blank",
          class   = "btn btn-sm btn-primary",
          style   = "margin-left: 1rem;",
          icon("envelope"),
          "Get updated when we add new features"
        ),
        tags$a(
          href    = "https://forms.gle/NDJJKj7XrsnFH6m16",
          target  = "_blank",
          class   = "btn btn-sm btn-primary",
          style   = "margin-left: 1rem;",
          icon("envelope"),
          "Submit feedback"
        ),
        
        h4("Version History"),
        tags$ul(
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

# Server Logic --------------------------------------------------------------

server <- function(input, output, session) {
  
  # Load data on startup
  baseball_data <- load_baseball_data()
  
  # Update player choices when data is available
  observe({
    if (nrow(baseball_data$lookup) > 0) {
      # Use compound_id if available, fallback to PlayerId for backwards compatibility
      if ("compound_id" %in% colnames(baseball_data$lookup)) {
        player_choices <- setNames(baseball_data$lookup$compound_id, baseball_data$lookup$display_name)
      } else {
        player_choices <- setNames(baseball_data$lookup$PlayerId, baseball_data$lookup$display_name)
      }
      updateSelectInput(session, "player_selection", choices = c("Select a player..." = "", player_choices))
    } else {
      # Show error message in dropdown when no data
      updateSelectInput(session, "player_selection", choices = c("⚠️ Data not loaded - check logs" = ""))
    }
  })
  
  # Data status display
  output$data_status <- renderUI({
    if (nrow(baseball_data$lookup) == 0) {
      return(div(class = "alert alert-warning", "No data loaded"))
    }
    
    coverage_stats <- calculate_photo_coverage(baseball_data)
    
    div(
      class = "data-status",
      style = "background: rgba(255, 255, 255, 0.9); padding: 1.5rem; border-radius: 15px; backdrop-filter: blur(10px); border: 1px solid rgba(255, 255, 255, 0.3);",
      p(strong("Players loaded:"), paste(coverage_stats$hitters_total, "hitters,", coverage_stats$pitchers_total, "pitchers")),
      p(strong("MLB photo coverage:"), 
        paste0(coverage_stats$hitters_with_mlb, "/", coverage_stats$hitters_total, " hitters (", 
               round(100 * coverage_stats$hitters_with_mlb / max(coverage_stats$hitters_total, 1), 1), "%)"),
        br(),
        paste0(coverage_stats$pitchers_with_mlb, "/", coverage_stats$pitchers_total, " pitchers (", 
               round(100 * coverage_stats$pitchers_with_mlb / max(coverage_stats$pitchers_total, 1), 1), "%)")
      ),
      if (coverage_stats$hitters_with_mlb == 0 && coverage_stats$pitchers_with_mlb == 0) {
        div(class = "alert alert-info", 
            "No MLB IDs found. Photos will use FanGraphs fallback or placeholders.")
      } else {
        NULL
      }
    )
  })
  
  # Player info card display
  output$player_info_card <- renderUI({
    if (input$player_selection == "") {
      return(NULL)
    }
    
    create_player_card(input$player_selection, baseball_data)
  })
  
  # Main analysis output
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
    
    # Perform analysis with progress indicator
    withProgress(message = 'Analyzing player performance...', value = 0, {
      incProgress(0.3, detail = "Preparing analysis")
      analysis_result <- analyze_player_performance(input$player_selection, input$analysis_mode, baseball_data)
      incProgress(1, detail = "Complete")
    })
    
    # Combine analysis and visualization
    tagList(
      div(class = "analysis-content", analysis_result),
      hr(style = "border-color: rgba(46, 134, 171, 0.3); margin: 2rem 0;"),
      renderPlot({
        trends_plot <- create_player_trends_plot(input$player_selection, baseball_data)
        if (is.null(trends_plot)) {
          # Empty state plot
          ggplot() + 
            geom_text(aes(x = 1, y = 1, label = "No trend data available"), size = 6, color = "#6c757d") +
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