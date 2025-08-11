# McFARLAND - Clean syntax version with Player Headshots
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

# Load data function
load_baseball_data <- function() {
  base_url <- "https://raw.githubusercontent.com/rpollack/leadRboard/master/"
  
  cat("Loading data from GitHub...\n")
  
  tryCatch({
    hitters <- read_csv(paste0(base_url, "full_stats_hitters.csv"), show_col_types = FALSE)
    pitchers <- read_csv(paste0(base_url, "full_stats_pitchers.csv"), show_col_types = FALSE)
    lookup <- read_csv(paste0(base_url, "player_lookup.csv"), show_col_types = FALSE)
    
    cat("Loaded:", nrow(hitters), "hitters,", nrow(pitchers), "pitchers\n")
    
    list(hitters = hitters, pitchers = pitchers, lookup = lookup)
    
  }, error = function(e) {
    cat("Error loading data:", e$message, "\n")
    list(
      hitters = data.frame(),
      pitchers = data.frame(),
      lookup = data.frame(display_name = character(0), PlayerId = character(0), player_type = character(0))
    )
  })
}

# Function to get player headshot URL
get_player_headshot_url <- function(player_id, baseball_data) {
  # First check if this player exists in our lookup
  player_info <- baseball_data$lookup[baseball_data$lookup$PlayerId == player_id, ]
  if (nrow(player_info) == 0) return("https://via.placeholder.com/200x200/2E86AB/ffffff?text=Player")
  
  player_name <- player_info$Name
  player_type <- player_info$player_type
  mlb_id <- NULL
  
  # Get MLB ID based on player type
  if (player_type == "hitter" && nrow(baseball_data$hitters) > 0) {
    player_data <- baseball_data$hitters[baseball_data$hitters$PlayerId == player_id, ]
    if (nrow(player_data) > 0 && "mlbamid" %in% colnames(player_data)) {
      mlb_id <- player_data$mlbamid[1]
    }
  } else if (player_type == "pitcher" && nrow(baseball_data$pitchers) > 0) {
    player_data <- baseball_data$pitchers[baseball_data$pitchers$PlayerId == player_id, ]
    if (nrow(player_data) > 0 && "mlbamid" %in% colnames(player_data)) {
      mlb_id <- player_data$mlbamid[1]
    }
  }
  
  # Try MLB headshot URL if we have a valid MLB ID (using correct MLB URL format)
  if (!is.null(mlb_id) && !is.na(mlb_id) && mlb_id != "" && mlb_id != 0) {
    return(paste0("https://img.mlbstatic.com/mlb-photos/image/upload/w_213,d_people:generic:headshot:silo:current.png,q_auto:best,f_auto/v1/people/", mlb_id, "/headshot/67/current"))
  }
  
  # Fallback 1: Try FanGraphs player photo using FG player ID
  fg_photo_url <- paste0("https://www.fangraphs.com/img/players/", player_id, ".jpg")
  
  # Return FanGraphs URL first, with JavaScript fallback to placeholder
  return(fg_photo_url)
}

# Function to create player info card with headshot and better error handling
create_player_info_card <- function(player_id, baseball_data) {
  player_info <- baseball_data$lookup[baseball_data$lookup$PlayerId == player_id, ]
  if (nrow(player_info) == 0) return(NULL)
  
  player_name <- player_info$Name
  player_type <- player_info$player_type
  headshot_url <- get_player_headshot_url(player_id, baseball_data)
  
  # Create fallback chain for photo loading
  mlb_fallback <- "https://via.placeholder.com/200x200/2E86AB/ffffff?text=⚾"
  
  # Get additional player info
  age <- NA
  position_info <- ""
  
  if (player_type == "hitter" && nrow(baseball_data$hitters) > 0) {
    player_data <- baseball_data$hitters[baseball_data$hitters$PlayerId == player_id, ]
    if (nrow(player_data) > 0) {
      age <- player_data$Age[1]
      position_info <- "Hitter"
    }
  } else if (player_type == "pitcher" && nrow(baseball_data$pitchers) > 0) {
    player_data <- baseball_data$pitchers[baseball_data$pitchers$PlayerId == player_id, ]
    if (nrow(player_data) > 0) {
      age <- player_data$Age[1]
      if ("position" %in% colnames(player_data)) {
        position_info <- paste(player_data$position[1], "• Pitcher")
      } else {
        position_info <- "Pitcher"
      }
    }
  }
  
  div(
    class = "text-center player-info-card",
    
    # Player headshot with multiple fallbacks
    img(
      src = headshot_url,
      alt = paste("Photo of", player_name),
      class = "player-photo",
      onerror = paste0("this.onerror=null; this.src='", mlb_fallback, "';")
    ),
    
    # Player name
    h4(player_name, class = "player-name"),
    
    # Player details
    p(
      class = "player-details",
      if (!is.na(age)) paste("Age:", age),
      if (position_info != "") paste("•", position_info)
    )
  )
}

# Stats comparison plotting function
create_comparison_plot <- function(player_id, baseball_data) {
  player_info <- baseball_data$lookup[baseball_data$lookup$PlayerId == player_id, ]
  if (nrow(player_info) == 0) return(NULL)
  
  player_name <- player_info$Name
  player_type <- player_info$player_type
  
  if (player_type == "hitter" && nrow(baseball_data$hitters) > 0) {
    player_data <- baseball_data$hitters[baseball_data$hitters$PlayerId == player_id, ]
    if (nrow(player_data) == 0) return(NULL)
    
    # Key hitter metrics
    base_metrics <- c("AVG", "OBP", "SLG", "K_pct", "BB_pct", "Barrel_pct", "BABIP", "wOBA", "xwOBA")
  } else if (player_type == "pitcher" && nrow(baseball_data$pitchers) > 0) {
    player_data <- baseball_data$pitchers[baseball_data$pitchers$PlayerId == player_id, ]
    if (nrow(player_data) == 0) return(NULL)
    
    # Key pitcher metrics
    base_metrics <- c("era", "xera", "k_percent", "bb_percent", "k_minus_bb_percent", "barrel_percent", "babip", "lob_percent")
  } else {
    return(NULL)
  }
  
  # Create comparison data manually
  comparison_data <- data.frame()
  
  for (metric in base_metrics) {
    current_col <- paste0(metric, "_cur")
    avg_col <- paste0(metric, "_l3")
    
    # Check if both columns exist
    if (current_col %in% colnames(player_data) && avg_col %in% colnames(player_data)) {
      current_val <- player_data[[current_col]]
      avg_val <- player_data[[avg_col]]
      
      # Skip if values are missing
      if (!is.na(current_val) && !is.na(avg_val)) {
        # Add two rows - one for each period
        new_rows <- data.frame(
          player = c(player_name, player_name),
          metric = c(metric, metric),
          period = c("Past 3 years", "2025"),
          value = c(avg_val, current_val),
          stringsAsFactors = FALSE
        )
        comparison_data <- rbind(comparison_data, new_rows)
      }
    }
  }
  
  if (nrow(comparison_data) == 0) return(NULL)
  
  # Clean up metric names for display based on player type
  if (player_type == "hitter") {
    comparison_data$metric_display <- ifelse(comparison_data$metric == "K_pct", "K%",
                                             ifelse(comparison_data$metric == "BB_pct", "BB%",
                                                    ifelse(comparison_data$metric == "Barrel_pct", "Barrel%",
                                                           comparison_data$metric)))
  } else {
    comparison_data$metric_display <- ifelse(comparison_data$metric == "era", "ERA",
                                             ifelse(comparison_data$metric == "xera", "xERA", 
                                                    ifelse(comparison_data$metric == "k_percent", "K%",
                                                           ifelse(comparison_data$metric == "bb_percent", "BB%",
                                                                  ifelse(comparison_data$metric == "k_minus_bb_percent", "K-BB%",
                                                                         ifelse(comparison_data$metric == "barrel_percent", "Barrel%",
                                                                                ifelse(comparison_data$metric == "babip", "BABIP",
                                                                                       ifelse(comparison_data$metric == "lob_percent", "LOB%",
                                                                                              comparison_data$metric))))))))
  }
  
  # Create the plot
  ggplot(comparison_data, aes(
    x = factor(period, levels = c("Past 3 years", "2025")),
    y = value, group = metric_display
  )) +
    geom_point(size = 3, color = "#2E86AB") +
    geom_line(color = "#2E86AB") +
    facet_wrap(~metric_display, scales = "free_y") +
    labs(
      title = paste("Trends:", player_name),
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

# Safe value function
safe_val <- function(x) {
  if (is.na(x) || is.null(x)) {
    return("N/A")
  }
  if (is.numeric(x)) {
    return(round(x, 3))
  }
  return(as.character(x))
}

# OpenAI function
call_openai <- function(prompt_text, analysis_mode) {
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
  
  # Your original personas
  prompt_modifier <- switch(analysis_mode,
                            "analytics_dork" = "You are a front office nerd, raised on moneyball and new school stats, always at the cutting edge. You favor new school stats, talk in probabilities, and are very dismissive of people who don't believe you. You might be the smartest person in the room, but people would describe you as a real tool. Be ruthless and dismissive!",
                            "old_coot" = "You are a deranged old coot, ranting and raving about everything. Yell a lot. People would describe you as 'off your meds'. Throw in references to people spying on you. Appear confused at times. Get stats wrong occasionally. You know, just -- be insane.",
                            "gen_z" = "You're an over the top Gen Z'er, using lots of slang, referencing hyper modern trends, apps, emojis, and such. But really lay it on thick, in a humorously over-the-top kind of way.",
                            "seventies" = "You prefer 1970s style of baseball, when men were men, stolen bases were high, starting pitchers completed every game, and guys had bushy mustaches and chewed tobacco all game. You strongly prefer old school stats to new school ones. Use lots of comparisons to famous 1970s baseball players: Pete Rose, Johnny Bench, Mike Schmidt, Willie Stargell, Rod Carew, Bobby Grich, Thurman Munson, etc -- but don't limit your comparisons to just these guys.",
                            "sensationalist" = "You report baseball analysis like a carnival barker in the jazz age: always trying to make things larger than life through flowery prose and colorful headlines. You practice sensationalist, ballyhoo sportswriting and yellow-journalism-style copy. Every flaw is a titanic tragedy, and every positive is a starry-eyed bright and shiny future.",
                            "shakespeare" = "You are William Shakespeare. Not just that, but you speak in verse -- preferably iambic pentameter.",
                            "Keep it simple and easy to understand. Use short but friendly sentences. Don't start with asides or extraneous clauses. Start your response with the conclusion/summary takeaways, then underneath, list your evidence for that summary and those conclusions."
  )
  
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
    prompt_modifier
  )
  
  tryCatch({
    res <- POST(
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
    
    parsed <- content(res, as = "parsed")
    
    if (!is.null(parsed$error)) {
      return(HTML(paste0("<div class='alert alert-danger'>API Error: ", parsed$error$message, "</div>")))
    }
    
    if (is.null(parsed$choices) || length(parsed$choices) == 0) {
      return(HTML("<div class='alert alert-warning'>No response from API.</div>"))
    }
    
    text <- parsed$choices[[1]]$message$content
    return(HTML(commonmark::markdown_html(text)))
    
  }, error = function(e) {
    return(HTML(paste0("<div class='alert alert-danger'>Error: ", e$message, "</div>")))
  })
}

# Analyze hitter
analyze_hitter <- function(player_name, analysis_mode, hitter_data) {
  # Filter data
  data <- hitter_data[hitter_data$Name == player_name, ]
  
  if (nrow(data) == 0) {
    return(HTML(paste0("<div class='alert alert-warning'>Hitter not found: ", player_name, "</div>")))
  }
  
  # Your original detailed hitter prompt
  prompt_parts <- c(
    paste0("Player: ", player_name, " (Hitter)"),
    "",
    "--- Key metrics to analyze---",
    paste0("Age: ", safe_val(data$Age)),
    paste0("Year: 2025"),
    paste0("Plate Appearances (PA): ", safe_val(data$PA_cur)),
    paste0("AVG: ", safe_val(data$AVG_cur), "  Last 3 Years: ", safe_val(data$AVG_l3), "  Diff: ", safe_val(data$AVG_diff)),
    paste0("OBP: ", safe_val(data$OBP_cur), "  Last 3 Years: ", safe_val(data$OBP_l3), "  Diff: ", safe_val(data$OBP_diff)),
    paste0("SLG: ", safe_val(data$SLG_cur), "  Last 3 Years: ", safe_val(data$SLG_l3), "  Diff: ", safe_val(data$SLG_diff)),
    paste0("K%: ", safe_val(data$K_pct_cur), "  Last 3 Years: ", safe_val(data$K_pct_l3), "  Diff: ", safe_val(data$K_pct_diff)),
    paste0("BB%: ", safe_val(data$BB_pct_cur), "  Last 3 Years: ", safe_val(data$BB_pct_l3), "  Diff: ", safe_val(data$BB_pct_diff)),
    paste0("Barrel%: ", safe_val(data$Barrel_pct_cur), "  Last 3 Years: ", safe_val(data$Barrel_pct_l3), "  Diff: ", safe_val(data$Barrel_pct_diff)),
    paste0("BABIP: ", safe_val(data$BABIP_cur), "  Last 3 Years: ", safe_val(data$BABIP_l3), "  Diff: ", safe_val(data$BABIP_diff)),
    paste0("wOBA: ", safe_val(data$wOBA_cur), "  Last 3 Years: ", safe_val(data$wOBA_l3), "  Diff: ", safe_val(data$wOBA_diff)),
    paste0("xwOBA: ", safe_val(data$xwOBA_cur), "  Last 3 Years: ", safe_val(data$xwOBA_l3), "  Diff: ", safe_val(data$xwOBA_diff)),
    paste0("xwOBA-wOBA gap: ", safe_val(data$xwOBA_wOBA_gap_cur), "  Last 3 Years: ", safe_val(data$xwOBA_wOBA_gap_l3), "  Diff: ", safe_val(data$xwOBA_wOBA_gap_diff)),
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
  
  prompt <- paste(prompt_parts, collapse = "\n")
  
  call_openai(prompt, analysis_mode)
}

# Analyze pitcher
analyze_pitcher <- function(player_name, analysis_mode, pitcher_data) {
  # Filter data
  data <- pitcher_data[pitcher_data$Name == player_name, ]
  
  if (nrow(data) == 0) {
    return(HTML(paste0("<div class='alert alert-warning'>Pitcher not found: ", player_name, "</div>")))
  }
  
  # Your original detailed pitcher prompt
  prompt_parts <- c(
    paste0("Player: ", player_name, " (Pitcher)"),
    "",
    "--- Key metrics to analyze---",
    paste0("Age: ", safe_val(data$Age)),
    paste0("Year: 2025"),
    paste0("Position: ", ifelse("position" %in% names(data), safe_val(data$position), "Pitcher")),
    paste0("Total Batters Faced: ", safe_val(data$tbf)),
    "",
    paste0("ERA: ", safe_val(data$era_cur), "  Last 3 Years: ", safe_val(data$era_l3), "  Diff: ", safe_val(data$era_diff)),
    paste0("xERA: ", safe_val(data$xera_cur), "  Last 3 Years: ", safe_val(data$xera_l3), "  Diff: ", safe_val(data$xera_diff)),
    paste0("BABIP: ", safe_val(data$babip_cur), "  Last 3 Years: ", safe_val(data$babip_l3), "  Diff: ", safe_val(data$babip_diff)),
    paste0("Barrel Rate: ", safe_val(data$barrel_percent_cur), "%  Last 3 Years: ", safe_val(data$barrel_percent_l3), "%  Diff: ", safe_val(data$barrel_percent_diff), "%"),
    paste0("Strikeout Rate (K%): ", safe_val(data$k_percent_cur), "%  Last 3 Years: ", safe_val(data$k_percent_l3), "%  Diff: ", safe_val(data$k_percent_diff), "%"),
    paste0("Called Strike & Whiff Rate (CSW%): ", safe_val(data$csw_percent_cur), "%  Last 3 Years: ", safe_val(data$csw_percent_l3), "%  Diff: ", safe_val(data$csw_percent_diff), "%"),
    paste0("Outside Zone Swing Rate (O-Swing%): ", safe_val(data$o_swing_percent_cur), "%  Last 3 Years: ", safe_val(data$o_swing_percent_l3), "%  Diff: ", safe_val(data$o_swing_percent_diff), "%"),
    paste0("Walk Rate (BB%): ", safe_val(data$bb_percent_cur), "%  Last 3 Years: ", safe_val(data$bb_percent_l3), "%  Diff: ", safe_val(data$bb_percent_diff), "%"),
    paste0("K-BB%: ", safe_val(data$k_minus_bb_percent_cur), "%  Last 3 Years: ", safe_val(data$k_minus_bb_percent_l3), "%  Diff: ", safe_val(data$k_minus_bb_percent_diff), "%"),
    paste0("LOB% (Left-on-base rate): ", safe_val(data$lob_percent_cur), "%  Last 3 Years: ", safe_val(data$lob_percent_l3), "%  Diff: ", safe_val(data$lob_percent_diff), "%"),
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
  
  prompt <- paste(prompt_parts, collapse = "\n")
  
  call_openai(prompt, analysis_mode)
}

# Main analysis function
analyze_player <- function(player_id, analysis_mode, baseball_data) {
  # Find player
  player_info <- baseball_data$lookup[baseball_data$lookup$PlayerId == player_id, ]
  
  if (nrow(player_info) == 0) {
    return(HTML("<div class='alert alert-warning'>Player not found.</div>"))
  }
  
  player_name <- player_info$Name
  player_type <- player_info$player_type
  
  if (player_type == "hitter") {
    return(analyze_hitter(player_name, analysis_mode, baseball_data$hitters))
  } else if (player_type == "pitcher") {
    return(analyze_pitcher(player_name, analysis_mode, baseball_data$pitchers))
  } else {
    return(HTML("<div class='alert alert-warning'>Unknown player type.</div>"))
  }
}

# UI
ui <- page_navbar(
  title = HTML("McFARLAND <small style='font-size: 0.6em; color: #6c757d; font-weight: 400;'>🤖⚾ AI-powered baseball player analysis</small>"),
  
  header = tagList(
    # Viewport constraints for mobile - prevents zooming and wiggling
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0, user-scalable=no, viewport-fit=cover"),
    
    # Custom CSS for professional, app-like styling
    tags$style(HTML("
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
    ")),
    
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
        p("AI-powered baseball player analysis with player headshots."),
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

# Server
server <- function(input, output, session) {
  
  # Load data
  baseball_data <- load_baseball_data()
  
  # Update player choices
  observe({
    if (nrow(baseball_data$lookup) > 0) {
      choices <- setNames(baseball_data$lookup$PlayerId, baseball_data$lookup$display_name)
      updateSelectInput(session, "player_selection", choices = c("Select a player..." = "", choices))
    }
  })
  
  # Data status output
  output$data_status <- renderUI({
    if (nrow(baseball_data$lookup) == 0) {
      return(div(class = "alert alert-warning", "No data loaded"))
    }
    
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
    
    div(
      class = "data-status",
      style = "background: rgba(255, 255, 255, 0.9); padding: 1.5rem; border-radius: 15px; backdrop-filter: blur(10px); border: 1px solid rgba(255, 255, 255, 0.3);",
      p(strong("Players loaded:"), paste(hitters_total, "hitters,", pitchers_total, "pitchers")),
      p(strong("MLB photo coverage:"), 
        paste0(hitters_with_mlb, "/", hitters_total, " hitters (", 
               round(100 * hitters_with_mlb / max(hitters_total, 1), 1), "%)"),
        br(),
        paste0(pitchers_with_mlb, "/", pitchers_total, " pitchers (", 
               round(100 * pitchers_with_mlb / max(pitchers_total, 1), 1), "%)")
      ),
      if (hitters_with_mlb == 0 && pitchers_with_mlb == 0) {
        div(class = "alert alert-info", 
            "No MLB IDs found. Photos will use FanGraphs fallback or placeholders.")
      } else {
        NULL
      }
    )
  })
  
  # Player info card with headshot
  output$player_info_card <- renderUI({
    if (input$player_selection == "") {
      return(NULL)
    }
    
    create_player_info_card(input$player_selection, baseball_data)
  })
  
  # Main output
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
    
    # Show loading message while analysis runs
    withProgress(message = 'Analyzing player performance...', value = 0, {
      incProgress(0.3, detail = "Preparing analysis")
      main_analysis <- analyze_player(input$player_selection, input$analysis_mode, baseball_data)
      incProgress(1, detail = "Complete")
    })
    
    # Combine main analysis and plots
    tagList(
      div(class = "analysis-content", main_analysis),
      hr(style = "border-color: rgba(46, 134, 171, 0.3); margin: 2rem 0;"),
      renderPlot({
        plot_result <- create_comparison_plot(input$player_selection, baseball_data)
        if (is.null(plot_result)) {
          # Return empty plot if no data
          ggplot() + 
            geom_text(aes(x = 1, y = 1, label = "No trend data available"), size = 6, color = "#6c757d") +
            theme_void() +
            theme(plot.background = element_rect(fill = "white", color = NA))
        } else {
          plot_result
        }
      }, height = 400)
    )
  })
}

shinyApp(ui, server)