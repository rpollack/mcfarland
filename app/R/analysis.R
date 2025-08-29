# Analysis Functions --------------------------------------------------------

#' Safe value formatting for display using tidyverse
#' @param x Numeric value to format
#' @return Formatted string
format_stat_value <- function(x) {
  case_when(
    is.na(x) | is.null(x) ~ "N/A",
    is.numeric(x) ~ as.character(round(x, 3)),
    TRUE ~ as.character(x)
  )
}

#' Get analysis persona prompt using tidyverse
#' @param mode Analysis mode string
#' @return Persona prompt text
get_analysis_persona <- function(mode) {
  personas <- list(
    analytics_dork = "You are a front office nerd, raised on moneyball and new school stats, always at the cutting edge. You favor new school stats, talk in probabilities, and are very dismissive of people who don't believe you. You might be the smartest person in the room, but people would describe you as a real tool. Be ruthless and dismissive!",
    old_coot = "You are a deranged old coot, ranting and raving about everything. Yell a lot. People would describe you as 'off your meds'. Throw in references to people spying on you. Appear confused at times. Get stats wrong occasionally. You know, just -- be insane.",
    gen_z = "You're an over the top Gen Z'er, using lots of slang, referencing hyper modern trends, apps, emojis, and such. But really lay it on thick, in a humorously over-the-top kind of way.",
    seventies = "You prefer 1970s style of baseball, when men were men, stolen bases were high, starting pitchers completed every game, and guys had bushy mustaches and chewed tobacco all game. You strongly prefer old school stats to new school ones. Use lots of comparisons to famous 1970s baseball players: Pete Rose, Johnny Bench, Mike Schmidt, Willie Stargell, Rod Carew, Bobby Grich, Thurman Munson, etc -- but don't limit your comparisons to just these guys.",
    sensationalist = "You report baseball analysis like a carnival barker in the jazz age: always trying to make things larger than life through flowery prose and colorful headlines. You practice sensationalist, ballyhoo sportswriting and yellow-journalism-style copy. Every flaw is a titanic tragedy, and every positive is a starry-eyed bright and shiny future.",
    shakespeare = "You are William Shakespeare. Not just that, but you speak in verse -- preferably iambic pentameter.",
    rose_colored_glasses = "You always find the positive. Cherry pick analysis and narratvies that accentuate the positive trends of the player, even if it means overlooking negative aspects or signs."
  )

  personas[[mode]] %||% "Keep it simple and easy to understand. Use short but friendly sentences. Don't start with asides or extraneous clauses. Start your response with the conclusion/summary takeaways, then underneath, list your evidence for that summary and those conclusions."
}

#' Call OpenAI API for analysis with caching
#' @param prompt_text Analysis prompt
#' @param analysis_mode Analysis style mode
#' @return HTML formatted response
call_openai_api <- function(prompt_text, analysis_mode) {
  # CACHE: Generate cache key and check cache first
  cache_key <- generate_cache_key(prompt_text, analysis_mode)

  # Try to load from cache
  cached_result <- load_api_response(cache_key)
  if (!is.null(cached_result)) {
    return(cached_result)
  }

  # Cache miss - proceed with API call
  cat("🌐 Making OpenAI API call (cache miss)\n")

  api_key <- Sys.getenv("OPENAI_API_KEY")

  if (api_key == "") {
    result <- HTML(str_glue(
      "<div class='alert alert-info'>",
      "<h5>OpenAI API Key Not Set</h5>",
      "<p>Set OPENAI_API_KEY environment variable to enable analysis.</p>",
      "<details><summary>View prompt</summary><pre>{htmlEscape(prompt_text)}</pre></details>",
      "</div>"
    ))
    return(result)
  }

  persona_prompt <- get_analysis_persona(analysis_mode)

  full_prompt <- str_glue(
    "Here is current-year performance data for a player:\n\n{prompt_text}\n\n",
    "General instructions:\n\n",
    "Please analyze how the player is performing this year, what trends stand out, and whether any aspects of the performance appear to be skill- or luck-driven. Incorporate a prediction: will the player improve, decline, or stay the same for the rest of the season? Explain your reasoning.\n\n",
    "The very first element of the response should be a title that encompasses your findings.\n\n",
    "Your analysis must incorporate metric, direction, and magnitude of difference. For example BB% is up, indicate by how much, and what the size of that gap might indicate. You don't need to explicitly call out this framing (e.g. in bullets), just make sure to weave it into your analysis.\n\n",
    "Separate your analysis into core skills and luck/regression indicators.\n\n",
    "Don't repeat yourself. For example, if you say a stat or performance or trend is 'lucky', you don't need to say it's 'not unlucky'.\n\n",
    "Remember that when it comes to stats and trends, you only have knowledge of two things: a player's current-year stats and the average of the same stats for the past 3 years (e.g. not their entire career). So when you say things like a stat is 'up' or 'down', make it clear that this is relative to the last 3 years' average.\n\n",
    "Here is your persona that should inform your writing style and response, even if it means overriding those previous instructions: {persona_prompt}"
  )

  result <- tryCatch(
    {
      response <- POST(
        "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = str_glue("Bearer {api_key}")),
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
        return(HTML(str_glue("<div class='alert alert-danger'>API Error: {parsed_response$error$message}</div>")))
      }

      if (is.null(parsed_response$choices) || length(parsed_response$choices) == 0) {
        return(HTML("<div class='alert alert-warning'>No response from API.</div>"))
      }

      analysis_text <- parsed_response$choices[[1]]$message$content
      HTML(commonmark::markdown_html(analysis_text))
    },
    error = function(e) {
      HTML(str_glue("<div class='alert alert-danger'>Error: {e$message}</div>"))
    }
  )

  # CACHE: Save successful results to cache
  if (!is.null(result) && !str_detect(as.character(result), "alert-danger")) {
    save_api_response(cache_key, result)
  }

  return(result)
}

#' Build hitter analysis prompt using tidyverse
#' @param player_name Player name
#' @param hitter_data Hitter statistics data frame
#' @return Analysis prompt text
build_hitter_prompt <- function(player_name, hitter_data) {
  data <- hitter_data %>% filter(Name == player_name)
  if (nrow(data) == 0) {
    return(NULL)
  }

  # Build prompt sections using tidyverse string manipulation
  header_section <- c(
    str_glue("Player: {player_name} (Hitter)"),
    "",
    "--- Key metrics to analyze---",
    str_glue("Age: {format_stat_value(data$Age)}"),
    str_glue("Year: {CURRENT_YEAR}"),
    str_glue("Plate Appearances (PA): {format_stat_value(data$PA_cur)}")
  )

  metrics_section <- c(
    str_glue("AVG: {format_stat_value(data$AVG_cur)}  Last 3 Years: {format_stat_value(data$AVG_l3)}  Diff: {format_stat_value(data$AVG_diff)}"),
    str_glue("OBP: {format_stat_value(data$OBP_cur)}  Last 3 Years: {format_stat_value(data$OBP_l3)}  Diff: {format_stat_value(data$OBP_diff)}"),
    str_glue("SLG: {format_stat_value(data$SLG_cur)}  Last 3 Years: {format_stat_value(data$SLG_l3)}  Diff: {format_stat_value(data$SLG_diff)}"),
    str_glue("K%: {format_stat_value(data$K_pct_cur)}  Last 3 Years: {format_stat_value(data$K_pct_l3)}  Diff: {format_stat_value(data$K_pct_diff)}"),
    str_glue("BB%: {format_stat_value(data$BB_pct_cur)}  Last 3 Years: {format_stat_value(data$BB_pct_l3)}  Diff: {format_stat_value(data$BB_pct_diff)}"),
    str_glue("Barrel%: {format_stat_value(data$Barrel_pct_cur)}  Last 3 Years: {format_stat_value(data$Barrel_pct_l3)}  Diff: {format_stat_value(data$Barrel_pct_diff)}"),
    str_glue("BABIP: {format_stat_value(data$BABIP_cur)}  Last 3 Years: {format_stat_value(data$BABIP_l3)}  Diff: {format_stat_value(data$BABIP_diff)}"),
    str_glue("wOBA: {format_stat_value(data$wOBA_cur)}  Last 3 Years: {format_stat_value(data$wOBA_l3)}  Diff: {format_stat_value(data$wOBA_diff)}"),
    str_glue("xwOBA: {format_stat_value(data$xwOBA_cur)}  Last 3 Years: {format_stat_value(data$xwOBA_l3)}  Diff: {format_stat_value(data$xwOBA_diff)}"),
    str_glue("xwOBA-wOBA gap: {format_stat_value(data$xwOBA_wOBA_gap_cur)}  Last 3 Years: {format_stat_value(data$xwOBA_wOBA_gap_l3)}  Diff: {format_stat_value(data$xwOBA_wOBA_gap_diff)}")
  )

  notes_section <- c(
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

  c(header_section, metrics_section, notes_section) %>% str_c(collapse = "\n")
}

#' Build pitcher analysis prompt using tidyverse
#' @param player_name Player name
#' @param pitcher_data Pitcher statistics data frame
#' @return Analysis prompt text
build_pitcher_prompt <- function(player_name, pitcher_data) {
  data <- pitcher_data %>% filter(Name == player_name)
  if (nrow(data) == 0) {
    return(NULL)
  }

  # Build prompt sections using tidyverse string manipulation
  header_section <- c(
    str_glue("Player: {player_name} (Pitcher)"),
    "",
    "--- Key metrics to analyze---",
    str_glue("Age: {format_stat_value(data$Age)}"),
    str_glue("Year: {CURRENT_YEAR}"),
    str_glue("Position: {if_else('position' %in% names(data), as.character(data$position), 'Pitcher')}"),
    str_glue("Total Batters Faced: {format_stat_value(data$tbf)}"),
    ""
  )

  metrics_section <- c(
    str_glue("ERA: {format_stat_value(data$era_cur)}  Last 3 Years: {format_stat_value(data$era_l3)}  Diff: {format_stat_value(data$era_diff)}"),
    str_glue("xERA: {format_stat_value(data$xera_cur)}  Last 3 Years: {format_stat_value(data$xera_l3)}  Diff: {format_stat_value(data$xera_diff)}"),
    str_glue("BABIP: {format_stat_value(data$babip_cur)}  Last 3 Years: {format_stat_value(data$babip_l3)}  Diff: {format_stat_value(data$babip_diff)}"),
    str_glue("Barrel Rate: {format_stat_value(data$barrel_percent_cur)}%  Last 3 Years: {format_stat_value(data$barrel_percent_l3)}%  Diff: {format_stat_value(data$barrel_percent_diff)}%"),
    str_glue("Strikeout Rate (K%): {format_stat_value(data$k_percent_cur)}%  Last 3 Years: {format_stat_value(data$k_percent_l3)}%  Diff: {format_stat_value(data$k_percent_diff)}%"),
    str_glue("Called Strike & Whiff Rate (CSW%): {format_stat_value(data$csw_percent_cur)}%  Last 3 Years: {format_stat_value(data$csw_percent_l3)}%  Diff: {format_stat_value(data$csw_percent_diff)}%"),
    str_glue("Outside Zone Swing Rate (O-Swing%): {format_stat_value(data$o_swing_percent_cur)}%  Last 3 Years: {format_stat_value(data$o_swing_percent_l3)}%  Diff: {format_stat_value(data$o_swing_percent_diff)}%"),
    str_glue("Walk Rate (BB%): {format_stat_value(data$bb_percent_cur)}%  Last 3 Years: {format_stat_value(data$bb_percent_l3)}%  Diff: {format_stat_value(data$bb_percent_diff)}%"),
    str_glue("K-BB%: {format_stat_value(data$k_minus_bb_percent_cur)}%  Last 3 Years: {format_stat_value(data$k_minus_bb_percent_l3)}%  Diff: {format_stat_value(data$k_minus_bb_percent_diff)}%"),
    str_glue("LOB% (Left-on-base rate): {format_stat_value(data$lob_percent_cur)}%  Last 3 Years: {format_stat_value(data$lob_percent_l3)}%  Diff: {format_stat_value(data$lob_percent_diff)}%")
  )

  notes_section <- c(
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

  c(header_section, metrics_section, notes_section) %>% str_c(collapse = "\n")
}

#' Analyze hitter performance using tidyverse
#' @param player_name Player name
#' @param analysis_mode Analysis style mode
#' @param hitter_data Hitter statistics data frame
#' @return HTML analysis result
analyze_hitter_performance <- function(player_name, analysis_mode, hitter_data) {
  if (nrow(hitter_data %>% filter(Name == player_name)) == 0) {
    return(HTML(str_glue("<div class='alert alert-warning'>Hitter not found: {player_name}</div>")))
  }

  prompt <- build_hitter_prompt(player_name, hitter_data)
  if (is.null(prompt)) {
    return(HTML(str_glue("<div class='alert alert-warning'>Unable to build analysis for: {player_name}</div>")))
  }

  call_openai_api(prompt, analysis_mode)
}

#' Analyze pitcher performance using tidyverse
#' @param player_name Player name
#' @param analysis_mode Analysis style mode
#' @param pitcher_data Pitcher statistics data frame
#' @return HTML analysis result
analyze_pitcher_performance <- function(player_name, analysis_mode, pitcher_data) {
  if (nrow(pitcher_data %>% filter(Name == player_name)) == 0) {
    return(HTML(str_glue("<div class='alert alert-warning'>Pitcher not found: {player_name}</div>")))
  }

  prompt <- build_pitcher_prompt(player_name, pitcher_data)
  if (is.null(prompt)) {
    return(HTML(str_glue("<div class='alert alert-warning'>Unable to build analysis for: {player_name}</div>")))
  }

  call_openai_api(prompt, analysis_mode)
}

# Improved generate_quick_insight with better error handling
generate_quick_insight <- function(player_data, player_type) {
  # Safety checks
  if (is.null(player_data) || nrow(player_data) == 0) {
    return("Player data loaded and ready for analysis.")
  }

  if (is.null(player_type) || !player_type %in% c("hitter", "pitcher")) {
    return("Player statistics available for detailed analysis.")
  }

  # Wrap the analysis logic in tryCatch for safety
  tryCatch(
    {
      positive_changes <- c()
      negative_changes <- c()

      if (player_type == "hitter") {
        # Positive changes for hitters - with proper NA checks
        if (!is.na(player_data$AVG_diff) && is.numeric(player_data$AVG_diff) && player_data$AVG_diff > 0.02) {
          positive_changes <- c(positive_changes, "batting average is up")
        }
        if (!is.na(player_data$K_pct_diff) && is.numeric(player_data$K_pct_diff) && player_data$K_pct_diff < -2) {
          positive_changes <- c(positive_changes, "strikeouts are down")
        }
        if (!is.na(player_data$BB_pct_diff) && is.numeric(player_data$BB_pct_diff) && player_data$BB_pct_diff > 1.5) {
          positive_changes <- c(positive_changes, "walks are up")
        }
        if (!is.na(player_data$Barrel_pct_diff) && is.numeric(player_data$Barrel_pct_diff) && player_data$Barrel_pct_diff > 2) {
          positive_changes <- c(positive_changes, "hard contact is up")
        }

        # Negative changes for hitters - with proper NA checks
        if (!is.na(player_data$AVG_diff) && is.numeric(player_data$AVG_diff) && player_data$AVG_diff < -0.02) {
          negative_changes <- c(negative_changes, "batting average is down")
        }
        if (!is.na(player_data$K_pct_diff) && is.numeric(player_data$K_pct_diff) && player_data$K_pct_diff > 2) {
          negative_changes <- c(negative_changes, "strikeouts are up")
        }
        if (!is.na(player_data$BB_pct_diff) && is.numeric(player_data$BB_pct_diff) && player_data$BB_pct_diff < -1.5) {
          negative_changes <- c(negative_changes, "walks are down")
        }
        if (!is.na(player_data$Barrel_pct_diff) && is.numeric(player_data$Barrel_pct_diff) && player_data$Barrel_pct_diff < -2) {
          negative_changes <- c(negative_changes, "hard contact is down")
        }
      } else { # pitcher - with proper NA checks
        # Positive changes for pitchers
        if (!is.na(player_data$era_diff) && is.numeric(player_data$era_diff) && player_data$era_diff < -0.5) {
          positive_changes <- c(positive_changes, "ERA is down")
        }
        if (!is.na(player_data$k_percent_diff) && is.numeric(player_data$k_percent_diff) && player_data$k_percent_diff > 2) {
          positive_changes <- c(positive_changes, "strikeouts are up")
        }
        if (!is.na(player_data$bb_percent_diff) && is.numeric(player_data$bb_percent_diff) && player_data$bb_percent_diff < -1.5) {
          positive_changes <- c(positive_changes, "walks allowed are down")
        }
        if (!is.na(player_data$barrel_percent_diff) && is.numeric(player_data$barrel_percent_diff) && player_data$barrel_percent_diff < -1) {
          positive_changes <- c(positive_changes, "hard contact allowed is down")
        }

        # Negative changes for pitchers
        if (!is.na(player_data$era_diff) && is.numeric(player_data$era_diff) && player_data$era_diff > 0.5) {
          negative_changes <- c(negative_changes, "ERA is up")
        }
        if (!is.na(player_data$k_percent_diff) && is.numeric(player_data$k_percent_diff) && player_data$k_percent_diff < -2) {
          negative_changes <- c(negative_changes, "strikeouts are down")
        }
        if (!is.na(player_data$bb_percent_diff) && is.numeric(player_data$bb_percent_diff) && player_data$bb_percent_diff > 1.5) {
          negative_changes <- c(negative_changes, "walks allowed are up")
        }
        if (!is.na(player_data$barrel_percent_diff) && is.numeric(player_data$barrel_percent_diff) && player_data$barrel_percent_diff > 1) {
          negative_changes <- c(negative_changes, "hard contact allowed is up")
        }
      }

      # Calculate regression likelihood safely
      regression_risk <- assess_regression_likelihood(player_data, player_type)

      # Build the insight message
      performance_text <- ""
      if (length(positive_changes) > 0 && length(negative_changes) > 0) {
        # Mixed performance
        pos_text <- if (length(positive_changes) > 1) {
          str_c(positive_changes[1:min(2, length(positive_changes))], collapse = " and ")
        } else {
          positive_changes[1]
        }
        neg_text <- if (length(negative_changes) > 1) {
          str_c(negative_changes[1:min(2, length(negative_changes))], collapse = " and ")
        } else {
          negative_changes[1]
        }
        performance_text <- str_glue("Mixed performance: {pos_text}, but {neg_text}.")
      } else if (length(positive_changes) > 0) {
        # Only positive changes
        if (length(positive_changes) == 1) {
          performance_text <- str_glue("Improved performance: {positive_changes[1]}.")
        } else if (length(positive_changes) == 2) {
          performance_text <- str_glue("Improved performance: {positive_changes[1]} and {positive_changes[2]}.")
        } else {
          performance_text <- str_glue("Strong improvement: {positive_changes[1]}, {positive_changes[2]}, and more.")
        }
      } else if (length(negative_changes) > 0) {
        # Only negative changes
        if (length(negative_changes) == 1) {
          performance_text <- str_glue("Concerning trend: {negative_changes[1]}.")
        } else if (length(negative_changes) == 2) {
          performance_text <- str_glue("Concerning trends: {negative_changes[1]} and {negative_changes[2]}.")
        } else {
          performance_text <- str_glue("Multiple concerns: {negative_changes[1]}, {negative_changes[2]}, and more.")
        }
      } else {
        # No significant changes
        performance_text <- "Performance is similar to recent seasons."
      }

      # Combine performance text with regression assessment
      return(str_glue("{performance_text} Likelihood of regression: {regression_risk}."))
    },
    error = function(e) {
      cat("❌ Error in generate_quick_insight:", e$message, "\n")
      return("Player data available for detailed analysis.")
    }
  )
}
# Assess regression likelihood based on luck indicators
# Fixed assess_regression_likelihood function

assess_regression_likelihood <- function(player_data, player_type) {
  if (is.null(player_data) || nrow(player_data) == 0) {
    return("low")
  }

  luck_indicators <- c()

  if (player_type == "hitter") {
    # BABIP analysis - FIXED: Proper NA handling
    babip_cur <- player_data$BABIP_cur
    babip_l3 <- player_data$BABIP_l3

    if (!is.null(babip_cur) && !is.null(babip_l3) &&
      !is.na(babip_cur) && !is.na(babip_l3) &&
      is.numeric(babip_cur) && is.numeric(babip_l3)) {
      babip_diff <- babip_cur - babip_l3
      if (!is.na(babip_diff) && abs(babip_diff) > 0.025) {
        if (babip_diff > 0) {
          luck_indicators <- c(luck_indicators, "high_babip")
        } else {
          luck_indicators <- c(luck_indicators, "low_babip")
        }
      }
    }

    # xwOBA-wOBA gap analysis - FIXED: Proper NA handling
    current_gap <- player_data$xwOBA_wOBA_gap_cur
    historical_gap <- player_data$xwOBA_wOBA_gap_l3

    if (!is.null(current_gap) && !is.na(current_gap) && is.numeric(current_gap)) {
      historical_gap <- if (is.null(historical_gap) || is.na(historical_gap)) 0 else historical_gap

      if (abs(current_gap) > 0.015 || abs(current_gap - historical_gap) > 0.020) {
        if (current_gap < -0.015) {
          luck_indicators <- c(luck_indicators, "lucky_woba")
        } else if (current_gap > 0.015) {
          luck_indicators <- c(luck_indicators, "unlucky_woba")
        }
      }
    }

    # Barrel rate vs BABIP inconsistency - FIXED: Proper NA handling
    barrel_diff <- player_data$Barrel_pct_diff
    babip_diff <- player_data$BABIP_diff

    if (!is.null(barrel_diff) && !is.null(babip_diff) &&
      !is.na(barrel_diff) && !is.na(babip_diff) &&
      is.numeric(barrel_diff) && is.numeric(babip_diff)) {
      if ((barrel_diff < -1 && babip_diff > 0.02) ||
        (barrel_diff > 1 && babip_diff < -0.02)) {
        luck_indicators <- c(luck_indicators, "contact_babip_mismatch")
      }
    }
  } else { # pitcher
    # BABIP analysis - FIXED: Proper NA handling
    babip_cur <- player_data$babip_cur
    babip_l3 <- player_data$babip_l3

    if (!is.null(babip_cur) && !is.null(babip_l3) &&
      !is.na(babip_cur) && !is.na(babip_l3) &&
      is.numeric(babip_cur) && is.numeric(babip_l3)) {
      babip_diff <- babip_cur - babip_l3
      if (!is.na(babip_diff) && abs(babip_diff) > 0.025) {
        if (babip_diff < 0) {
          luck_indicators <- c(luck_indicators, "lucky_babip")
        } else {
          luck_indicators <- c(luck_indicators, "unlucky_babip")
        }
      }
    }

    # ERA vs xERA gap - FIXED: Proper NA handling
    era_cur <- player_data$era_cur
    xera_cur <- player_data$xera_cur

    if (!is.null(era_cur) && !is.null(xera_cur) &&
      !is.na(era_cur) && !is.na(xera_cur) &&
      is.numeric(era_cur) && is.numeric(xera_cur)) {
      era_xera_gap <- era_cur - xera_cur
      if (!is.na(era_xera_gap) && abs(era_xera_gap) > 0.30) {
        if (era_xera_gap < -0.30) {
          luck_indicators <- c(luck_indicators, "lucky_era")
        } else if (era_xera_gap > 0.30) {
          luck_indicators <- c(luck_indicators, "unlucky_era")
        }
      }
    }

    # LOB% analysis - FIXED: Proper NA handling
    lob_cur <- player_data$lob_percent_cur
    lob_l3 <- player_data$lob_percent_l3

    if (!is.null(lob_cur) && !is.null(lob_l3) &&
      !is.na(lob_cur) && !is.na(lob_l3) &&
      is.numeric(lob_cur) && is.numeric(lob_l3)) {
      lob_diff <- lob_cur - lob_l3
      if (!is.na(lob_diff) && abs(lob_diff) > 3) {
        if (lob_diff > 3) {
          luck_indicators <- c(luck_indicators, "high_lob")
        } else if (lob_diff < -3) {
          luck_indicators <- c(luck_indicators, "low_lob")
        }
      }
    }

    # Barrel rate vs ERA inconsistency - FIXED: Proper NA handling
    barrel_diff <- player_data$barrel_percent_diff
    era_diff <- player_data$era_diff

    if (!is.null(barrel_diff) && !is.null(era_diff) &&
      !is.na(barrel_diff) && !is.na(era_diff) &&
      is.numeric(barrel_diff) && is.numeric(era_diff)) {
      if ((barrel_diff > 1 && era_diff < -0.5) ||
        (barrel_diff < -1 && era_diff > 0.5)) {
        luck_indicators <- c(luck_indicators, "barrel_era_mismatch")
      }
    }
  }

  # Determine regression likelihood based on luck indicators
  luck_count <- length(luck_indicators)

  if (luck_count == 0) {
    return("low")
  }

  lucky_indicators <- sum(str_detect(luck_indicators, "lucky|high_babip|high_lob"))
  unlucky_indicators <- sum(str_detect(luck_indicators, "unlucky|low_babip|low_lob"))
  mismatch_indicators <- sum(str_detect(luck_indicators, "mismatch"))

  if (luck_count >= 3 || mismatch_indicators >= 2) {
    return("high")
  } else if (luck_count == 2 || (lucky_indicators >= 1 && unlucky_indicators >= 1)) {
    return("medium")
  } else if (luck_count == 1) {
    return("medium")
  } else {
    return("low")
  }
}
#' Main player analysis router using tidyverse
#' @param player_id FanGraphs player ID
#' @param analysis_mode Analysis style mode
#' @param baseball_data Complete baseball data list
#' @return HTML analysis result
analyze_player_performance <- function(player_id, analysis_mode, baseball_data) {
  player_info <- get_player_info(player_id, baseball_data)
  if (is.null(player_info)) {
    return(HTML("<div class='alert alert-warning'>Player not found.</div>"))
  }

  case_when(
    player_info$type == "hitter" ~ analyze_hitter_performance(player_info$name, analysis_mode, baseball_data$hitters),
    player_info$type == "pitcher" ~ analyze_pitcher_performance(player_info$name, analysis_mode, baseball_data$pitchers),
    TRUE ~ HTML("<div class='alert alert-warning'>Unknown player type.</div>")
  )
}

