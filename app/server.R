# Server
# Complete Server Logic with Internal UI Functions and Trends in Step 1
server <- function(input, output, session) {
  # Generate user ID on session start
  user_id <- generate_user_id(session)
  
  # Keep-alive ping handler
  observeEvent(input$keepalive_ping,
               {
                 if (!is.null(input$keepalive_ping)) {
                   timestamp <- as.POSIXct(input$keepalive_ping / 1000, origin = "1970-01-01")
                   cat(
                     "💓 Keep-alive ping received at:", format(timestamp, "%H:%M:%S"),
                     "- User:", substr(user_id, 1, 8), "...\n"
                   )
                   session$userData$last_keepalive <- Sys.time()
                 }
               },
               ignoreInit = TRUE,
               ignoreNULL = TRUE
  )
  
  # Session ended handler
  session$onSessionEnded(function() {
    cat("📤 Session ended for user:", substr(user_id, 1, 8), "...\n")
  })
  
  # Load data on startup
  baseball_data <- load_baseball_data_cached()
  
  # Initialize reactive values with safe defaults
  values <- reactiveValues(
    selected_player_info = NULL,
    analysis_mode = "default", 
    trends_plot = NULL,
    ai_analysis_result = NULL,
    ai_analysis_loading = FALSE,
    current_analysis_key = "",
    last_logged_key = "",
    stat_line_data = NULL  # ADD THIS LINE
  )
  
  # UI update trigger for forcing refreshes
  ui_update_trigger <- reactiveVal(0)
  
  # ============================================================================
  # INTERNAL UI GENERATION FUNCTIONS (moved inside server for proper scoping)
  # ============================================================================

  # Replace your entire generate_player_stat_line function with this clean version:

generate_player_stat_line <- function(player_id, baseball_data) {
  if (is.null(player_id) || player_id == "" || is.null(baseball_data)) {
    return(NULL)
  }
  
  player_info <- get_player_info(player_id, baseball_data)
  if (is.null(player_info)) return(NULL)
  
  # Extract actual player ID
  actual_player_id <- if ("compound_id" %in% colnames(baseball_data$lookup)) {
    extract_player_id(player_id)
  } else {
    player_id
  }
  
  if (player_info$type == "hitter" && nrow(baseball_data$hitters) > 0) {
    player_data <- baseball_data$hitters %>% filter(PlayerId == actual_player_id)
    
    if (nrow(player_data) > 0) {
      stats <- list(
        list(label = "AVG", value = format_stat_value(player_data$AVG_cur)),
        list(label = "OBP", value = format_stat_value(player_data$OBP_cur)),
        list(label = "SLG", value = format_stat_value(player_data$SLG_cur)),
        list(label = "K%", value = paste0(round(player_data$K_pct_cur, 1), "%")),
        list(label = "BB%", value = paste0(round(player_data$BB_pct_cur, 1), "%")),
        list(label = "wOBA", value = format_stat_value(player_data$wOBA_cur))
      )
      
      return(list(
        type = "hitter",
        pa = player_data$PA_cur,
        stats = stats
      ))
    }
  } else if (player_info$type == "pitcher" && nrow(baseball_data$pitchers) > 0) {
    player_data <- baseball_data$pitchers %>% filter(PlayerId == actual_player_id)
    
    if (nrow(player_data) > 0) {
      # Format pitcher stats
      babip_val <- sprintf("%.3f", player_data$babip_cur) %>% sub("^0\\.", ".", .)
      
      stats <- list(
        list(label = "ERA", value = sprintf("%.2f", player_data$era_cur)),
        list(label = "xERA", value = sprintf("%.2f", player_data$xera_cur)),
        list(label = "K%", value = paste0(round(player_data$k_percent_cur, 1), "%")),
        list(label = "BB%", value = paste0(round(player_data$bb_percent_cur, 1), "%")),
        list(label = "BABIP", value = babip_val),
        list(label = "LOB%", value = paste0(round(player_data$lob_percent_cur, 1), "%"))
      )
      
      return(list(
        type = "pitcher", 
        tbf = player_data$tbf,
        position = if ("position" %in% colnames(player_data)) player_data$position else "P",
        stats = stats
      ))
    }
  }
  
  return(NULL)
}
  
  # Generate Step 1: Player Selection UI (with trends plot)
  generate_step_1_ui <- function(player_selected = FALSE, player_info = NULL, trends_plot = NULL, 
                                 ai_loading = FALSE, ai_result = NULL, analysis_mode = "default", 
                                 stat_line_data = NULL) {
    step_class <- if (player_selected) "step-card active" else "step-card inactive"
    
    div(
      class = step_class,
      div(
        class = "step-header",
        div(class = if (player_selected) "step-number" else "step-number inactive", "1"),
        h3(class = if (player_selected) "step-title" else "step-title inactive", "Player Selected"),
        # AI Analysis status badge in header
        if (player_selected) {
          if (ai_loading) {
            span(
              class = "badge bg-primary ms-3",
              tags$i(class = "fas fa-spinner fa-spin me-1"),
              "Analyzing..."
            )
          } else if (!is.null(ai_result)) {
            span(
              class = "badge bg-success ms-3", 
              tags$i(class = "fas fa-check-circle me-1"),
              "Analysis Ready"
            )
          }
        }
      ),
      if (player_selected && !is.null(player_info)) {
        tagList(
          # INSTANT: Player card with photo
          div(
            class = "player-preview",
            img(
              src = player_info$photo_url %||% "https://via.placeholder.com/60x60/2E86AB/ffffff?text=⚾",
              alt = str_glue("Photo of {player_info$name}"),
              class = "player-preview-avatar",
              onerror = "this.src='https://via.placeholder.com/60x60/2E86AB/ffffff?text=⚾';"
            ),
            div(
              class = "player-preview-info",
              h4(player_info$name),
              p(str_glue("Age: {player_info$age %||% 'N/A'} • {player_info$position_info}"))
            )
          ),
          
          # NEW: Player stat line using pre-computed data
          if (!is.null(stat_line_data)) {
            div(
              class = "player-stat-line",
              div(
                class = "stat-line-header",
                h5(
                  class = "stat-line-title",
                  if (stat_line_data$type == "hitter") {
                    "2025 Hitting Stats"
                  } else {
                    "2025 Pitching Stats" 
                  }
                ),
                span(
                  class = "stat-line-context",
                  if (stat_line_data$type == "hitter") {
                    str_glue("{stat_line_data$pa} PA")
                  } else {
                    str_glue("{stat_line_data$tbf} TBF • {stat_line_data$position}")
                  }
                )
              ),
              div(
                class = "stats-grid",
                map(stat_line_data$stats, ~ {
                  div(
                    class = "stat-item",
                    div(class = "stat-label", .x$label),
                    div(class = "stat-value", .x$value)
                  )
                })
              )
            )
          },
          
          # INSTANT: Quick statistical insight
          div(
            class = "insight-summary",
            h5(icon("lightbulb"), "Summary Assessment"),
            p(player_info$quick_insight)
          ),
          
          # INSTANT: Performance trends plot
          if (!is.null(trends_plot)) {
            div(
              style = "margin-top: 1rem;",
              h5("Performance Trends", style = "color: #2E86AB; margin-bottom: 1rem;"),
              renderPlot(trends_plot, height = 300)
            )
          },
          
          # AI Analysis status section
          if (ai_loading) {
            div(
              class = "alert alert-info mt-3",
              div(
                class = "d-flex align-items-center",
                div(class = "spinner-border spinner-border-sm text-primary me-3", role = "status"),
                div(
                  tags$strong("Detailed AI analysis in progress..."),
                  tags$br(),
                  tags$small(
                    class = "text-muted",
                    str_glue("Generating {analysis_mode} analysis. Usually takes 5-15 seconds.")
                  )
                )
              )
            )
          } else if (!is.null(ai_result)) {
            div(
              class = "alert alert-success mt-3",
              div(
                class = "d-flex align-items-center justify-content-between",
                div(
                  class = "d-flex align-items-center",
                  tags$i(class = "fas fa-check-circle text-success me-2"),
                  div(
                    tags$strong("AI analysis complete!"),
                    tags$br(),
                    tags$small(class = "text-muted", "Scroll down to see detailed AI insights.")
                  )
                ),
                tags$button(
                  class = "btn btn-sm btn-outline-success",
                  onclick = "document.querySelector('.analysis-content').scrollIntoView({behavior: 'smooth', block: 'start'});",
                  tags$i(class = "fas fa-arrow-down me-1"),
                  "View AI Analysis"
                )
              )
            )
          }
        )
      } else {
        div(
          class = "empty-state",
          icon("search", class = "empty-icon"),
          h4(class = "empty-title", "Select a player above"),
          p(class = "empty-subtitle", "Choose from over 500 MLB players to get started")
        )
      }
    )
  }
  
  
  # Generate Step 2: Analysis Style UI - COMPACT VERSION
  generate_step_2_ui <- function(player_selected = FALSE, current_mode = "default") {
    step_class <- if (player_selected) "step-card active" else "step-card inactive"
    number_class <- if (player_selected) "step-number" else "step-number inactive"
    title_class <- if (player_selected) "step-title" else "step-title inactive"
    
    vibe_options <- list(
      list(mode = "default", icon = "📊", name = "Straightforward"),
      list(mode = "analytics_dork", icon = "🤓", name = "Analytics Dork"),
      list(mode = "old_coot", icon = "👴", name = "Old Coot"),
      list(mode = "gen_z", icon = "🔥", name = "Gen Z"),
      list(mode = "seventies", icon = "🥸", name = "1970s Fan"),
      list(mode = "sensationalist", icon = "📰", name = "Sensationalist"),
      list(mode = "shakespeare", icon = "🎭", name = "Shakespeare"),
      list(mode = "rose_colored_glasses", icon = "🌹", name = "Rose-colored")
    )
    
    div(
      class = step_class,
      div(
        class = "step-header",
        div(class = number_class, "2"),
        h3(class = title_class, "Choose Analysis Vibe")
      ),
      if (player_selected) {
        tagList(
          # Desktop/Tablet: Compact 2x4 grid
          div(
            class = "vibe-selector d-none d-md-flex",
            div(
              class = "vibe-row",
              map(vibe_options[1:4], ~ {
                card_class <- if (.x$mode == current_mode) "vibe-card-compact selected" else "vibe-card-compact"
                div(
                  class = card_class,
                  `data-mode` = .x$mode,
                  onclick = str_glue("Shiny.setInputValue('analysis_mode', '{.x$mode}', {{priority: 'event'}});"),
                  div(class = "vibe-icon-compact", .x$icon),
                  div(class = "vibe-name-compact", .x$name)
                )
              })
            ),
            div(
              class = "vibe-row",
              map(vibe_options[5:8], ~ {
                card_class <- if (.x$mode == current_mode) "vibe-card-compact selected" else "vibe-card-compact"
                div(
                  class = card_class,
                  `data-mode` = .x$mode,
                  onclick = str_glue("Shiny.setInputValue('analysis_mode', '{.x$mode}', {{priority: 'event'}});"),
                  div(class = "vibe-icon-compact", .x$icon),
                  div(class = "vibe-name-compact", .x$name)
                )
              })
            )
          ),
          
          # Mobile Option A: Compact vertical stack
          div(
            class = "vibe-selector-mobile d-md-none",
            map(vibe_options, ~ {
              option_class <- if (.x$mode == current_mode) "vibe-option-mobile selected" else "vibe-option-mobile"
              div(
                class = option_class,
                `data-mode` = .x$mode,
                onclick = str_glue("Shiny.setInputValue('analysis_mode', '{.x$mode}', {{priority: 'event'}});"),
                span(class = "vibe-option-icon", .x$icon),
                div(
                  class = "vibe-option-text",
                  div(class = "vibe-option-name", .x$name),
                  div(class = "vibe-option-desc", 
                      case_when(
                        .x$mode == "default" ~ "Clear, data-driven analysis",
                        .x$mode == "analytics_dork" ~ "Modern stats, dismissive vibes",
                        .x$mode == "old_coot" ~ "Grumpy old-school wisdom",
                        .x$mode == "gen_z" ~ "Modern slang and trends",
                        .x$mode == "seventies" ~ "Retro baseball perspective",
                        .x$mode == "sensationalist" ~ "Dramatic sports journalism",
                        .x$mode == "shakespeare" ~ "Iambic pentameter analysis",
                        .x$mode == "rose_colored_glasses" ~ "Always positive",
                        TRUE ~ ""
                      )
                  )
                )
              )
            })
          )
          
      
        )
      } else {
        div(
          class = "empty-state",
          icon("palette", class = "empty-icon"),
          h4(class = "empty-title", "Analysis styles will appear here"),
          p(class = "empty-subtitle", "First select a player to continue")
        )
      }
    )
  }
  
  # Generate Step 3: Analysis Results UI (without trends plot)
  generate_step_3_ui <- function(player_selected = FALSE, analysis_mode = NULL,
                                 ai_loading = FALSE, ai_result = NULL) {
    both_selected <- player_selected && !is.null(analysis_mode)
    
    div(
      class = if (both_selected) "step-card active" else "step-card inactive",
      div(
        class = "step-header",
        div(class = if (both_selected) "step-number" else "step-number inactive", "3"),
        h3(class = if (both_selected) "step-title" else "step-title inactive", "Detailed Analysis")
      ),
      if (both_selected) {
        # DYNAMIC: AI Analysis section only (trends plot now in Step 1)
        if (!is.null(ai_result)) {
          # COMPLETE: Show AI analysis
          tagList(
            div(class = "analysis-content", ai_result)
          )
        } else if (ai_loading) {
          # LOADING: Show progress with context
          tagList(
            div(
              class = "loading-state",
              div(
                class = "d-flex align-items-center",
                div(class = "spinner-border text-primary me-3", role = "status"),
                div(
                  h5("Analyzing with AI..."),
                  p(
                    class = "text-muted mb-0",
                    str_glue("Generating {analysis_mode} analysis. This typically takes 5-15 seconds.")
                  )
                )
              )
            )
          )
        } else {
          # READY: Analysis will start automatically
          tagList(
            div(
              class = "empty-state",
              icon("robot", class = "empty-icon"),
              h4(class = "empty-title", "Analyzing player..."),
              p(class = "empty-subtitle", "AI analysis will be available shortly")
            )
          )
        }
      } else {
        div(
          class = "empty-state",
          icon("robot", class = "empty-icon"),
          h4(class = "empty-title", "AI analysis will appear here"),
          p(class = "empty-subtitle", "Complete the steps above to get started")
        )
      }
    )
  }
  
  # ============================================================================
  # HELPER FUNCTIONS (moved inside server for proper scoping)
  # ============================================================================
  
  # Generate quick insight based on player data
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
  assess_regression_likelihood <- function(player_data, player_type) {
    if (is.null(player_data) || nrow(player_data) == 0) {
      return("low")
    }
    
    luck_indicators <- c()
    
    if (player_type == "hitter") {
      # BABIP analysis - Proper NA handling
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
      
      # xwOBA-wOBA gap analysis - Proper NA handling
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
      
      # Barrel rate vs BABIP inconsistency - Proper NA handling
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
      # BABIP analysis - Proper NA handling
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
      
      # ERA vs xERA gap - Proper NA handling
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
      
      # LOB% analysis - Proper NA handling
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
      
      # Barrel rate vs ERA inconsistency - Proper NA handling
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
  
  # ============================================================================
  # DATA AND REACTIVE LOGIC
  # ============================================================================
  
  # Update player choices using tidyverse
  observe({
    if (nrow(baseball_data$lookup) > 0) {
      player_choices <- if ("compound_id" %in% colnames(baseball_data$lookup)) {
        setNames(baseball_data$lookup$compound_id, baseball_data$lookup$display_name)
      } else {
        setNames(baseball_data$lookup$PlayerId, baseball_data$lookup$display_name)
      }
      
      updateSelectInput(session, "player_selection",
                        choices = c("Select a player..." = "", player_choices)
      )
    } else {
      updateSelectInput(session, "player_selection",
                        choices = c("⚠️ Data not loaded - check logs" = "")
      )
    }
  })
  
  # IMMEDIATE: React to player selection - UPDATE UI INSTANTLY
  observeEvent(input$player_selection,
               {
                 values$stat_line_data <- generate_player_stat_line(input$player_selection, baseball_data)
                 
                 
                 if (!is.null(input$player_selection) && input$player_selection != "") {
                   player_info <- get_player_info(input$player_selection, baseball_data)
                   
                   if (!is.null(player_info)) {
                     # Get player data for quick insight
                     actual_player_id <- if ("compound_id" %in% colnames(baseball_data$lookup)) {
                       extract_player_id(input$player_selection)
                     } else {
                       input$player_selection
                     }
                     
                     if (player_info$type == "hitter" && nrow(baseball_data$hitters) > 0) {
                       player_data <- baseball_data$hitters %>% filter(PlayerId == actual_player_id)
                     } else if (player_info$type == "pitcher" && nrow(baseball_data$pitchers) > 0) {
                       player_data <- baseball_data$pitchers %>% filter(PlayerId == actual_player_id)
                     } else {
                       player_data <- NULL
                     }
                     
                     # Generate quick insight safely
                     quick_insight <- if (!is.null(player_data) && nrow(player_data) > 0) {
                       generate_quick_insight(player_data, player_info$type)
                     } else {
                       "Player data available for analysis."
                     }
                     
                     # INSTANT UPDATE: Store player info + quick insight
                     values$selected_player_info <- list(
                       name = player_info$name,
                       type = player_info$type,
                       age = player_info$age,
                       position_info = player_info$position_info,
                       photo_url = get_player_photo_url(input$player_selection, baseball_data),
                       quick_insight = quick_insight
                     )
                     
                     # INSTANT: Generate and store trends plot (fast, no API needed)
                     values$trends_plot <- create_player_trends_plot(input$player_selection, baseball_data)
                     
                     # Clear AI analysis state
                     values$ai_analysis_result <- NULL
                     values$ai_analysis_loading <- FALSE
                     
                     # Force UI update immediately
                     ui_update_trigger(ui_update_trigger() + 1)
                     
                     cat("✅ INSTANT: Player info loaded for:", player_info$name, "\n")
                     
                     # IMMEDIATE LOGGING AND AI TRIGGER - Use default mode if none selected
                     current_mode <- if (is.null(values$analysis_mode) || values$analysis_mode == "") {
                       "default"
                     } else {
                       values$analysis_mode
                     }
                     
                     analysis_key <- paste(player_info$name, current_mode, sep = "_")
                     if (analysis_key != values$last_logged_key) {
                       log_if_not_admin(session, player_info$name, current_mode)
                       values$last_logged_key <- analysis_key
                       cat("📊 IMMEDIATE LOG: Player selected, triggering analysis with mode:", current_mode, "\n")
                     }
                   }
                 } else {
                   values$selected_player_info <- NULL
                   values$trends_plot <- NULL
                   values$ai_analysis_result <- NULL
                   values$ai_analysis_loading <- FALSE
                   ui_update_trigger(ui_update_trigger() + 1)
                   cat("🗑️ Player selection cleared\n")
                 }
               },
               ignoreInit = TRUE
  )
  
  # IMMEDIATE: React to analysis mode selection
  observeEvent(input$analysis_mode,
               {
                 if (!is.null(input$analysis_mode)) {
                   cat("🎨 Analysis mode changed to:", input$analysis_mode, "\n")
                   values$analysis_mode <- input$analysis_mode
                   
                   # Clear previous AI analysis when mode changes
                   values$ai_analysis_result <- NULL
                   values$ai_analysis_loading <- FALSE
                   
                   # Force UI update immediately
                   ui_update_trigger(ui_update_trigger() + 1)
                   
                   # IMMEDIATE LOGGING: If player is already selected, log now
                   if (!is.null(values$selected_player_info)) {
                     analysis_key <- paste(values$selected_player_info$name, input$analysis_mode, sep = "_")
                     if (analysis_key != values$last_logged_key) {
                       log_if_not_admin(session, values$selected_player_info$name, input$analysis_mode)
                       values$last_logged_key <- analysis_key
                       cat("📊 IMMEDIATE LOG: Vibe changed with existing player\n")
                     }
                   }
                 }
               },
               ignoreInit = TRUE
  )
  
  # SEPARATE ASYNC OBSERVER - AI Analysis Generation
  observeEvent(
    {
      list(values$selected_player_info, values$analysis_mode)
    },
    {
      # Use isolate() to prevent this from blocking other reactive updates
      selected_info <- isolate(values$selected_player_info)
      analysis_mode <- isolate(values$analysis_mode)
      player_selection <- isolate(input$player_selection)
      
      # Allow default mode to trigger AI analysis
      if (!is.null(selected_info) &&
          !is.null(analysis_mode) &&
          !is.null(player_selection) &&
          player_selection != "") {
        analysis_key <- paste(selected_info$name, analysis_mode, sep = "_")
        current_key <- isolate(values$current_analysis_key)
        
        if (analysis_key != current_key) {
          cat("🎯 ASYNC: Starting AI analysis for:", analysis_key, "\n")
          
          # Set loading state immediately
          values$ai_analysis_loading <- TRUE
          values$ai_analysis_result <- NULL
          values$current_analysis_key <- analysis_key
          
          # Generate AI analysis asynchronously
          later::later(function() {
            tryCatch(
              {
                cat("🤖 Generating AI analysis...\n")
                
                analysis_result <- analyze_player_performance(
                  player_selection,
                  analysis_mode,
                  baseball_data
                )
                
                # Update when complete
                values$ai_analysis_result <- analysis_result
                values$ai_analysis_loading <- FALSE
                
                cat("✅ ASYNC: AI analysis complete for:", analysis_key, "\n")
              },
              error = function(e) {
                cat("❌ ASYNC: Error in AI analysis:", e$message, "\n")
                values$ai_analysis_loading <- FALSE
                values$ai_analysis_result <- HTML(paste0(
                  "<div class='alert alert-danger'>",
                  "Error generating analysis: ", e$message,
                  "</div>"
                ))
              }
            )
          }, delay = 0.1)
        } else {
          cat("♻️ ASYNC: Using existing analysis for:", analysis_key, "\n")
        }
      }
    },
    ignoreInit = TRUE
  )
  
  # ============================================================================
  # UI OUTPUTS USING INTERNAL FUNCTIONS
  # ============================================================================
  
  # Render Step 1: Player Selection (using internal function)

  # DEBUGGING STEPS:
  
  # 1. First, let's check what's being passed to the generate_step_1_ui function
  # Update your output$step_1_player_selection in server.R:
  
  output$step_1_player_selection <- renderUI({
    ui_update_trigger()  # Access reactive trigger
    
    player_selected <- !is.null(values$selected_player_info)
    
    generate_step_1_ui(
      player_selected = player_selected,
      player_info = values$selected_player_info,
      trends_plot = values$trends_plot,
      ai_loading = isTRUE(values$ai_analysis_loading),
      ai_result = values$ai_analysis_result,
      analysis_mode = values$analysis_mode %||% "default",
      stat_line_data = values$stat_line_data  # Make sure this line exists
    )
  })
  
  # 2. Let's also add debugging to the stat line function itself:
  generate_player_stat_line <- function(player_id, baseball_data) {
    cat("🔍 DEBUG: generate_player_stat_line called with:\n")
    cat("  - player_id:", player_id %||% "NULL", "\n")
    cat("  - baseball_data is null:", is.null(baseball_data), "\n")
    
    if (is.null(player_id) || player_id == "" || is.null(baseball_data)) {
      cat("❌ DEBUG: Early return - missing player_id or baseball_data\n")
      return(NULL)
    }
    
    player_info <- get_player_info(player_id, baseball_data)
    cat("  - player_info is null:", is.null(player_info), "\n")
    
    if (is.null(player_info)) {
      cat("❌ DEBUG: Early return - no player_info\n")
      return(NULL)
    }
    
    cat("  - player_type:", player_info$type, "\n")
    
    # Extract actual player ID
    actual_player_id <- if ("compound_id" %in% colnames(baseball_data$lookup)) {
      extract_player_id(player_id)
    } else {
      player_id
    }
    
    cat("  - actual_player_id:", actual_player_id, "\n")
    
    if (player_info$type == "hitter" && nrow(baseball_data$hitters) > 0) {
      player_data <- baseball_data$hitters %>% filter(PlayerId == actual_player_id)
      cat("  - hitter data rows found:", nrow(player_data), "\n")
      
      if (nrow(player_data) > 0) {
        cat("✅ DEBUG: Generating hitter stats\n")
        # Clean current stats display
        stats <- list(
          list(label = "AVG", value = format_stat_value(player_data$AVG_cur)),
          list(label = "OBP", value = format_stat_value(player_data$OBP_cur)),
          list(label = "SLG", value = format_stat_value(player_data$SLG_cur)),
          list(label = "K%", value = paste0(format_stat_value(player_data$K_pct_cur), "%")),
          list(label = "BB%", value = paste0(format_stat_value(player_data$BB_pct_cur), "%")),
          list(label = "wOBA", value = format_stat_value(player_data$wOBA_cur))
        )
        
        return(list(
          type = "hitter",
          pa = player_data$PA_cur,
          stats = stats
        ))
      }
    } else if (player_info$type == "pitcher" && nrow(baseball_data$pitchers) > 0) {
      player_data <- baseball_data$pitchers %>% filter(PlayerId == actual_player_id)
      cat("  - pitcher data rows found:", nrow(player_data), "\n")
      
      if (nrow(player_data) > 0) {
        cat("✅ DEBUG: Generating pitcher stats\n")
        # Clean current stats display
        stats <- list(
          list(label = "ERA", value = format_stat_value(player_data$era_cur)),
          list(label = "xERA", value = format_stat_value(player_data$xera_cur)),
          list(label = "K%", value = paste0(format_stat_value(player_data$k_percent_cur), "%")),
          list(label = "BB%", value = paste0(format_stat_value(player_data$bb_percent_cur), "%")),
          list(label = "BABIP", value = format_stat_value(player_data$babip_cur)),
          list(label = "LOB%", value = paste0(format_stat_value(player_data$lob_percent_cur), "%"))
        )
        
        return(list(
          type = "pitcher", 
          tbf = player_data$tbf,
          position = if ("position" %in% colnames(player_data)) player_data$position else "P",
          stats = stats
        ))
      }
    }
    
    cat("❌ DEBUG: No data found for player\n")
    return(NULL)
  }
  
  # 3. Also update the UI generation to show when it's trying to call the stat line:
  # In your generate_step_1_ui function, replace the stat line section with:
  
  
  
  # Render Step 2: Analysis Style (using internal function)
  output$step_2_analysis_style <- renderUI({
    ui_update_trigger()
    
    player_selected <- !is.null(values$selected_player_info)
    
    generate_step_2_ui(
      player_selected = player_selected, 
      current_mode = values$analysis_mode %||% "default"
    )
  })
  
  # Render Step 3: Analysis Results (using internal function)
  output$step_3_analysis_results <- renderUI({
    ui_update_trigger()
    
    player_selected <- !is.null(values$selected_player_info)
    
    generate_step_3_ui(
      player_selected = player_selected,
      analysis_mode = values$analysis_mode,
      ai_loading = isTRUE(values$ai_analysis_loading),
      ai_result = values$ai_analysis_result
    )
  })
  
  # Force UI outputs to not suspend when hidden
  outputOptions(output, "step_1_player_selection", suspendWhenHidden = FALSE)
  outputOptions(output, "step_2_analysis_style", suspendWhenHidden = FALSE)
  outputOptions(output, "step_3_analysis_results", suspendWhenHidden = FALSE)
}
