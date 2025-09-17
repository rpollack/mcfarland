# Server
# Complete Server Logic with Internal UI Functions and Trends in Step 1
server <- function(input, output, session) {
  session$allowReconnect(TRUE)

  # Generate user ID on session start
  user_id <- generate_user_id(session)
  log_session_if_not_admin(session)

  # Visibility-aware keep-alive ping from JS
  observeEvent(input$heartbeat, {}, ignoreNULL = TRUE)

  # Session ended handler
  session$onSessionEnded(function() {
    cat("📤 Session ended for user:", substr(user_id, 1, 8), "...\n")
  })
  
  # Load data on startup
  baseball_data <- load_baseball_data_cached()

  # Parse initial shareable URL parameters for deep linking
  initial_query <- parseQueryString(isolate(session$clientData$url_search))
  initial_player <- initial_query$player
  initial_vibe <- initial_query$vibe
  initial_view <- initial_query$view %||% if (!is.null(initial_query$players)) "compare" else "single"
  initial_compare_players <- if (!is.null(initial_query$players)) {
    players <- trimws(strsplit(initial_query$players, ",")[[1]])
    players[nzchar(players)]
  } else {
    NULL
  }
  initial_compare_type <- initial_query$type
  initial_single_mode <- if (!is.null(initial_vibe) && !identical(initial_view, "compare")) {
    initial_vibe
  } else {
    "default"
  }
  initial_compare_mode <- if (!is.null(initial_vibe) && identical(initial_view, "compare")) {
    initial_vibe
  } else {
    "default"
  }

  # Initialize admin mode if admin password is provided
  is_admin(session)

  # Initialize reactive values with safe defaults
  values <- reactiveValues(
    selected_player_info = NULL,
    single_analysis_mode = initial_single_mode,
    compare_analysis_mode = initial_compare_mode,
    analysis_view = initial_view,
    initial_single_mode_from_query = if (!is.null(initial_vibe) && !identical(initial_view, "compare")) initial_vibe else NULL,
    trends_plot = NULL,
    ai_analysis_result = NULL,
    ai_analysis_loading = FALSE,
    current_analysis_key = "",
    current_compare_key = "",
    last_logged_key = "",
    stat_line_data = NULL,  # current stat line
    pending_share_run = !is.null(initial_player),
    pending_compare_run = !is.null(initial_compare_players) && length(initial_compare_players) > 0,
    pending_compare_expected = if (!is.null(initial_compare_players)) length(initial_compare_players) else 0,
    compare_results = NULL,
    compare_ai_result = NULL,
    compare_ai_loading = FALSE,
    initial_compare_players = initial_compare_players,
    initial_compare_type = initial_compare_type,
    last_query_string = NULL
  )
  
  # Populate player selector on startup
  observe({
    lookup <- baseball_data$lookup
    if ("compound_id" %in% colnames(lookup)) {
      ids <- lookup$compound_id
      names <- lookup$display_name
    } else {
      ids <- lookup$PlayerId
      names <- lookup$display_name
    }
    updateSelectizeInput(
      session,
      "player_selection",
      choices = setNames(ids, names),
      selected = initial_player %||% "",
      server = TRUE
    )
  })

  observeEvent(TRUE, {
    updateRadioButtons(
      session,
      "analysis_view",
      selected = values$analysis_view %||% "single"
    )
  }, once = TRUE)

  observeEvent(TRUE, {
    default_type <- values$initial_compare_type %||% "hitter"
    updateRadioButtons(session, "compare_type", selected = default_type)
  }, once = TRUE)

  observeEvent(input$analysis_view, {
    if (!is.null(input$analysis_view)) {
      values$analysis_view <- input$analysis_view
      new_mode <- if (identical(input$analysis_view, "compare")) {
        values$compare_analysis_mode %||% "default"
      } else {
        values$single_analysis_mode %||% "default"
      }
      if (!is.null(input$analysis_mode)) {
        updateSelectInput(session, "analysis_mode", selected = new_mode)
      }
    }
  }, ignoreNULL = TRUE)

  # Populate compare selectors based on player type
  observeEvent(input$compare_type, {
    req(input$compare_type)
    lookup <- baseball_data$lookup %>% dplyr::filter(player_type == input$compare_type)
    ids <- if ("compound_id" %in% colnames(lookup)) lookup$compound_id else lookup$PlayerId
    choices <- setNames(ids, lookup$display_name)

    initial_players <- values$initial_compare_players
    if (!is.null(initial_players) && length(initial_players) > 0) {
      player_vals <- c(initial_players, rep("", 3))[1:3]
      updateSelectizeInput(session, "compare_player1", choices = choices, selected = player_vals[[1]], server = TRUE)
      updateSelectizeInput(session, "compare_player2", choices = choices, selected = player_vals[[2]], server = TRUE)
      updateSelectizeInput(session, "compare_player3", choices = choices, selected = player_vals[[3]], server = TRUE)
      values$initial_compare_players <- NULL
    } else {
      updateSelectizeInput(session, "compare_player1", choices = choices, selected = "", server = TRUE)
      updateSelectizeInput(session, "compare_player2", choices = choices, selected = "", server = TRUE)
      updateSelectizeInput(session, "compare_player3", choices = choices, selected = "", server = TRUE)
    }
  }, ignoreNULL = FALSE)

  run_compare_analysis <- function() {
    ids <- c(input$compare_player1, input$compare_player2, input$compare_player3)
    ids <- ids[ids != ""]

    if (length(ids) == 0) {
      values$compare_results <- NULL
      values$compare_ai_loading <- FALSE
      values$compare_ai_result <- HTML("<div class='alert alert-warning'>Select players to analyze.</div>")
      values$current_compare_key <- ""
      return(NULL)
    }

    players <- purrr::map(ids, function(id) {
      list(
        info = get_player_info(id, baseball_data),
        stats = generate_player_stat_line(id, baseball_data),
        photo = get_player_photo_url(id, baseball_data)
      )
    })

    player_names <- purrr::map_chr(players, function(p) p$info$name)
    analysis_mode <- values$compare_analysis_mode %||% "default"
    compare_type <- input$compare_type %||% "hitter"
    compare_key <- paste(c(compare_type, sort(ids), analysis_mode), collapse = "_")
    log_analysis_if_not_admin(session, paste(player_names, collapse = " vs "), analysis_mode)

    rec_id <- recommend_best_player(ids, baseball_data)
    rec_name <- if (!is.null(rec_id)) get_player_info(rec_id, baseball_data)$name else NULL

    player_cards <- purrr::map(players, function(p) {
      stat_rows <- purrr::map(p$stats$stats, function(s) tags$tr(tags$th(s$label), tags$td(s$value)))
      tags$div(
        class = "card mb-3 text-center",
        tags$div(
          class = "card-body",
          img(src = p$photo, class = "compare-photo mb-2"),
          tags$h5(class = "card-title", p$info$name),
          tags$table(class = "table table-sm mb-0", stat_rows)
        )
      )
    })

    values$compare_results <- tagList(
      fluidRow(purrr::map(player_cards, function(card) column(4, card))),
      if (!is.null(rec_name))
        div(class = "alert alert-info mt-3", HTML(paste0("<strong>Recommendation:</strong> ", rec_name, " has the edge going forward.")))
    )

    values$compare_ai_loading <- TRUE
    values$compare_ai_result <- NULL
    values$current_compare_key <- compare_key

    player_ids <- ids
    analysis_mode_local <- analysis_mode
    compare_key_local <- compare_key

    later::later(function() {
      result <- tryCatch(
        analyze_player_comparison(player_ids, baseball_data, analysis_mode_local),
        error = function(e) htmltools::HTML(paste0("<div class='alert alert-danger'>Error: ", e$message, "</div>"))
      )

      shiny::withReactiveDomain(session, {
        current_key <- shiny::isolate(values$current_compare_key)
        if (!identical(current_key, compare_key_local)) {
          cat("⏭️ Ignoring stale comparison analysis for:", compare_key_local, "\n")
          return()
        }

        values$compare_ai_result <- div(class = "analysis-content", result)
        values$compare_ai_loading <- FALSE
      })
    }, delay = 0.1)
  }

  observeEvent(input$compare_analyze, {
    run_compare_analysis()
  })

  observe({
    if (isTRUE(values$pending_compare_run)) {
      players <- c(input$compare_player1, input$compare_player2, input$compare_player3)
      players <- players[players != ""]
      expected <- values$pending_compare_expected %||% 0
      if (length(players) >= max(1, expected)) {
        values$pending_compare_run <- FALSE
        values$pending_compare_expected <- 0
        run_compare_analysis()
      }
    }
  })

  # ============================================================================
  # INTERNAL UI GENERATION FUNCTIONS (moved inside server for proper scoping)
  # ============================================================================

  # Generate stat line for selected player

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
        list(label = "K%", value = format_percentage(player_data$K_pct_cur)),
        list(label = "BB%", value = format_percentage(player_data$BB_pct_cur)),
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
        list(label = "ERA", value = format_era(player_data$era_cur)),
        list(label = "xERA", value = format_era(player_data$xera_cur)),
        list(label = "K%", value = format_percentage(player_data$k_percent_cur)),
        list(label = "BB%", value = format_percentage(player_data$bb_percent_cur)),
        list(label = "BABIP", value = babip_val),
        list(label = "LOB%", value = format_percentage(player_data$lob_percent_cur))
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
  
  generate_player_preview <- function(player_info = NULL, ai_loading = FALSE,
                                      ai_result = NULL, analysis_mode = "default",
                                      stat_line_data = NULL) {
    if (!is.null(player_info)) {
      tagList(
        div(
          class = "player-preview-grid",
          img(
            src = player_info$photo_url %||% "https://via.placeholder.com/60x60/2E86AB/ffffff?text=⚾",
            alt = str_glue("Photo of {player_info$name}"),
            class = "player-preview-avatar",
            onerror = "this.src='https://via.placeholder.com/60x60/2E86AB/ffffff?text=⚾';"
          ),
          div(
            class = "player-preview-info",
            h4(player_info$name),
            p(
              str_glue(
                "Age: {player_info$age %||% 'N/A'} • {if (player_info$type == 'pitcher') 'TBF' else 'PA'}: {(if (player_info$type == 'pitcher') player_info$tbf else player_info$pa) %||% 'N/A'}"
              )
            )
          ),
          if (!is.null(stat_line_data)) {
            map(stat_line_data$stats, ~ {
              div(
                class = "stat-item",
                div(class = "stat-label", .x$label),
                div(class = "stat-value", .x$value)
              )
            })
          }
        ),
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
      NULL
    }
  }
  
  
  # Generate Step 2: Analysis Style UI - COMPACT VERSION
  generate_step_2_ui <- function(player_selected = FALSE, current_mode = "default") {
    step_class <- if (player_selected) "step-card active" else "step-card inactive"
    number_class <- if (player_selected) "step-number" else "step-number inactive"
    title_class <- if (player_selected) "step-title" else "step-title inactive"
    
    vibe_options <- list(
      list(mode = "seventies", icon = "🥸", name = "1970s Fan"),
      list(mode = "analytics_dork", icon = "🤓", name = "Analytics Dork"),
      list(mode = "gen_z", icon = "🔥", name = "Gen Z"),
      list(mode = "old_coot", icon = "👴", name = "Old Coot"),
      list(mode = "rose_colored_glasses", icon = "🌹", name = "Rose-colored"),
      list(mode = "rotisserie_expert", icon = "🧠", name = "Rotisserie Expert"),
      list(mode = "sensationalist", icon = "📰", name = "Sensationalist"),
      list(mode = "shakespeare", icon = "🎭", name = "Shakespeare"),
      list(mode = "default", icon = "📊", name = "Straightforward (default)")
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
          # Desktop/Tablet: Compact grid in rows of up to 4
          div(
            class = "vibe-selector d-none d-md-flex",
            {
              vibe_rows <- split(vibe_options, ceiling(seq_along(vibe_options) / 4))
              purrr::map(vibe_rows, ~ {
                div(
                  class = "vibe-row",
                  map(.x, ~ {
                    card_class <- if (.x$mode == current_mode) "vibe-card-compact selected" else "vibe-card-compact"
                    div(
                      class = card_class,
                      `data-mode` = .x$mode,
                      onclick = sprintf("Shiny.setInputValue('analysis_mode', '%s', {priority: 'event'});", .x$mode),
                      div(class = "vibe-icon-compact", .x$icon),
                      div(class = "vibe-name-compact", .x$name)
                    )
                  })
                )
              })
            }
          ),
          
          # Mobile: Dropdown selector
          div(
            class = "vibe-dropdown-mobile d-md-none",
            selectInput(
              "analysis_mode",
              label = NULL,
              choices = setNames(
                purrr::map_chr(vibe_options, "mode"),
                purrr::map_chr(vibe_options, ~ paste0(.x$icon, " ", .x$name))
              ),
              selected = current_mode,
              selectize = FALSE,
              width = "100%"
            )
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
  
  # Generate Step 3: Analysis Results UI (AI analysis with trends summary)
  generate_step_3_ui <- function(player_selected = FALSE, analysis_mode = NULL,
                                 ai_loading = FALSE, ai_result = NULL,
                                 trends_plot = NULL) {
    both_selected <- player_selected && !is.null(analysis_mode)
    
    div(
      class = if (both_selected) "step-card active" else "step-card inactive",
      div(
        class = "step-header",
        div(class = if (both_selected) "step-number" else "step-number inactive", "3"),
        h3(class = if (both_selected) "step-title" else "step-title inactive", "Detailed Analysis")
      ),
      if (both_selected) {
        # DYNAMIC: AI Analysis followed by trends plot
        if (!is.null(ai_result)) {
          # COMPLETE: Show AI analysis with trends and share button
          tagList(
            div(class = "analysis-content", ai_result),
            if (!is.null(trends_plot)) {
              div(
                style = "margin-top: 1rem;",
                h5("Performance Trends", style = "color: #2E86AB; margin-bottom: 1rem;"),
                renderPlot(trends_plot, height = 300)
              )
            },
            div(
              style = "margin-top: 1rem;",
              actionButton("share_x", label = "Share on X", icon = icon("share"), class = "btn-primary")
            )
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

  generate_compare_step_ui <- function(players_selected = 0,
                                       compare_results = NULL,
                                       ai_loading = FALSE,
                                       ai_result = NULL,
                                       analysis_mode = "default") {
    has_players <- players_selected > 0
    div(
      class = if (has_players) "step-card active" else "step-card inactive",
      div(
        class = "step-header",
        div(class = if (has_players) "step-number" else "step-number inactive", "3"),
        h3(class = if (has_players) "step-title" else "step-title inactive", "Compare Players")
      ),
      if (has_players) {
        tagList(
          actionButton(
            "compare_analyze",
            "Analyze",
            icon = icon("robot"),
            class = "btn-primary mb-3",
            onclick = "document.getElementById('compare-results').scrollIntoView({behavior: 'smooth', block: 'start'});"
          ),
          div(
            id = "compare-results",
            if (!is.null(compare_results)) {
              compare_results
            } else {
              div(
                class = "empty-state",
                icon("users", class = "empty-icon"),
                h4(class = "empty-title", "Ready to compare"),
                p(class = "empty-subtitle", "Click Analyze to build the side-by-side breakdown.")
              )
            },
            if (isTRUE(ai_loading)) {
              div(
                class = "loading-state mt-3",
                div(
                  class = "d-flex align-items-center",
                  div(class = "spinner-border text-primary me-3", role = "status"),
                  div(
                    h5("Analyzing with AI..."),
                    p(
                      class = "text-muted mb-0",
                      stringr::str_glue(
                        "Generating {analysis_mode} analysis. This typically takes 5-15 seconds."
                      )
                    )
                  )
                )
              )
            } else if (!is.null(ai_result)) {
              ai_result
            }
          )
        )
      } else {
        div(
          class = "empty-state",
          icon("users", class = "empty-icon"),
          h4(class = "empty-title", "Add players to compare"),
          p(class = "empty-subtitle", "Pick at least one player above to unlock comparison analysis.")
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
  
  # IMMEDIATE: React to player selection - UPDATE UI INSTANTLY
  observeEvent(
    input$player_selection,
    {
      if (!isTruthy(input$player_selection)) {
        values$selected_player_info <- NULL
        values$trends_plot <- NULL
        values$ai_analysis_result <- NULL
        values$ai_analysis_loading <- FALSE
        cat("🗑️ Player selection cleared\n")
        return()
      }

      values$stat_line_data <- generate_player_stat_line(input$player_selection, baseball_data)

      # Default to standard vibe unless preset via shareable link
      if (!is.null(values$initial_single_mode_from_query)) {
        values$single_analysis_mode <- values$initial_single_mode_from_query
        values$initial_single_mode_from_query <- NULL
      } else {
        values$single_analysis_mode <- "default"
      }

      current_view <- input$analysis_view %||% values$analysis_view %||% "single"
      if (!identical(current_view, "compare") && !is.null(input$analysis_mode)) {
        updateSelectInput(session, "analysis_mode", selected = values$single_analysis_mode)
      }
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
          tbf = player_info$tbf,
          pa = player_info$pa,
          photo_url = get_player_photo_url(input$player_selection, baseball_data),
          quick_insight = quick_insight
        )

        # INSTANT: Generate and store trends plot (fast, no API needed)
        values$trends_plot <- create_player_trends_plot(input$player_selection, baseball_data)

        # Clear AI analysis state
        values$ai_analysis_result <- NULL
        values$ai_analysis_loading <- FALSE

        cat("✅ INSTANT: Player info loaded for:", player_info$name, "\n")

        # IMMEDIATE LOGGING AND AI TRIGGER - Use default mode if none selected
        current_mode <- if (is.null(values$single_analysis_mode) || values$single_analysis_mode == "") {
          "default"
        } else {
          values$single_analysis_mode
        }

        analysis_key <- paste(player_info$name, current_mode, sep = "_")
        if (analysis_key != values$last_logged_key) {
          log_analysis_if_not_admin(session, player_info$name, current_mode)
          if (isTRUE(values$pending_share_run)) {
            log_share_if_not_admin(session, player_info$name, current_mode, "shared_run")
            values$pending_share_run <- FALSE
          }
          values$last_logged_key <- analysis_key
          cat("📊 IMMEDIATE LOG: Player selected, triggering analysis with mode:", current_mode, "\n")
        }
      }
    },
    ignoreNULL = TRUE
  )
  
  # IMMEDIATE: React to analysis mode selection
  observeEvent(input$analysis_mode, {
    mode <- input$analysis_mode
    if (is.null(mode)) {
      return()
    }

    current_view <- input$analysis_view %||% values$analysis_view %||% "single"

    if (identical(current_view, "compare")) {
      if (identical(mode, values$compare_analysis_mode)) {
        return()
      }
      cat("🎨 Comparison analysis mode changed to:", mode, "\n")
      values$compare_analysis_mode <- mode
      updateSelectInput(session, "analysis_mode", selected = mode)

      compare_players <- c(input$compare_player1, input$compare_player2, input$compare_player3)
      selected_ids <- compare_players[compare_players != ""]
      if (length(selected_ids) > 0) {
        cat("🔁 Re-running comparison analysis for new vibe\n")
        values$pending_compare_run <- FALSE
        run_compare_analysis()
      }
      return()
    }

    if (identical(mode, values$single_analysis_mode)) {
      return()
    }

    cat("🎨 Analysis mode changed to:", mode, "\n")
    values$single_analysis_mode <- mode
    updateSelectInput(session, "analysis_mode", selected = mode)

    # Clear previous AI analysis when single-player mode changes
    values$ai_analysis_result <- NULL
    values$ai_analysis_loading <- FALSE

    # IMMEDIATE LOGGING: If player is already selected, log now
    if (!is.null(values$selected_player_info)) {
      analysis_key <- paste(values$selected_player_info$name, mode, sep = "_")
      if (analysis_key != values$last_logged_key) {
        log_analysis_if_not_admin(session, values$selected_player_info$name, mode)
        if (isTRUE(values$pending_share_run)) {
          log_share_if_not_admin(session, values$selected_player_info$name, mode, "shared_run")
          values$pending_share_run <- FALSE
        }
        values$last_logged_key <- analysis_key
        cat("📊 IMMEDIATE LOG: Vibe changed with existing player\n")
      }
    }
  }, ignoreInit = TRUE)
  
  # SEPARATE ASYNC OBSERVER - AI Analysis Generation
  observeEvent(
    {
      list(values$selected_player_info, values$single_analysis_mode)
    },
    {
      # Use isolate() to prevent this from blocking other reactive updates
      selected_info <- isolate(values$selected_player_info)
      analysis_mode <- isolate(values$single_analysis_mode)
      player_selection <- isolate(input$player_selection)
      
      # Allow default mode to trigger AI analysis
      if (!is.null(selected_info) &&
          !is.null(analysis_mode) &&
          !is.null(player_selection) &&
          nzchar(player_selection)) {
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

                # Update when complete within session's reactive domain
                shiny::withReactiveDomain(session, {
                  values$ai_analysis_result <- analysis_result
                  values$ai_analysis_loading <- FALSE
                })

                cat("✅ ASYNC: AI analysis complete for:", analysis_key, "\n")
              },
              error = function(e) {
                cat("❌ ASYNC: Error in AI analysis:", e$message, "\n")
                shiny::withReactiveDomain(session, {
                  values$ai_analysis_loading <- FALSE
                  values$ai_analysis_result <- HTML(paste0(
                    "<div class='alert alert-danger'>",
                    "Error generating analysis: ", e$message,
                    "</div>"
                  ))
                })
              }
            )
          }, delay = 0.1)

          # Timeout safeguard
          later::later(function() {
            shiny::withReactiveDomain(session, {
              shiny::isolate({
                if (isTRUE(values$ai_analysis_loading) &&
                    values$current_analysis_key == analysis_key) {
                  values$ai_analysis_loading <- FALSE
                  values$ai_analysis_result <- HTML(
                    "<div class='alert alert-danger'>Analysis timed out. Please try again.</div>"
                  )
                  cat("⏱️ ASYNC: Analysis timed out for:", analysis_key, "\n")
                }
              })
            })
          }, delay = 60)
        } else {
          cat("♻️ ASYNC: Using existing analysis for:", analysis_key, "\n")
        }
      }
    },
    ignoreInit = TRUE
  )

  build_query_values <- function(view_override = NULL) {
    view <- view_override %||% input$analysis_view %||% values$analysis_view %||% "single"
    params <- list(view = view)

    mode <- if (identical(view, "compare")) {
      values$compare_analysis_mode
    } else {
      values$single_analysis_mode
    }
    include_mode <- FALSE

    if (identical(view, "compare")) {
      type <- input$compare_type
      players <- c(input$compare_player1, input$compare_player2, input$compare_player3)
      players <- players[players != ""]
      if (!is.null(type) && nzchar(type)) params$type <- type
      if (length(players) > 0) {
        params$players <- paste(players, collapse = ",")
        include_mode <- TRUE
      }
    } else {
      player_id <- input$player_selection
      if (!is.null(player_id) && nzchar(player_id)) {
        params$player <- player_id
        include_mode <- TRUE
      }
    }

    if (include_mode && !is.null(mode) && nzchar(mode)) params$vibe <- mode
    if (is_admin(session)) params$admin <- Sys.getenv("ADMIN_PASSWORD", "")

    params <- params[!vapply(params, function(x) is.null(x) || identical(x, ""), logical(1))]
    if (identical(params, list(view = "single"))) {
      return(character())
    }
    if (length(params) == 0) {
      return(character())
    }

    encoded <- vapply(
      params,
      function(x) utils::URLencode(as.character(x), reserved = TRUE),
      character(1)
    )
    names(encoded) <- names(params)
    encoded
  }

  build_query_string <- function(view_override = NULL) {
    query_values <- build_query_values(view_override)
    if (length(query_values) == 0) {
      ""
    } else {
      paste(names(query_values), query_values, sep = "=", collapse = "&")
    }
  }

  observe({
    query <- build_query_string()
    if (!identical(query, values$last_query_string)) {
      new_query <- if (nzchar(query)) paste0("?", query) else ""
      updateQueryString(new_query, mode = "replace", session = session)
      values$last_query_string <- query
    }
  })

  # Share analysis on X (Twitter)
  observeEvent(input$share_x, {
    req(values$selected_player_info)
    req(input$player_selection)

    mode <- values$single_analysis_mode %||% "default"
    base_url <- paste0(
      session$clientData$url_protocol, "//",
      session$clientData$url_hostname,
      if (nzchar(session$clientData$url_port) && !session$clientData$url_port %in% c("80", "443"))
        paste0(":", session$clientData$url_port)
      else "",
      session$clientData$url_pathname
    )
    query <- build_query_string("single")
    share_url <- paste0(base_url, if (nzchar(query)) paste0("?", query) else "")
    insight <- values$selected_player_info$quick_insight %||% ""
    share_text <- str_glue("{values$selected_player_info$name}: {insight} via McFARLAND")
    share_text <- stringr::str_trunc(share_text, 200)

    session$sendCustomMessage('open-x-share', list(text = share_text, url = share_url))
    log_share_if_not_admin(session, values$selected_player_info$name, mode, "share_click")
  })
  
  # ============================================================================
  # UI OUTPUTS USING INTERNAL FUNCTIONS
  # ============================================================================
  
  # Render player preview section
  output$player_preview <- renderUI({
    generate_player_preview(
      player_info = values$selected_player_info,
      ai_loading = isTRUE(values$ai_analysis_loading),
      ai_result = values$ai_analysis_result,
      analysis_mode = values$single_analysis_mode %||% "default",
      stat_line_data = values$stat_line_data
    )
  })

  # Render Step 2: Analysis Style (using internal function)
  output$step_2_analysis_style <- renderUI({
    view <- input$analysis_view %||% values$analysis_view %||% "single"
    player_selected <- if (view == "compare") {
      compare_players <- c(input$compare_player1, input$compare_player2, input$compare_player3)
      any(compare_players != "")
    } else {
      !is.null(values$selected_player_info)
    }

    current_mode <- if (view == "compare") {
      values$compare_analysis_mode %||% "default"
    } else {
      values$single_analysis_mode %||% "default"
    }

    generate_step_2_ui(
      player_selected = player_selected,
      current_mode = current_mode
    )
  })

  # Render Step 3: Analysis Results (using internal function)
  output$step_3_analysis_results <- renderUI({
    view <- input$analysis_view %||% values$analysis_view %||% "single"
    analysis_mode <- if (view == "compare") {
      values$compare_analysis_mode %||% "default"
    } else {
      values$single_analysis_mode %||% "default"
    }

    if (view == "compare") {
      compare_players <- c(input$compare_player1, input$compare_player2, input$compare_player3)
      players_selected <- sum(compare_players != "")
      generate_compare_step_ui(
        players_selected = players_selected,
        compare_results = values$compare_results,
        ai_loading = isTRUE(values$compare_ai_loading),
        ai_result = values$compare_ai_result,
        analysis_mode = analysis_mode
      )
    } else {
      player_selected <- !is.null(values$selected_player_info)
      ai_loading <- isTRUE(values$ai_analysis_loading)
      ai_result <- values$ai_analysis_result
      trends_plot <- values$trends_plot

      generate_step_3_ui(
        player_selected = player_selected,
        analysis_mode = analysis_mode,
        ai_loading = ai_loading,
        ai_result = ai_result,
        trends_plot = trends_plot
      )
    }
  })

  # Force UI outputs to not suspend when hidden
  outputOptions(output, "player_preview", suspendWhenHidden = FALSE)
  outputOptions(output, "step_2_analysis_style", suspendWhenHidden = FALSE)
  outputOptions(output, "step_3_analysis_results", suspendWhenHidden = FALSE)
}

