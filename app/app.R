# This is a Shiny web application for sabermetric analysis powered by ChatGPT.
# Run with the 'Run App' button above.

library(shiny)
library(tidyverse)
library(readr)
library(httr)
library(jsonlite)
library(stringi)
library(baseballr)
library(glue)
library(shinyWidgets)
library(bslib)
library(commonmark)
library(shinybusy)


# load data from github. these CSV's are generated at 6 AM daily via a Github Action.
load_baseball_data <- function() {
  tryCatch(
    {
      base_url <- "https://raw.githubusercontent.com/rpollack/leadRboard/master/"

      cat("Attempting to load data from:", base_url, "\n")

      full_stats <- read_csv(paste0(base_url, "full_stats.csv"), show_col_types = FALSE)
      cat("Loaded full_stats with", nrow(full_stats), "rows\n")

      player_names <- read_csv(paste0(base_url, "player_names.csv"), show_col_types = FALSE)
      cat("Loaded player_names with", nrow(player_names), "rows\n")

      list(
        full_stats = full_stats,
        player_names = player_names
      )
    },
    error = function(e) {
      cat("Error loading data:", e$message, "\n")
      # Return empty data instead of NULL
      list(
        full_stats = data.frame(),
        player_names = data.frame(Name = character(0))
      )
    }
  )
}

# ChatGPT integration
generate_gpt_analysis <- function(player_name, prompt_text, analysis_mode = "default") {
  prompt_modifier <- switch(analysis_mode,
    "analytics_dork" = "You are a front office nerd, raised on moneyball and new school stats, always at the cutting edge. You favor new school stats, talk in probabilities, and are very dismissive of people who don't believe you. You might be the smartet person in the room, but people would describe you as a real tool. Be ruthless and dismissive!",
    "old_coot" = "You are a deranged old coot, ranting and raving about everything. Yell a lot. People would describe you as 'off your meds'. Throw in references to people spying on you. Appear confused at times. Get stats wrong occasionally. You know, just -- be insane.",
    "gen_z" = "You're an over the top Gen Z'er, using lots of slang, referencing hyper modern trends, apps, emojis, and such. But really lay it on thick, in a humorously over-the-top kind of way.",
    "seventies" = "You prefer 1970s style of baseball, when men were men, stolen bases were high, starting pitchers completed every game, and guys had bushy mustaches and chewed tobacco all game. You strongly prefer old school stats to new school ones. Use lots of comparisons to famous 1970s baseball players: Pete Rose, Johnny Bench, Mike Schmidt, Willie Stargell, Rod Carew, Bobby Grich, Thurman Munson, etc -- but don't limit your comparisons to just these guys.",
    "sensationalist" = "You report baseball analysis like a carnival barker in the jazz age: always trying to make things larger than life through flowery prose and colorful headlines. You practice sensationalist, ballyhoo sportswriting and yellow-journalism-style copy. Every flaw is a titantic tragedy, and every positive is a starry-eyed bright and shiny future.",
    "shakespeare" = "You are William Shakespeare. Not just that, but you speak in verse -- preferably iambic pentameter.",
    "Keep it simple and easy to understand. Use short but friendly sentences. Don't start with asides or extraneous clauses. Start your response with the conclusion/summary takeaways, then underneath, list your evidence for that summary and those conclusions. " # default "Straightforward"
  )
  api_key <- Sys.getenv("OPENAI_API_KEY")
  res <- POST(
    "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    body = toJSON(list(
      model = "gpt-4.1",
      messages = list(
        list(role = "system", content = "You are a sabermetric baseball analyst tasked with understanding both current in-season performance and getting a sense of future performance for the rest of the season. You need to communicate to a technical audience, but also to the lay audience who just wants to know how excited or concerned they should be about this player."),
        list(role = "user", content = glue(
          "Here is current-year performance data for {player_name}:

{prompt_text}

General instructions:

Please analyze how the player is performing this year, what trends stand out, and whether any aspects of the performance appear to be skill- or luck-driven. Incorporate a prediction: will the player improve, decline, or stay the same for the rest of the season? Explain your reasoning.

The very first element of the response should be a title that encompasses your findings.

Your analysis must incorporate metric, direction, and magnitude of difference. For example BB% is up, indicate by how much, and what the size of that gap might indicate. You don't need to explicitly call out this framing (e.g. in bullets), just make sure to weave it into your analysis.

Separate your analysis into core skills and luck/regression indicators.

Don't repeat yourself. For example, if you say a stat or performance or trend is 'lucky', you don't need to say it's 'not unlucky'.

Remember that when it comes to stats and trends, you only have knowledge of two things: a player's current-year stats and the average of the same stats for the past 3 years (e.g. not their entire career). So when you say things like a stat is 'up' or 'down', make it clear that this is relative to the last 3 years' average.

Here is your persona that should inform your writing style and response, even if it means overriding those previous instructions: {prompt_modifier}"
        ))
      ),
      temperature = 0.7
    ), auto_unbox = TRUE)
  )


  parsed <- content(res, as = "parsed", type = "application/json")
  if (!is.null(parsed$error)) {
    return(HTML(paste0("<p style='color:red;'>GPT API error: ", parsed$error$message, "</p>")))
  }
  if (is.null(parsed$choices) || length(parsed$choices) == 0) {
    return(HTML("<p style='color:red;'>No response from GPT.</p>"))
  }
  text <- parsed$choices[[1]]$message$content
  HTML(commonmark::markdown_html(text))
}

# function to analyze a given player
analyze_player <- function(player_name, analysis_mode = "default", full_stats, current_year) {
  data <- full_stats |> filter(trimws(tolower(Name)) == trimws(tolower(player_name)))
  prompt <- glue(
    "Player: {player_name}

--- Key metrics to analyze---
",
    "Age: {data$Age}
",
    "Year: {current_year}
",
    "Plate Appearances (PA): {data$PA_cur}
",
    "AVG: {data$AVG_cur}  Last 3 Years: {data$AVG_l3}  Diff: {data$AVG_diff}
",
    "OBP: {data$OBP_cur}  Last 3 Years: {data$OBP_l3}  Diff: {data$OBP_diff}
",
    "SLG: {data$SLG_cur}  Last 3 Years: {data$SLG_l3}  Diff: {data$SLG_diff}
",
    "K%: {data$K_pct_cur}  Last 3 Years: {data$K_pct_l3}  Diff: {data$K_pct_diff}
",
    "BB%: {data$BB_pct_cur}  Last 3 Years: {data$BB_pct_l3}  Diff: {data$BB_pct_diff}
",
    "BABIP: {data$BABIP_cur}  Last 3 Years: {data$BABIP_l3}  Diff: {data$BABIP_diff}
",
    "wOBA: {data$wOBA_cur}  Last 3 Years: {data$wOBA_l3}  Diff: {data$wOBA_diff}
",
    "xwOBA: {data$xwOBA_cur}  Last 3 Years: {data$xwOBA_l3}  Diff: {data$xwOBA_diff}

",
    "--- Notes for analysis ---
",
    "- Focus on current-year performance compared to the last three years explanations.
",
    "- BABIP above/below norms indicates luck.
",
    "- Gaps between wOBA and xwOBA signal luck vs skill.
",
    "- Remember that xwOBA includes contact quality and plate discipline.
",
    "- BB%/K% changes reflect plate discpline skills, which are more sustainable than batted-ball performance generally. 
",
    "- Take age into account. Older players less likely to improve; younger trend upward. Players generally peak in their early to mid 20's now.
",
    "- Incorporate injuries or known context.
",
    "- For small samples, be cautious with conclusions. For context, larger samples trend towards hundreds of PA. A full season is ~600 PA."
  )
  generate_gpt_analysis(player_name, prompt, analysis_mode)
}

prepare_player_comparison <- function(player_name, full_stats_data) {
  library(tidyr)
  library(dplyr)

  # Filter to the specific player
  player_data <- full_stats_data |>
    filter(Name == player_name)

  # Define the metrics we want to compare (without suffixes)
  base_metrics <- c("AVG", "OBP", "SLG", "K_pct", "BB_pct", "BABIP", "wOBA", "xwOBA")

  # Create comparison data
  comparison_data <- tibble()

  for (metric in base_metrics) {
    current_col <- paste0(metric, "_cur")
    avg_col <- paste0(metric, "_l3") # l3 = last 3 years

    # Check if both columns exist
    if (current_col %in% colnames(player_data) && avg_col %in% colnames(player_data)) {
      # Get current and 3-year average values
      current_val <- player_data[[current_col]]
      avg_val <- player_data[[avg_col]]

      # Create rows for this metric
      metric_data <- tibble(
        player = player_name,
        metric = metric,
        period = c("Past 3 years", "2025"),
        value = c(avg_val, current_val)
      )

      comparison_data <- bind_rows(comparison_data, metric_data)
    }
  }

  return(comparison_data)
}

create_comparison_plot <- function(comparison_data, selected_metrics = NULL) {
  library(ggplot2)

  # Filter to selected metrics if specified
  if (!is.null(selected_metrics)) {
    comparison_data <- comparison_data |>
      filter(metric %in% selected_metrics)
  }

  # Clean up metric names for display
  comparison_data <- comparison_data |>
    mutate(
      metric_display = case_when(
        metric == "K_pct" ~ "K%",
        metric == "BB_pct" ~ "BB%",
        TRUE ~ metric
      )
    )

  # Create the plot
  ggplot(comparison_data, aes(
    x = factor(period, levels = c("Past 3 years", "2025")),
    y = value, group = metric_display
  )) +
    geom_point(size = 3, color = "#2E86AB") +
    geom_line() +
    facet_wrap(~metric_display, scales = "free_y") +
    labs(
      title = paste("Trends: ", unique(comparison_data$player)),
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

ui <- page_navbar(
  title = "McFARLAND",
  header = tagList(
    # (a) “Full‐page” busy spinner
    add_busy_bar(
      # spin     = "fading-circle",
      #     position = "full-page",
      # optionally add text below the spinner (see docs for add_busy_spinner)
      #     color    = "#333333",
      height = "25px"
    ),

    # (b) Custom <meta> + CSS + JS to prevent horizontal scrolling & auto‐close picker
    tags$head(
      # 1a) Lock viewport & disable Safari’s shrink‐to‐fit
      tags$meta(
        name = "viewport",
        content = paste(
          "width=device-width",
          "initial-scale=1",
          "minimum-scale=1",
          "maximum-scale=1",
          "user-scalable=no",
          "viewport-fit=cover",
          "shrink-to-fit=no",
          sep = ", "
        )
      ),

      # 1b) Force no horizontal overflow on page + cards
      tags$style(HTML("
        html, body, .shiny-fill-page {
          width:          100% !important;
          max-width:      100% !important;
          overflow-x:     hidden !important;
        }
        .card, .bslib-card {
          max-width:             100% !important;
          overflow-x:            hidden !important;
          touch-action:          pan-y !important;
          overscroll-behavior-x: none !important;
        }
      ")),

      # 1c) JS to auto‐close Bootstrap‐Select dropdown when an option is picked
      #     Note the selector '#player_name' (must include '#')
      # tags$script(HTML("
      #   $(document).ready(function() {
      #     // whenever the <select> with id='player_name' changes, force it to hide:
      #     $(document).on('changed.bs.select', '#player_name', function(e) {
      #       $(this).selectpicker('hide');
      #     });
      #   });
      # "))
    )
  ),

  # ─────────────────────────────────────────────────────────────────────────────
  # 2) Now your nav_panel()s can come next—no other UI at top level
  # ─────────────────────────────────────────────────────────────────────────────

  # ─────────── Home Panel ───────────
  nav_panel(
    title = "Home",
    icon = icon("home"),
    layout_columns(
      # On ≥768px: col1 = 4/12, col2 = 8/12. On <768px: they stack 12/12.
      col_widths = breakpoints(
        sm = c(12),
        md = c(4, 8),
        lg = c(3, 9)
      ),

      # Column 1: “picker” card
      card(
        #   card_header("McFARLAND"),
        card_body(
          pickerInput(
            inputId = "player_name",
            label = "Player:",
            choices = NULL,
            multiple = FALSE,
            width = "100%",
            options = pickerOptions(
              container = "body",
              dropupAuto = FALSE,
              liveSearch = TRUE,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Search for a player..."
            )
          ),
          pickerInput(
            inputId = "analysis_mode",
            label = "Vibe:",
            choices = c(
              "Straightforward" = "default",
              "Analytics dork" = "analytics_dork",
              "Deranged old coot" = "old_coot",
              "Gen Z" = "gen_z",
              "1970s baseball fan" = "seventies",
              "Sensationalist" = "sensationalist",
              "Shakespeare" = "shakespeare"
            ),
            multiple = FALSE,
            width = "100%",
            options = pickerOptions(
              create      = FALSE,
              container   = "body",
              dropupAuto  = FALSE
            )
          )
        )
      ),

      # Column 2: result card placeholder
      card(
        width = "100%",
        # as_fill_carrier() will ensure this UI expands vertically as needed
        as_fill_carrier(
          uiOutput("result_wrapper")
        )
      )
    )
  ),

  # ─────────── About Panel ───────────
  nav_panel(
    title = "About",
    icon = icon("info-circle"),
    card(
      card_header("About"),
      card_body(
        p("McFARLAND: Machine-crafted Forecasting And Reasoning for Luck, Analytics, Narratives, and Data"),
        img(
          src = "tjmcfarland.png",
          style = "width: 100%; max-width: 400px; height: auto;"
        ),
        p("Hitters only (for now)."),
        p("Data from FanGraphs. Comparing 2025 stats (refreshed daily) to 2022-2024 averages."),
        p("Built with R, shiny, tidyverse, baseballr, bslib, shinyWidgets, and shinybusy."),
        p("Powered by gpt-4.1."),
        h4("Version History"),
        tags$ul(
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
  current_year <- 2025

  # Load data once when app starts
  baseball_data <- load_baseball_data()
  full_stats <- baseball_data$full_stats
  player_names <- baseball_data$player_names

  if (!is.null(baseball_data) && nrow(baseball_data$player_names) > 0) {
    # Update the picker with actual player names
    updatePickerInput(
      session = session,
      inputId = "player_name",
      choices = c("", baseball_data$player_names$Name)
    )
  }

  output$result_wrapper <- renderUI({
    # ensure we're about to analyze a valid player name.
    shiny::validate(
      need(input$player_name %in% player_names$Name, "Enter a valid player name.")
    )

    # Get the main analysis
    main_analysis <- analyze_player(input$player_name, input$analysis_mode, full_stats, current_year)

    # Prepare comparison data
    player_comparison <- prepare_player_comparison(input$player_name, full_stats)

    # Combine everything
    tagList(
      # Your existing analysis
      main_analysis,

      # Add separator and comparison section
      hr(),
      #  h3("Current Season vs 3-Year Average"),

      # The comparison plot
      renderPlot(
        {
          create_comparison_plot(player_comparison)
        },
        height = 400
      )
    )
  })
}

# Run App
shinyApp(ui, server)
