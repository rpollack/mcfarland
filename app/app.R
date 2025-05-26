# This is a Shiny web application for sabermetric analysis powered by ChatGPT.
# Run with the 'Run App' button above.

library(shiny)
library(tidyverse)
library(readr)
library(httr)
library(shinyjs)
library(jsonlite)
library(shinycssloaders)
library(stringi)
library(baseballr)
library(glue)
library(shinyWidgets)
library(bslib)
library(commonmark)

# Load data and preprocessing
current_year <- 2025

stats <-
  bind_rows(
    read_csv("fangraphs-leaderboards-2022.csv", show_col_types = FALSE) |> mutate(year = 2022),
    read_csv("fangraphs-leaderboards-2023.csv", show_col_types = FALSE) |> mutate(year = 2023),
    read_csv("fangraphs-leaderboards-2024.csv", show_col_types = FALSE) |> mutate(year = 2024),
    fg_bat_leaders(pos = "np", startseason = current_year, endseason = current_year) |>
      select(Name = PlayerName, Age, PlayerId = playerid, AB, PA, `1B`, `2B`, `3B`, HR, H, HBP, SF, wOBA, xwOBA, SO, BB) |> 
      mutate(year = current_year)
  ) |>
  mutate(
    TB = `1B` + 2 * `2B` + 3 * `3B` + 4 * HR,
    Name = stri_trans_general(Name, id = "Latin-ASCII")
  ) 

last_3 <-
  stats |>
  filter(year != current_year) |>
  group_by(Name, PlayerId) |>
  summarize(
    AVG = sum(H) / sum(AB),
    OBP = (sum(H) + sum(BB) + sum(HBP)) / (sum(AB) + sum(BB) + sum(HBP) + sum(SF)),
    SLG = sum(TB) / sum(AB),
    K_pct = 100 * sum(SO) / sum(PA),
    BB_pct = 100 * sum(BB) / sum(PA),
    BABIP = (sum(H) - sum(HR)) / (sum(AB) - sum(SO) - sum(HR) + sum(SF)),
    wOBA = weighted.mean(wOBA, w = PA, na.rm = TRUE),
    xwOBA = weighted.mean(xwOBA, w = PA, na.rm = TRUE),
    PA = sum(PA),
    .groups = "drop"
  )

this_year <-
  stats |>
  filter(year == current_year) |>
  group_by(Name, PlayerId) |>
  summarize(
    AVG = sum(H) / sum(AB),
    OBP = (sum(H) + sum(BB) + sum(HBP)) / (sum(AB) + sum(BB) + sum(HBP) + sum(SF)),
    SLG = sum(TB) / sum(AB),
    K_pct = 100 * sum(SO) / sum(PA),
    BB_pct = 100 * sum(BB) / sum(PA),
    BABIP = (sum(H) - sum(HR)) / (sum(AB) - sum(SO) - sum(HR) + sum(SF)),
    wOBA = weighted.mean(wOBA, w = PA, na.rm = TRUE),
    xwOBA = weighted.mean(xwOBA, w = PA, na.rm = TRUE),
    PA = sum(PA),
    Age = first(Age),
    .groups = "drop"
  )

full_stats <-
  this_year |>
  left_join(last_3, by = c("Name", "PlayerId"), suffix = c("_cur", "_l3")) |>
  mutate(
    AVG_diff = AVG_cur - AVG_l3,
    OBP_diff = OBP_cur - OBP_l3,
    SLG_diff = SLG_cur - SLG_l3,
    K_pct_diff = K_pct_cur - K_pct_l3,
    BB_pct_diff = BB_pct_cur - BB_pct_l3,
    BABIP_diff = BABIP_cur - BABIP_l3,
    wOBA_diff = wOBA_cur - wOBA_l3,
    xwOBA_diff = xwOBA_cur - xwOBA_l3
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) 

player_names <-
  this_year |>
  select(Name) |>
  distinct()

# ChatGPT integration
generate_gpt_analysis <- function(player_name, prompt_text, analysis_mode = "default") {
  prompt_modifier <- switch(analysis_mode,
    "analytics_dork" = "You are a front office nerd, raised on moneyball and new school stats, always at the cutting edge. You favor new school stats, talk in probabilities, and are very dismissive of people who don't believe you. You might be the smartet person in the room, but people would describe you as a real tool. Be ruthless and dismissive!",
    "old_coot" = "You are a deranged old coot, ranting and raving about everything. Yell a lot. People would describe you as 'off your meds'. Throw in references to people spying on you. Appear confused at times. Get stats wrong occasionally. You know, just -- be insane.",
    "gen_z" = "You're an over the top Gen Z'er, using lots of slang, referencing hyper modern trends, apps, emojis, and such. But really lay it on thick, in a humorously over-the-top kind of way.",
    "seventies" = "You prefer 1970s style of baseball, when men were men, stolen bases were high, starting pitchers completed every game, and guys had bushy mustaches and chewed tobacco all game. You strongly prefer old school stats to new school ones. Use lots of comparisons to famous 1970s baseball players: Pete Rose, Johnny Bench, Mike Schmidt, Willie Stargell, Rod Carew, Bobby Grich, Thurman Munson, etc -- but don't limit your comparisons to just these guys.",
    "" # default
  )
  api_key <- Sys.getenv("OPENAI_API_KEY")
  res <- POST(
    "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    body = toJSON(list(
      model = "gpt-4.1",
      messages = list(
        list(role = "system", content = "You are a sabermetric baseball analyst tasked with understanding both current in-season performance and getting a sense of future performance for the rest of the season. You need to communicate to a technical audience, but also to the lay audience who just wants to know how concerned they should be about this player."),
        list(role = "user", content = glue(
          "Here is current-year performance data for {player_name}:

{prompt_text}

General instructions:

Please write a clear, concise analysis explaining how the player is performing this year, what trends stand out, and whether any aspects of the performance appear to be skill- or luck-driven. Start your response with the conclusion/summary takeaways, then underneath, list your evidence for that summary and those conclusions. Incorporate a prediction: will the player improve, decline, or stay the same for the rest of the season? Explain your reasoning.

The very first element of the response should be a title that encompasses your findings in a catchy, headline-y way -- but keep it honest. The headline should not be a question.

Your analysis should be framed as: metric, direction, and magnitude of difference. For example BB% is up, indicate by how much, and what the size of that gap might indicate. You don't need to explicitly call out this framing (e.g. in bullets), just make sure to weave it into your analysis.

Separate your analysis into core skills and luck/regression indicators.

Writing style: use short but friendly sentences. Don't start with asides or extraneous clauses.

Don't repeat yourself. For example, if you say a stat or performance or trend is 'lucky', you don't need to say it's 'not unlucky'.

Remember that when it comes to stats and trends, you only have knowledge of two things: a player's current-year stats and the average of the same stats for the past 3 years (e.g. not their entire career). So when you say things like a stat is 'up' or 'down', make it clear that this is relative to the last 3 years' average.

Now that I've said that: here is your persona that should inform your writing style and response, even if it means overriding those previous instructions: {prompt_modifier}"
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

analyze_player <- function(player_name, analysis_mode = "default") {
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
    "- BB%/K% changes reflect approach/contact skill.
",
    "- Strong BB/K balance shows good zone control.
",
    "- Take age into account. Older players less likely to improve; younger trend upward. Players generally peak in their early to mid 20's now.
",
    "- Incorporate injuries or known context.
",
    "- For small sample, be cautious with conclusions. For context, larger samples trend towards hundreds of PA. A full season is ~600 PA."
  )
  generate_gpt_analysis(player_name, prompt, analysis_mode)
}

ui <- page_fillable(
  #'hard-code' theme to prevent future breakage
  theme = bs_theme(version = 5),
  useShinyjs(),
  
  # attempt to prevent horiz scrolling om mobile.
  tags$head(
    # 1) lock viewport & disable shrink-to-fit
    tags$meta(
      name    = "viewport",
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
    
    # 2) force no horizontal overflow on load
    tags$style(HTML("
      html, body, .shiny-fill-page {
        width:          100% !important;
        max-width:      100% !important;
        overflow-x:     hidden !important;
      }
      .card, .bslib-card {
        max-width:          100% !important;
        overflow-x:         hidden !important;
        touch-action:       pan-y !important;
        overscroll-behavior-x: none !important;
      }
      
      /* hide the text‐entry field inside a single‐selectize input 
      
      disabled for now. 
.selectize-control.single .selectize-input > input {
  display: none !important;
  pointer-events: none !important;
  cursor: default !important;
}

/* optionally remove the blinking caret */
.selectize-control.single .selectize-input.is-focused {
  caret-color: transparent !important;
  
  */
}

    "))),
  
  #title = "McFARLAND: Instant MLB Player Analysis",
  
  # def 12 column layout per screen size
  layout_columns(
    col_widths = breakpoints(
      sm = c(12),
      md = c(6, 6),
      lg = c(4, 8)
    ),
  #fillable_mobile = TRUE,
  card(
    card_header("McFARLAND"),
    selectizeInput(
      "player_name",
      "Player:",
      
      # start off input with blank/placeholder text
      choices = c("", this_year$Name),
      options = list(
        placeholder = "Type a player name...",
        maxOptions = this_year |> distinct(Name) |> nrow(),
        multiple = FALSE,
        create = FALSE,
        dropdownParent = "body"
      ),
      
    ),
    selectizeInput(
      "analysis_mode",
      "Vibe:",
      choices = c(
        "Straightforward" = "default",
        "Analytics dork" = "analytics_dork",
        "Deranged old coot" = "old_coot",
        "Gen Z" = "gen_z",
        "1970s baseball fan" = "seventies"
      ),
      options = list(
        create = FALSE,
        dropdownParent = "body"
      ),
      # options = list(
      #   dropdownParent = "body"
      # )
    ),
  #  actionButton("analyze", "Analyze", class = "btn btn-success btn-sm"),
  ),
  card(
    width = "100%",
    # display progress spinner only if analysis is running
    # conditionalPanel(
    #   condition = "input.analyze > 0",
      withSpinner(as_fill_carrier(uiOutput("result_wrapper")),
                  caption = "Analyzing...")
    # ),
    # card_footer(
    #   hr(),
    #   p("About"),
    #   p("McFARLAND: Machine-crafted Forecasting And Reasoning for Luck, Analytics, Narratives, and Data"),
    #   img(src="tjmcfarland.png",
    #       width = "100%"),
    #   p("Powered by ChatGPT."),
    #   p("2025 position players  only (for now). Data refreshed daily. Comparisons are made to 2022-2024 data (cumulative)."),
    #   p("Thanks to baseballr by Bill Petti!")
    # )
  ),
),
)


# Server
server <- function(input, output, session) {

  output$result_wrapper <- renderUI({
    
    # ensure we're about to analyze a valid player name.
    shiny::validate(
      need(input$player_name %in% this_year$Name, "Enter a valid player name.")
    )
    
    analyze_player(input$player_name, input$analysis_mode)
  })

}

# Run App
shinyApp(ui, server)
