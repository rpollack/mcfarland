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
library(glue)
library(bslib)
library(commonmark)

# Load data and preprocessing
current_year <- 2025

stats <-
  bind_rows(
    read_csv("fangraphs-leaderboards-2022.csv", show_col_types = FALSE) |> mutate(year = 2022),
    read_csv("fangraphs-leaderboards-2023.csv", show_col_types = FALSE) |> mutate(year = 2023),
    read_csv("fangraphs-leaderboards-2024.csv", show_col_types = FALSE) |> mutate(year = 2024),
    read_csv("fangraphs-leaderboards-2025.csv", show_col_types = FALSE) |> mutate(year = 2025)
  ) |>
  mutate(
    TB = `1B` + 2*`2B` + 3*`3B` + 4*HR
  )

last_3 <-
  stats |>
  filter(year != current_year) |>
  group_by(Name, PlayerId) |>
  summarize(
    AVG = sum(H)/sum(AB),
    OBP = (sum(H)+sum(BB)+sum(HBP))/(sum(AB)+sum(BB)+sum(HBP)+sum(SF)),
    SLG = sum(TB)/sum(AB),
    K_pct = 100*sum(SO)/sum(PA),
    BB_pct = 100*sum(BB)/sum(PA),
    BABIP = (sum(H)-sum(HR))/(sum(AB)-sum(SO)-sum(HR)+sum(SF)),
    wOBA = weighted.mean(wOBA, w=PA, na.rm=TRUE),
    xwOBA = weighted.mean(xwOBA, w=PA, na.rm=TRUE),
    PA = sum(PA),
    .groups="drop"
  )

this_year <-
  stats |>
  filter(year == current_year) |>
  group_by(Name, PlayerId) |>
  summarize(
    AVG = sum(H)/sum(AB),
    OBP = (sum(H)+sum(BB)+sum(HBP))/(sum(AB)+sum(BB)+sum(HBP)+sum(SF)),
    SLG = sum(TB)/sum(AB),
    K_pct = 100*sum(SO)/sum(PA),
    BB_pct = 100*sum(BB)/sum(PA),
    BABIP = (sum(H)-sum(HR))/(sum(AB)-sum(SO)-sum(HR)+sum(SF)),
    wOBA = weighted.mean(wOBA, w=PA, na.rm=TRUE),
    xwOBA = weighted.mean(xwOBA, w=PA, na.rm=TRUE),
    PA = sum(PA),
    Age = first(Age),
    .groups="drop"
  )

full_stats <-
  this_year |>
  left_join(last_3, by=c("Name","PlayerId"), suffix=c("_cur","_l3")) |>
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
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  mutate(Name = stri_trans_general(Name, id="Latin-ASCII"))

# ChatGPT integration
generate_gpt_analysis <- function(player_name, prompt_text, analysis_mode="default") {
  prompt_modifier <- switch(
    analysis_mode,
    "analytics_dork" = "You are a front office nerd, raised on moneyball and new school stats, always at the cutting edge. You favor new school stats, talk in probabilities, and are very dismissive of people who don't believe you. You might be the smartet person in the room, but people would describe you as a real tool.",
    "old_coot" = "You are a deranged old coot, ranting and raving about everything. Yell a lot. People would describe you as 'off your meds'. Throw in references to people spying on you. Appear confused at times. Get stats wrong occasionally. You know, just -- be insane.",
    "gen_z" = "You're an over the top Gen Z'er, using lots of slang, referencing hyper modern trends, apps, emojis, and such. But really lay it on thick, in a humorously over-the-top kind of way.",
    "seventies" = "You prefer 1970s style of baseball, when men were men, stolen bases were high, starting pitchers completed every game, and guys had bushy mustaches and chewed tobacco all game. You strongly prefer old school stats to new school ones. Use lots of comparisons to famous 1970s baseball players: Pete Rose, Johnny Bench, Mike Schmidt, Willie Stargell, Rod Carew, Bobby Grich, Thurman Munson, etc -- but don't limit your comparisons to just these guys.",
    ""  # default
  )
  api_key <- Sys.getenv("OPENAI_API_KEY")
  res <- POST(
    "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization=paste("Bearer",api_key)),
    content_type_json(),
    body = toJSON(list(
      model = "gpt-4.1",
      messages = list(
        list(role="system", content="You are a sabermetric baseball analyst tasked with understanding both current in-season performance and getting a sense of future performance for the rest of the season. You need to communicate to a technical audience, but also to the lay audience who just wants to know how concerned they should be about this player."),
        list(role="user", content=glue(
          "Here is current-year performance data for {player_name}:

{prompt_text}

General instructions:

Please write a clear, concise analysis explaining how the player is performing this year, what trends stand out, and whether any aspects of the performance appear to be skill- or luck-driven. Start your response with the conclusion/summary takeaways, then underneath, list your evidence for that summary and those conclusions. Incorporate a prediction: will the player improve, decline, or stay the same for the rest of the season? Explain your reasoning.

The very first element of the response should be a title that encompasses your findings in a catchy, headline-y way -- but keep it honest. 

Your analysis should be framed as: metric, direction, and magnitude of difference. For example BB% is up, indicate by how much, and what the size of that gap might indicate. You don't need to explicitly call out this framing (e.g. in bullets), just make sure to weave it into your analysis.

Separate your analysis into core skills and luck/regression indicators. 

Writing style: use short but friendly sentences. Don't start with asides or extraneous clauses. 

Don't repeat yourself. For example, if you say a stat or performance or trend is 'lucky', you don't need to say it's 'not unlucky'.

Remember that when it comes to stats and trends, you only have knowledge of two things: a player's current-year stats and the average of the same stats for the past 3 years (e.g. not their entire career). So when you say things like a stat is 'up' or 'down', make it clear that this is relative to the last 3 years' average.

Now that I've said that: here is your persona that should inform your writing style and response, even if it means overriding those previous instructions: {prompt_modifier}"
        ))
      ),
      temperature=0.7
    ), auto_unbox=TRUE)
  )
  
  
  parsed <- content(res, as="parsed", type="application/json")
  if (!is.null(parsed$error)) return(HTML(paste0("<p style='color:red;'>GPT API error: ",parsed$error$message,"</p>")))
  if (is.null(parsed$choices) || length(parsed$choices)==0) return(HTML("<p style='color:red;'>No response from GPT.</p>"))
  text <- parsed$choices[[1]]$message$content
  HTML(commonmark::markdown_html(text))
}

analyze_player <- function(player_name, analysis_mode="default") {
  data <- full_stats |> filter(trimws(tolower(Name))==trimws(tolower(player_name)))
  prompt <- glue(
    "Player: {player_name}

--- Key metrics to analyze---
" ,
    "Age: {data$Age}
" ,
    "Year: {current_year}
" ,
    "Plate Appearances (PA): {data$PA_cur}
" ,
    "AVG: {data$AVG_cur}  Last 3 Years: {data$AVG_l3}  Diff: {data$AVG_diff}
" ,
    "OBP: {data$OBP_cur}  Last 3 Years: {data$OBP_l3}  Diff: {data$OBP_diff}
" ,
    "SLG: {data$SLG_cur}  Last 3 Years: {data$SLG_l3}  Diff: {data$SLG_diff}
" ,
    "K%: {data$K_pct_cur}  Last 3 Years: {data$K_pct_l3}  Diff: {data$K_pct_diff}
" ,
    "BB%: {data$BB_pct_cur}  Last 3 Years: {data$BB_pct_l3}  Diff: {data$BB_pct_diff}
" ,
    "BABIP: {data$BABIP_cur}  Last 3 Years: {data$BABIP_l3}  Diff: {data$BABIP_diff}
" ,
    "wOBA: {data$wOBA_cur}  Last 3 Years: {data$wOBA_l3}  Diff: {data$wOBA_diff}
" ,
    "xwOBA: {data$xwOBA_cur}  Last 3 Years: {data$xwOBA_l3}  Diff: {data$xwOBA_diff}

" ,
    "--- Notes for analysis ---
" ,
    "- Focus on current-year performance compared to the last three years explanations.
" ,
    "- BABIP above/below norms indicates luck.
" ,
    "- Gaps between wOBA and xwOBA signal luck vs skill.
" ,
    "- XwOBA includes contact quality and plate discipline.
" ,
    "- BB%/K% changes reflect approach/contact skill.
" ,
    "- Strong BB/K balance shows good zone control.
" ,
    "- Older players less likely to improve; younger trend upward.
" ,
    "- Incorporate injuries or known context.
" ,
    "- For small sample, be cautious with conclusions. For context, larger samples trend towards hundreds of PA. A full season is ~600 PA."
  )
  generate_gpt_analysis(player_name, prompt, analysis_mode)
}

# UI
ui <- fluidPage(
  titlePanel("McFARLAND"),
  tags$h4("Machine-crafted Forecasting And Reasoning for Luck, Analytics, Narratives, and Data"),
  tags$p("Instant MLB player analysis. Powered by ChatGPT."),
  tags$p("2025 position players only (for now)."),
  tags$p("Data updated every morning."),
  layout_columns(
    width = c(4, 8),
    # Input panel
    div(
      useShinyjs(),
      textInput("player_name", "Enter a player name:"),
      uiOutput("player_suggestions"),
      selectInput("analysis_mode", "Vibe:",
                  choices = c(
                    "Straightforward" = "default",
                    "Analytics dork" = "analytics_dork",
                    "Deranged old coot" = "old_coot",
                    "Gen Z" = "gen_z",
                    "1970s baseball fan" = "seventies"
                  )
      ),
      actionButton("analyze", "Analyze Player", class = "btn btn-success btn-lg"),
      tags$div(
        style = "margin-top:1em; padding:1em; background:#f8f9fa; border:1px solid #dee2e6; border-radius:5px; font-size:0.9em;",
        HTML(
          "<strong>Version notes:</strong><p>v0.1: Launch.</p><p>v0.2: Added vibe selector.</p>"
        )
      )
    ),
    # Output panel
    div(
      conditionalPanel(
        condition = "input.analyze > 0",
        withSpinner(uiOutput("result_wrapper"), type = 4, color = "#2C3E50")
      ),
      tags$style(HTML("#gpt_result { white-space: normal; }"))
    )
  )
)

# Server
server <- function(input,output,session) {
  
  
    # Initially disable analyze button until valid input
    shinyjs::disable("analyze")
    output$player_suggestions <- renderUI({
      # Disable analyze if input is empty or just spaces
      if (nchar(trimws(input$player_name %||% "")) == 0) {
        shinyjs::disable("analyze")
        return(NULL)
      }
      # Find matching names
      matches <- full_stats$Name[grepl(input$player_name, full_stats$Name, ignore.case = TRUE)]
      if (length(matches) == 0) {
        shinyjs::disable("analyze")
        return(NULL)
      } else {
        shinyjs::enable("analyze")
        selectInput("player_suggest", "Did you mean:", choices = matches)
      }
    })
  
  observeEvent(input$analyze,{
    req(input$player_suggest)
    output$result_wrapper <- renderUI({
      analyze_player(input$player_suggest,input$analysis_mode)
    })
  })
}

# Run App
shinyApp(ui,server)
