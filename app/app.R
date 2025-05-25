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
library(commonmark)

# Load data and preprocessing
current_year <- 2025

stats <-
  bind_rows(
    read_csv("fangraphs-leaderboards-2022.csv", show_col_types = FALSE) %>% mutate(year = 2022),
    read_csv("fangraphs-leaderboards-2023.csv", show_col_types = FALSE) %>% mutate(year = 2023),
    read_csv("fangraphs-leaderboards-2024.csv", show_col_types = FALSE) %>% mutate(year = 2024),
    read_csv("fangraphs-leaderboards-2025.csv", show_col_types = FALSE) %>% mutate(year = 2025)
  ) %>%
  mutate(
    TB = `1B` + 2*`2B` + 3*`3B` + 4*HR
  )

last_3 <-
  stats %>%
  filter(year != current_year) %>%
  group_by(Name, PlayerId) %>%
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
  stats %>%
  filter(year == current_year) %>%
  group_by(Name, PlayerId) %>%
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
  this_year %>%
  left_join(last_3, by=c("Name","PlayerId"), suffix=c("_cur","_l3")) %>%
  mutate(
    AVG_diff = AVG_cur - AVG_l3,
    OBP_diff = OBP_cur - OBP_l3,
    SLG_diff = SLG_cur - SLG_l3,
    K_pct_diff = K_pct_cur - K_pct_l3,
    BB_pct_diff = BB_pct_cur - BB_pct_l3,
    BABIP_diff = BABIP_cur - BABIP_l3,
    wOBA_diff = wOBA_cur - wOBA_l3,
    xwOBA_diff = xwOBA_cur - xwOBA_l3
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  mutate(Name = stri_trans_general(Name, id="Latin-ASCII"))

# ChatGPT integration
generate_gpt_analysis <- function(player_name, prompt_text, analysis_mode="default") {
  prompt_modifier <- switch(
    analysis_mode,
    "analytics_dork" = "You are a front office nerd, raised on moneyball and new school stats, always at the cutting edge. You favor new school stats, talk in probabilities, and are very dismissive of people who don't believe you. You might be the smartet person in the room, but people would describe you as a real tool.",
    "old_coot" = "You are a deranged old coot, ranting and raving about everything. People would describe you as 'off your meds'.",
    "gen_z" = "You're an over the top Gen Z'er, using lots of slang, referencing hyper modern trends, apps, and such.",
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
        list(role="system", content="You are a sabermetric baseball analyst..."),
        list(role="user", content=glue(
          "Here is current-year performance data for {player_name}:\n\n{prompt_text}\n\n{prompt_modifier}\n\nWrite analysis..."
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
  data <- full_stats %>% filter(trimws(tolower(Name))==trimws(tolower(player_name)))
  prompt <- glue(
    "Player: {player_name}

--- Key metrics to analyze---
" ,
    "Age: {data$Age}
" ,
    "Year: {current_year}
" ,
    "Plate Appearances: {data$PA_cur}
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
  tags$h4("Machine-crafted Forecasting And Reasoning..."),
  fluidRow(
    column(12, lg=4,
           useShinyjs(),
           textInput("player_name","Enter a player name:"),
           uiOutput("player_suggestions"),
           selectInput("analysis_mode","Vibe:",
                       choices=c(
                         "Straightforward"="default",
                         "Analytics dork"="analytics_dork",
                         "Deranged old coot"="old_coot",
                         "Gen Z"="gen_z",
                         "1970s baseball fan"="seventies"
                       )
           ),
           actionButton("analyze","Analyze Player",class="btn btn-success btn-lg"),
           tags$div(style="margin-top:1em;padding:1em;background:#f8f9fa;border:1px solid #dee2e6;border-radius:5px;font-size:0.9em;",
                    HTML("<strong>Version notes:</strong><p>v0.1: Launch.</p><p>v0.2: Added vibe selector.</p>"))
    ),
    column(12, lg=8,
           uiOutput("result_wrapper"),
           tags$style(HTML("#gpt_result{white-space:normal;}") )
    )
  )
)

# Server
server <- function(input,output,session) {
  output$player_suggestions <- renderUI({
    req(input$player_name)
    matches <- full_stats$Name[grepl(input$player_name,full_stats$Name,ignore.case=TRUE)]
    if (length(matches)==0) {
      disable("analyze"); return(NULL)
    } else {
      enable("analyze")
      selectInput("player_suggest","Did you mean:",choices=matches)
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
