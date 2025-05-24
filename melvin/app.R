#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shiny)
library(tidyverse)
library(readr)
library(httr)
library(jsonlite)
library(shinycssloaders)
library(glue)
library(rmarkdown)

#load data, clean, and transform
current_year <- 2025

# read in separate reports because fangraphs doesn't report xWOBA in season-spanning reports
stats <-
  read_csv("~/Downloads/fangraphs-leaderboards-2022.csv", show_col_types = FALSE) |>
  mutate(year = 2022) |>
  bind_rows(read_csv("~/Downloads/fangraphs-leaderboards-2023.csv", show_col_types = FALSE) |>
              mutate(year = 2023)) |>
  bind_rows(read_csv("~/Downloads/fangraphs-leaderboards-2024.csv", show_col_types = FALSE) |>
              mutate(year = 2024)) |>
  bind_rows(read_csv("~/Downloads/fangraphs-leaderboards-2025.csv", show_col_types = FALSE) |>
              mutate(year = 2025)) |>
  mutate(TB = `1B` + (2 * `2B`) + (3 * `3B`) + (4 * HR))

# average of last 3 seasons, per player name/ID
last_3 <- 
  stats |>
  filter(year != current_year) |>
  group_by(Name, PlayerId) |>
  summarize(
    AVG = sum(H) / sum(AB),
    OBP = (sum(H) + sum(BB) + sum(HBP)) /
      (sum(AB) + sum(BB) + sum(HBP) + sum(SF)),
    SLG = sum(TB) / sum(AB),
    K_pct = 100 * sum(SO) / sum(PA),
    BB_pct = 100 * sum(BB) / sum(PA),
    BABIP = (sum(H) - sum(HR)) /
      (sum(AB) - sum(SO) - sum(HR) + sum(SF)),
    wOBA = weighted.mean(wOBA, w = PA, na.rm = TRUE),
    xwOBA = weighted.mean(xwOBA, w = PA, na.rm = TRUE),
    PA = sum(PA)
  ) |>
  ungroup()

# current year
this_year <- 
  stats |>
  filter(year == current_year) |>
  group_by(Name, PlayerId) |>
  summarize(
    AVG = sum(H) / sum(AB),
    OBP = (sum(H) + sum(BB) + sum(HBP)) /
      (sum(AB) + sum(BB) + sum(HBP) + sum(SF)),
    SLG = sum(TB) / sum(AB),
    K_pct = 100 * sum(SO) / sum(PA),
    BB_pct = 100 * sum(BB) / sum(PA),
    BABIP = (sum(H) - sum(HR)) /
      (sum(AB) - sum(SO) - sum(HR) + sum(SF)),
    wOBA = weighted.mean(wOBA, w = PA, na.rm = TRUE),
    
    xwOBA = weighted.mean(xwOBA, w = PA, na.rm = TRUE),
    PA = sum(PA),
    Age = Age
  ) |>
  ungroup()


full_stats <-
  this_year |>
  left_join(last_3, by = c("Name", "PlayerId"), suffix = c("_cur", "_l3")) |>
  mutate(AVG_diff = AVG_cur - AVG_l3,
         OBP_diff = OBP_cur - OBP_l3,
         SLG_diff = SLG_cur - SLG_l3,
         K_pct_diff = K_pct_cur - K_pct_l3,
         BB_pct_diff = BB_pct_cur - BB_pct_l3,
         BABIP_diff = BABIP_cur - BABIP_l3,
         wOBA_diff = wOBA_cur - wOBA_l3,
         xwOBA_diff = xwOBA_cur - xwOBA_l3,
         across(where(is.numeric), ~ round(.x, 3))
  )


player_names <-
  unique(this_year$Name)

# Function to send structured prompt to GPT
# Function to send structured prompt to GPT
generate_gpt_analysis <- function(player_name, prompt_text) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    body = toJSON(list(
      model = "gpt-4.1",
      messages = list(
        list(role = "system", content = "You are a sabermetric baseball analyst tasked with understanding both current in-season performance and getting a sense of future performance for the rest of the season. You need to communicate to a technical audience, but also to the lay audience who just wants to know how concerned they should be about this player."),
        list(role = "user", content = glue("
Here is current-year performance data for {player_name}:

{prompt_text}

Please write a clear, concise analysis explaining how the player is performing this year, what trends stand out, and whether any aspects of the performance appear to be skill- or luck-driven. Start your response with the conclusion/summary takeaways, then underneath, list your evidence for that summary and those conclusions. Incorporate a prediction: will the player improve, decline, or stay the same for the rest of the season? Explain your reasoning.

Your analysis should be framed as: metric, direction, and magnitude of difference. For example BB% is up, indicate by how much, and what the size of that gap might indicate. You don't need to explicitly call out this framing (e.g. in bullets), just make sure to weave it into your analysis.

Writing style: use short but friendly sentences. Don't start with asides or extraneous clauses. 

Separate your analysis into core skills and luck/regression indicators. 

Don't repeat yourself. For example, if you say a stat or performance or trend is 'lucky', you don't need to say it's 'not unlucky'.

Remember that when it comes to stats and trends, you only have knowledge of two things: a player's current-year stats and the average of the same stats for the past 3 years (e.g. not their entire career). So when you say things like a stat is 'up' or 'down', make it clear that this is relative to the last 3 years' average.

The very first element of the response should be a title that encompasses your findings in a catchy, headline-y way -- but keep it honest. 
        "))
      ),
      temperature = 0.7
    ), auto_unbox = TRUE)
  )
  
  parsed <- content(response, as = "parsed", type = "application/json")
  
  output_raw <- parsed$choices[[1]]$message$content
  output_clean <- markdown::renderMarkdown(text = output_raw)
  return(HTML(output_clean))
}

# Function to analyze a player
analyze_player <- function(player_name) {
  player_data <- 
    full_stats %>%
    filter(trimws(tolower(Name)) == trimws(tolower(player_name)))
  
  if (nrow(player_data) < 1) {
    return(HTML(glue("<p>{player_name} not found.</p>")))
  }
  
  output <- glue("
Player: {player_name}

--- Key metrics to analyze---
Age: {player_data$Age}
Year: {current_year}
Plate Appearances: {player_data$PA_cur}
AVG: {player_data$AVG_cur}  Last 3 Years: {player_data$AVG_l3}  Diff: {player_data$AVG_diff}
OBP: {player_data$OBP_cur}   Last 3 Years: {player_data$OBP_l3}  Diff: {player_data$OBP_diff}
SLG: {player_data$SLG_cur}   Last 3 Years: {player_data$SLG_l3}  Diff: {player_data$SLG_diff}
K%: {player_data$K_pct_cur}  Last 3 Years: {player_data$K_pct_l3}  Diff: {player_data$K_pct_diff}
BB%: {player_data$BB_pct_cur}  Last 3 Years: {player_data$BB_pct_l3}  Diff: {player_data$BB_pct_diff}
BABIP: {player_data$BABIP_cur}   Last 3 Years: {player_data$BABIP_l3}  Diff: {player_data$BABIP_diff}
wOBA: {player_data$wOBA_cur}  Last 3 Years: {player_data$wOBA_l3}  Diff: {player_data$wOBA_diff}
xwOBA: {player_data$xwOBA_cur}   Last 3 Years: {player_data$xwOBA_l3}  Diff: {player_data$xwOBA_diff}
")
  
  return(generate_gpt_analysis(player_name, output))
}

# UI
i <- fluidPage(
  titlePanel("McFARLAND"),
  tags$h4("Machine-crafted Forecasting And Reasoning for Luck, Analytics, Narratives, and Data"),
  tags$p("Instant MLB player analysis. Powered by ChatGPT."),
  tags$p("2025 position players only (for now)."),
  tags$p("Data updated every morning."),
  sidebarLayout(
    sidebarPanel(
      textInput("player_name", "Enter a player name:", value = ""),
      uiOutput("player_suggestions"),
      actionButton("analyze", "Analyze Player")
    ),
    mainPanel(
  #    h3(textOutput("selected_player")),
      uiOutput("result_wrapper"),
      tags$style(HTML("#gpt_result {
  max-height: none;
  overflow-y: visible;
  white-space: normal;
}"))
    )
  )
)

# Server
s <- function(input, output, session) {
  output$selected_player <- renderText({ "" })
  
  output$player_suggestions <- renderUI({
    req(input$player_name)
    matches <- full_stats$Name[grepl(input$player_name, full_stats$Name, ignore.case = TRUE)]
    if (length(matches) == 0) return(NULL)
    selectInput("player_suggest", "Did you mean:", choices = matches, selected = matches[1])
  })
  
  observeEvent(input$analyze, {
    req(input$player_suggest)
    selected_name <- input$player_suggest
    output$selected_player <- renderText(selected_name)
    output$gpt_result <- renderUI({
      analyze_player(selected_name)
    })
  })
  
  output$result_wrapper <- renderUI({
    if (input$analyze == 0) {
      return(NULL)
    } else {
      withSpinner(uiOutput("gpt_result"), type = 4, color = "#2C3E50")
    }
  })
}

# Run the app
shinyApp(ui = i, server = s)
