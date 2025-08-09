# Baseball Game Simulator Shiny App
# Allows users to adjust outcome probabilities, simulate a game,
# and generate a newspaper-style recap using an LLM.

library(shiny)
library(httr)
library(jsonlite)

# Load a list of player names to use in randomly generated lineups
get_lineup <- function(n = 9) {
  names <- read.csv("../player_names.csv", stringsAsFactors = FALSE)$Name
  sample(names, n)
}

# Simulate a single half-inning given event probabilities and a lineup
simulate_half_inning <- function(probabilities, lineup, start_index = 1) {
  outs <- 0
  bases <- c(first = 0, second = 0, third = 0)
  runs <- 0
  events <- names(probabilities)
  log <- data.frame(player = character(), event = character(), stringsAsFactors = FALSE)
  idx <- start_index

  while (outs < 3) {
    batter <- lineup[idx]
    event <- sample(events, size = 1, prob = probabilities)
    log <- rbind(log, data.frame(player = batter, event = event, stringsAsFactors = FALSE))

    if (event %in% c("single", "walk")) {
      runs <- runs + bases["third"]
      bases["third"] <- bases["second"]
      bases["second"] <- bases["first"]
      bases["first"] <- 1
    } else if (event == "double") {
      runs <- runs + bases["third"] + bases["second"]
      bases["third"] <- bases["first"]
      bases["second"] <- 1
      bases["first"] <- 0
    } else if (event == "triple") {
      runs <- runs + bases["third"] + bases["second"] + bases["first"]
      bases["third"] <- 1
      bases["second"] <- 0
      bases["first"] <- 0
    } else if (event == "home_run") {
      runs <- runs + bases["third"] + bases["second"] + bases["first"] + 1
      bases[] <- 0
    } else { # out
      outs <- outs + 1
    }
    idx <- ifelse(idx == length(lineup), 1, idx + 1)
  }
  list(runs = runs, next_index = idx, log = log)
}

# Simulate a full game, returning a scoreboard, totals, and play log
simulate_game <- function(probA, probB, lineupA, lineupB, innings = 9) {
  inning <- 1
  runsA <- c()
  runsB <- c()
  log <- data.frame(inning = integer(), half = character(), player = character(),
                    event = character(), stringsAsFactors = FALSE)
  idxA <- 1
  idxB <- 1

  while (TRUE) {
    top <- simulate_half_inning(probA, lineupA, idxA)
    idxA <- top$next_index
    runsA[inning] <- top$runs
    if (nrow(top$log) > 0) {
      log <- rbind(log, data.frame(inning = inning, half = "Top",
                                   player = top$log$player, event = top$log$event))
    }

    bottom <- simulate_half_inning(probB, lineupB, idxB)
    idxB <- bottom$next_index
    runsB[inning] <- bottom$runs
    if (nrow(bottom$log) > 0) {
      log <- rbind(log, data.frame(inning = inning, half = "Bottom",
                                   player = bottom$log$player, event = bottom$log$event))
    }

    if (inning >= innings && sum(runsA) != sum(runsB)) {
      break
    }
    inning <- inning + 1
  }

  scoreboard <- rbind(
    `Team A` = c(runsA, sum(runsA)),
    `Team B` = c(runsB, sum(runsB))
  )
  colnames(scoreboard) <- c(seq_len(length(runsA)), "R")

  list(scoreboard = as.data.frame(scoreboard),
       away_total = sum(runsA),
       home_total = sum(runsB),
       log = log)
}

# Convert play log and scoreboard into a prompt and call the OpenAI API
generate_narrative <- function(log, scoreboard) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") {
    return("Set OPENAI_API_KEY to generate a narrative.")
  }

  # Build play-by-play strings
  event_text <- function(e) switch(e,
                                   single = "singled",
                                   double = "doubled",
                                   triple = "tripled",
                                   home_run = "hit a home run",
                                   walk = "walked",
                                   out = "made an out",
                                   e)
  plays <- apply(log, 1, function(row) {
    sprintf("%s of the %s: %s %s", row["half"], row["inning"], row["player"], event_text(row["event"]))
  })
  score_txt <- paste(capture.output(print(scoreboard)), collapse = "\n")
  prompt <- paste("Provide a concise newspaper-style recap of this baseball game.",
                  "\nScoreboard:\n", score_txt,
                  "\nKey Plays:\n", paste(plays, collapse = "\n"))

  resp <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(list(role = "user", content = prompt)),
      temperature = 0.7
    )
  )

  content <- tryCatch(content(resp, as = "parsed"), error = function(e) NULL)
  if (!is.null(content$choices[[1]]$message$content)) {
    content$choices[[1]]$message$content
  } else {
    "Narrative generation failed."
  }
}

ui <- fluidPage(
  titlePanel("Baseball Game Simulator"),
  sidebarLayout(
    sidebarPanel(
      h3("Team A Probabilities"),
      numericInput("a_single", "Single", value = 0.15, min = 0, max = 1, step = 0.01),
      numericInput("a_double", "Double", value = 0.05, min = 0, max = 1, step = 0.01),
      numericInput("a_triple", "Triple", value = 0.01, min = 0, max = 1, step = 0.01),
      numericInput("a_hr", "Home Run", value = 0.04, min = 0, max = 1, step = 0.01),
      numericInput("a_walk", "Walk", value = 0.08, min = 0, max = 1, step = 0.01),
      numericInput("a_out", "Out", value = 0.67, min = 0, max = 1, step = 0.01),
      h3("Team B Probabilities"),
      numericInput("b_single", "Single", value = 0.15, min = 0, max = 1, step = 0.01),
      numericInput("b_double", "Double", value = 0.05, min = 0, max = 1, step = 0.01),
      numericInput("b_triple", "Triple", value = 0.01, min = 0, max = 1, step = 0.01),
      numericInput("b_hr", "Home Run", value = 0.04, min = 0, max = 1, step = 0.01),
      numericInput("b_walk", "Walk", value = 0.08, min = 0, max = 1, step = 0.01),
      numericInput("b_out", "Out", value = 0.67, min = 0, max = 1, step = 0.01),
      actionButton("simulate", "Simulate Game")
    ),
    mainPanel(
      tableOutput("scoreboard"),
      textOutput("result"),
      verbatimTextOutput("narrative")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$simulate, {
    probA <- c(single = input$a_single, double = input$a_double, triple = input$a_triple,
               home_run = input$a_hr, walk = input$a_walk, out = input$a_out)
    probB <- c(single = input$b_single, double = input$b_double, triple = input$b_triple,
               home_run = input$b_hr, walk = input$b_walk, out = input$b_out)
    probA <- probA / sum(probA)
    probB <- probB / sum(probB)

    lineupA <- get_lineup()
    lineupB <- get_lineup()
    sim <- simulate_game(probA, probB, lineupA, lineupB)

    output$scoreboard <- renderTable(sim$scoreboard, rownames = TRUE)
    output$result <- renderText(
      sprintf("Final Score: Team A %d, Team B %d", sim$away_total, sim$home_total)
    )
    output$narrative <- renderText(generate_narrative(sim$log, sim$scoreboard))
  })
}

shinyApp(ui, server)

