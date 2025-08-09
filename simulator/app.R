# Baseball Game Simulator Shiny App
# Allows users to adjust outcome probabilities and simulate a game.

library(shiny)

# Simulate a single half-inning given event probabilities
simulate_half_inning <- function(probabilities) {
  outs <- 0
  bases <- c(first = 0, second = 0, third = 0)
  runs <- 0
  events <- names(probabilities)
  while (outs < 3) {
    event <- sample(events, size = 1, prob = probabilities)
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
  }
  runs
}

# Simulate a full game, returning a scoreboard and totals
simulate_game <- function(probA, probB, innings = 9) {
  inning <- 1
  runsA <- c()
  runsB <- c()
  while (TRUE) {
    runsA[inning] <- simulate_half_inning(probA)
    runsB[inning] <- simulate_half_inning(probB)
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
       home_total = sum(runsB))
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
      textOutput("result")
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
    sim <- simulate_game(probA, probB)
    output$scoreboard <- renderTable(sim$scoreboard, rownames = TRUE)
    output$result <- renderText(
      sprintf("Final Score: Team A %d, Team B %d", sim$away_total, sim$home_total)
    )
  })
}

shinyApp(ui, server)
