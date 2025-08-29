# McFARLAND Shiny Application

# Load setup and helper scripts
source("R/setup.R")
source("R/cache.R")
source("R/data.R")
source("R/photos.R")
source("R/player_info.R")
source("R/ui_components.R")
source("R/visualizations.R")
source("R/analysis.R")
source("R/styles.R")

# Load UI and server
source("ui.R")
source("server.R")

# Initialize application
shinyApp(ui = ui, server = server)
