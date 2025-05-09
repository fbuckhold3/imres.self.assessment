# Load required packages
library(shiny)


source("R/global.R")  # Load all libraries, API calls, and data

shinyApp(ui = source("R/ui.R")$value, server = source("R/server.R")$value)



