library(shiny)

source("R/global.R")  # Load all libraries, API calls, and data
shinyApp(ui = source("R/ui.R")$value, server = source("R/server.R")$value)


devtools::load_all("~/Documents/GitHub/IMSLU RDM/imres")
shinyApp(ui, server)

# WdyPEh 7uG6gS   NYqoWH   ujflcv  i9MmGj   TuAjIG
#


