library(shiny)

# Try to install imres package from GitHub if it's not available
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
if (!requireNamespace("imres", quietly = TRUE)) {
  message("Installing imres package from GitHub...")
  remotes::install_github("fbuckhold3/imres")
}


source("R/global.R")  # Load all libraries, API calls, and data
shinyApp(ui = source("R/ui.R")$value, server = source("R/server.R")$value)


##devtools::load_all("~/Documents/GitHub/IMSLU RDM/imres")
##shinyApp(ui, server)


#


### imres::mod_miles_rating_server



