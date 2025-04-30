# Load required packages
library(shiny)

# Try to load imres with error handling
tryCatch({
  library(imres)
  message("imres package loaded successfully")

  # Check if the functions exist
  if (!exists("scholarship_table_server", mode="function")) {
    stop("scholarship_table_server function not found after loading imres package")
  }
  if (!exists("mod_miles_rating_server", mode="function")) {
    stop("mod_miles_rating_server function not found after loading imres package")
  }

}, error = function(e) {
  # If there's an error loading the package, try installing it again
  message("Error loading imres package: ", e$message)
  message("Attempting to install imres package...")

  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }

  remotes::install_github("fbuckhold3/imres", force = TRUE)
  library(imres)

  message("imres package re-installed and loaded")
})


source("R/global.R")  # Load all libraries, API calls, and data

tryCatch({
  source("R/global.R")
  message("global.R loaded successfully")
}, error = function(e) {
  message("Error loading global.R: ", e$message)
  stop("Failed to load global.R")
})


shinyApp(ui = source("R/ui.R")$value, server = source("R/server.R")$value)


##devtools::load_all("~/Documents/GitHub/IMSLU RDM/imres")
##shinyApp(ui, server)


#


### imres::mod_miles_rating_server



