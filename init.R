# Install remotes if not already installed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install your package from GitHub with force to update to latest version
remotes::install_github("fbuckhold3/imres", force = TRUE)

# Install other dependencies (add any other packages your app needs)
required_packages <- c(
  "shiny",
  "bslib",
  "redcapAPI",
  "ggplot2",
  "DT",
  "dplyr",
  "config",
  "httr",
  "gganimate",
  "stringr",
  "xml2",
  "fontawesome",
  "tidyr",
  "reactable",
  "htmltools",
  "data.table",
  "purrr",
  "ggradar"
)

# Install all required packages if not already present
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load imres package to check for any load-time errors
library(imres)

# Print a message to verify that the package is loaded
cat("imres package loaded successfully\n")
cat("Available functions include scholarship_table_server:",
    exists("scholarship_table_server", mode="function"), "\n")
cat("Available functions include mod_miles_rating_server:",
    exists("mod_miles_rating_server", mode="function"), "\n")
