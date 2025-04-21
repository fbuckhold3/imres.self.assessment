# Install remotes if not already installed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install your package from GitHub with force to update to latest version
remotes::install_github("fbuckhold3/imres", force = TRUE)

# Install other dependencies
required_packages <- c(
  "shiny",
  "bslib",
  "redcapAPI",
  "ggplot2",
  "DT",
  "dplyr",
  "config",
  "httr"  # Needed for API calls
)

# Install all required packages if not already present
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load packages to check for any load-time errors
library(imres)


