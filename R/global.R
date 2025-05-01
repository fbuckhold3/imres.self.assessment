# ---------- LIBRARY IMPORTS ----------
library(shiny)
library(shinyjs)
library(redcapAPI)
library(REDCapR)
library(ggplot2)
library(DT)
library(dplyr)
library(config)
library(imres)
library(bslib)
library(httr)
library(gganimate)
library(stringr)
library(xml2)
library(fontawesome)
library(tidyr)
library(reactable)
library(htmltools)
library(data.table)
library(purrr)
library(ggradar)

# In global.R, after your library imports

# Helper function to safely get data dictionary
safe_get_data_dict <- function(token, url) {
  if (is.null(token) || token == "") {
    cat("WARNING: Token is missing or empty, cannot get data dictionary\n")
    return(NULL)
  }

  tryCatch({
    cat("Attempting to get data dictionary with token length:", nchar(token), "\n")
    result <- get_data_dict(token, url)
    cat("Successfully retrieved data dictionary with", nrow(result), "rows\n")
    return(result)
  }, error = function(e) {
    cat("ERROR getting data dictionary:", e$message, "\n")
    return(NULL)
  })
}

# Initialize app configuration and tokens
initialize_app_config <- function() {
  # Set up REDCap API URL
  url <- "https://redcapsurvey.slu.edu/api/"

  # Debug information about environment variables
  cat("Available environment variables (first 10):\n")
  env_vars <- names(Sys.getenv())
  print(head(env_vars, 10))
  cat("EVAL_TOKEN exists:", "EVAL_TOKEN" %in% names(Sys.getenv()), "\n")
  cat("RDM_TOKEN exists:", "RDM_TOKEN" %in% names(Sys.getenv()), "\n")
  cat("FAC_TOKEN exists:", "FAC_TOKEN" %in% names(Sys.getenv()), "\n")

  # Identify whether we are in a hosted environment
  is_hosted <- Sys.getenv("EVAL_TOKEN") != ""

  # Load tokens from environment variables or config file
  if (is_hosted) {
    eval_token <- Sys.getenv("EVAL_TOKEN")
    rdm_token <- Sys.getenv("RDM_TOKEN")
    fac_token <- Sys.getenv("FAC_TOKEN")

    # Check if tokens are empty strings even though they exist
    if (nchar(eval_token) == 0 || nchar(rdm_token) == 0 || nchar(fac_token) == 0) {
      cat("WARNING: One or more required tokens are empty in environment!\n")
      cat("Using config file as fallback.\n")
      # Load from config file as fallback
      conf <- tryCatch({
        config::get(file = "config.yml")
      }, error = function(e) {
        message("Error loading config file: ", e$message)
        list(
          eval_token = "",
          rdm_token = "",
          fac_token = ""
        )
      })

      # Use config values if environment variables are empty
      if (nchar(eval_token) == 0) eval_token <- conf$eval_token
      if (nchar(rdm_token) == 0) rdm_token <- conf$rdm_token
      if (nchar(fac_token) == 0) fac_token <- conf$fac_token
    }

    # Disable SSL verification in the hosted environment (NOT recommended for production)
    httr::set_config(httr::config(ssl_verifypeer = FALSE))
  } else {
    # Use config file for local development
    conf <- tryCatch({
      config::get(file = "config.yml")
    }, error = function(e) {
      message("Error loading config file: ", e$message)
      list(
        eval_token = "",
        rdm_token = "",
        fac_token = ""
      )
    })
    eval_token <- conf$eval_token
    rdm_token <- conf$rdm_token
    fac_token <- conf$fac_token
  }

  # Print token values (length only for security)
  cat("EVAL_TOKEN length:", nchar(eval_token), "\n")
  cat("RDM_TOKEN length:", nchar(rdm_token), "\n")
  cat("FAC_TOKEN length:", nchar(fac_token), "\n")

  # Return the environment with the tokens and URL
  list(
    url = url,
    eval_token = eval_token,
    rdm_token = rdm_token,
    fac_token = fac_token
  )
}

# Global storage for app data
app_data_store <- NULL

# Function to ensure data is loaded
ensure_data_loaded <- function() {
  if (is.null(app_data_store)) {
    cat("Starting data load process...\n")

    # Get configuration with tokens
    config <- initialize_app_config()

    # First, get the data dictionaries
    rdm_dict <- safe_get_data_dict(config$rdm_token, config$url)
    ass_dict <- safe_get_data_dict(config$eval_token, config$url)

    # Only continue if we have the dictionaries
    if (is.null(rdm_dict) || is.null(ass_dict)) {
      cat("CRITICAL ERROR: Failed to load data dictionaries\n")
      return(NULL)
    }

    # Continue with other data loading
    # ... rest of your loading code

    # Store the results
    app_data_store <<- list(
      rdm_dict = rdm_dict,
      ass_dict = ass_dict,
      url = config$url,
      eval_token = config$eval_token,
      rdm_token = config$rdm_token,
      fac_token = config$fac_token
      # Add other data as loaded
    )
  }

  return(app_data_store)
}

# Try to load data now
cat("Pre-loading data in global.R...\n")
app_data <- ensure_data_loaded()

# Make data globally available if loaded
if (!is.null(app_data)) {
  rdm_dict <- app_data$rdm_dict
  ass_dict <- app_data$ass_dict
  url <- app_data$url
  eval_token <- app_data$eval_token
  rdm_token <- app_data$rdm_token
  fac_token <- app_data$fac_token
  cat("Global variables created from app_data\n")
} else {
  cat("WARNING: app_data is NULL, global variables not created\n")
}

# Helper lists for competencies and milestones
subcompetency_maps <- list(
  PC = list(
    "1" = "History",
    "2" = "Physical Examination",
    "3" = "Clinical Reasoning",
    "4" = "Patient Management - Inpatient",
    "5" = "Patient Management - Outpatient",
    "6" = "Digital Health"
  ),
  MK = list(
    "1" = "Applied Foundational Sciences",
    "2" = "Therapeutic Knowledge",
    "3" = "Knowledge of Diagnostic Testing"
  ),
  SBP = list(
    "1" = "Patient Safety and Quality Improvement",
    "2" = "System Navigation for Patient-Centered Care",
    "3" = "Physician Role in Health Care Systems"
  ),
  PBLI = list(
    "1" = "Evidence-Based and Informed Practice",
    "2" = "Reflective Practice and Commitment to Personal Growth"
  ),
  PROF = list(
    "1" = "Professional Behavior",
    "2" = "Ethical Principles",
    "3" = "Accountability/Conscientiousness",
    "4" = "Knowledge of Systemic and Individual Factors of Well-Being"
  ),
  ICS = list(
    "1" = "Patient- and Family-Centered Communication",
    "2" = "Interprofessional and Team Communication",
    "3" = "Communication within Health Care Systems"
  )
)

milestone_levels <- list(
  "1" = "Novice",
  "2" = "Advanced beginner",
  "3" = "Competent",
  "4" = "Proficient",
  "5" = "Expert"
)

competency_list <- list(
  PC = "Patient Care",
  MK = "Medical Knowledge",
  SBP = "Systems-Based Practice",
  PBLI = "Practice-Based Learning and Improvement",
  PROF = "Professionalism",
  ICS = "Interpersonal and Communication Skills"
)

# ---------- HELPER FUNCTIONS ----------
# Load helper functions
source("R/helpers.R")

# Define a reactive value to hold app data
app_data_store <- NULL

# Function to ensure data is loaded
ensure_data_loaded <- function() {
  if (is.null(app_data_store)) {
    # Only initialize data when needed
    config <- initialize_app_config()
    app_data_store <<- load_imres_data(config)
  }
  return(app_data_store)
}
