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

# Add this to your global.R file
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

# Add this to your global.R file after initialize_app_config
load_imres_data <- function(config) {
  # Get data dictionaries
  rdm_dict <- tryCatch({
    cat("Attempting to get rdm_dict data dictionary...\n")
    result <- get_data_dict(config$rdm_token, config$url)
    cat("Successfully retrieved rdm_dict with", nrow(result), "rows\n")
    result
  }, error = function(e) {
    cat("Error getting rdm_dict:", e$message, "\n")
    NULL
  })

  schol_data <- tryCatch({
    rdm_dat$scholarship
  }, error = function(e) {
    cat("Warning: scholarship data not found in rdm_dat\n")
    NULL
  })


  ass_dict <- tryCatch({
    cat("Attempting to get ass_dict data dictionary...\n")
    result <- get_data_dict(config$eval_token, config$url)
    cat("Successfully retrieved ass_dict with", nrow(result), "rows\n")
    result
  }, error = function(e) {
    cat("Error getting ass_dict:", e$message, "\n")
    NULL
  })

  # Function to safely pull resident data from REDCap API
  resident_data <- tryCatch({
    cat("Attempting to pull assessment data...\n")
    ass_dat <- full_api_pull(config$eval_token, config$url)
    cat("Successfully pulled assessment data\n")

    cat("Wrangling assessment data...\n")
    ass_dat <- wrangle_assessment_data(ass_dat)
    cat("Assessment data wrangled\n")

    cat("Pulling forms data...\n")
    rdm_dat <- forms_api_pull(config$rdm_token, config$url, 'resident_data', 'faculty_evaluation', 'ilp', 's_eval', 'scholarship')
    cat("Forms data pulled\n")

    cat("Creating resident data...\n")
    result <- create_res_data(ass_dat, rdm_dat)
    cat("Resident data created with", nrow(result), "rows\n")

    result
  }, error = function(e) {
    cat("Error in resident data API pull:", e$message, "\n")
    NULL
  })

  # Load milestone data
  miles <- tryCatch({
    cat("Getting all milestones...\n")
    result <- get_all_milestones(config$rdm_token, config$url)
    cat("All milestones retrieved\n")

    cat("Filling missing resident data in milestones...\n")
    result <- fill_missing_resident_data(result)
    cat("Resident data filled in milestones\n")

    result
  }, error = function(e) {
    cat("Error loading milestones:", e$message, "\n")
    NULL
  })

  # Process milestones
  p_miles <- NULL
  s_miles <- NULL
  if (!is.null(miles)) {
    p_miles <- tryCatch({
      cat("Processing program milestones...\n")
      result <- process_milestones(miles, type = "program")
      cat("Program milestones processed\n")
      result
    }, error = function(e) {
      cat("Error processing program milestones:", e$message, "\n")
      NULL
    })

    s_miles <- tryCatch({
      cat("Processing self milestones...\n")
      result <- process_milestones(miles, type = "self")
      cat("Self milestones processed\n")
      result
    }, error = function(e) {
      cat("Error processing self milestones:", e$message, "\n")
      NULL
    })
  }

  # Return all the data
  list(
    rdm_dict = rdm_dict,
    ass_dict = ass_dict,
    resident_data = resident_data,
    miles = miles,
    schol_data = schol_data,
    p_miles = p_miles,
    s_miles = s_miles,
    url = config$url,
    eval_token = config$eval_token,
    rdm_token = config$rdm_token,
    fac_token = config$fac_token
  )
}

# Define a variable to hold app data
app_data_store <- NULL

# Function to ensure data is loaded
ensure_data_loaded <- function() {
  if (is.null(app_data_store)) {
    # Only initialize data when needed
    cat("Starting data load process...\n")
    config <- initialize_app_config()
    cat("Config initialized\n")
    app_data_store <<- load_imres_data(config)
    cat("Data loaded\n")
  }
  return(app_data_store)
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
