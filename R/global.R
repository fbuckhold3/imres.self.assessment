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

load_imres_data <- function(config) {
  # Get data dictionaries
  rdm_dict <- safe_get_data_dict(config$rdm_token, config$url)
  ass_dict <- safe_get_data_dict(config$eval_token, config$url)

  # Function to safely pull resident data from REDCap API
  safe_get_resident_data <- function() {
    if (is.null(config$eval_token) || config$eval_token == "" ||
        is.null(config$rdm_token) || config$rdm_token == "") {
      cat("WARNING: Required tokens for resident data are missing\n")
      return(NULL)
    }

    tryCatch({
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

      return(result)
    }, error = function(e) {
      cat("Error in resident data API pull:", e$message, "\n")
      return(NULL)
    })
  }

  # Load resident data
  resident_data <- safe_get_resident_data()

  # Load scholarship data
  schol_data <- tryCatch({
    cat("Pulling scholarship data...\n")
    result <- redcap_read(
      redcap_uri = config$url,
      token = config$rdm_token,
      forms = "scholarship"
    )$data
    cat("Scholarship data pulled with", nrow(result), "rows\n")
    return(result)
  }, error = function(e) {
    cat("Error loading scholarship data:", e$message, "\n")
    return(NULL)
  })

  # Step 1: Pull all milestone data
  miles <- tryCatch({
    cat("Getting all milestones...\n")
    result <- get_all_milestones(config$rdm_token, config$url)
    cat("All milestones retrieved\n")
    return(result)
  }, error = function(e) {
    cat("Error loading milestones:", e$message, "\n")
    return(NULL)
  })

  # Step 2: Fill in resident data (name, record_id)
  if (!is.null(miles)) {
    cat("Filling missing resident data in milestones...\n")
    miles <- tryCatch({
      fill_missing_resident_data(miles)
    }, error = function(e) {
      cat("Error filling resident data:", e$message, "\n")
      return(miles)  # Return original miles if filling fails
    })
    cat("Resident data filled in milestones\n")
  }

  # Step 3 & 4: Process milestones
  p_miles <- NULL
  s_miles <- NULL
  if (!is.null(miles)) {
    p_miles <- tryCatch({
      cat("Processing program milestones...\n")
      result <- process_milestones(miles, type = "program")
      cat("Program milestones processed\n")
      return(result)
    }, error = function(e) {
      cat("Error processing program milestones:", e$message, "\n")
      return(NULL)
    })

    s_miles <- tryCatch({
      cat("Processing self milestones...\n")
      result <- process_milestones(miles, type = "self")
      cat("Self milestones processed\n")
      return(result)
    }, error = function(e) {
      cat("Error processing self milestones:", e$message, "\n")
      return(NULL)
    })
  }

  # Return all the data
  list(
    rdm_dict = rdm_dict,
    ass_dict = ass_dict,
    resident_data = resident_data,
    schol_data = schol_data,
    miles = miles,
    p_miles = p_miles,
    s_miles = s_miles,
    url = config$url,
    eval_token = config$eval_token,
    rdm_token = config$rdm_token,
    fac_token = config$fac_token
  )
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
