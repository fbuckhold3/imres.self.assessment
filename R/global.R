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

# Define these as global variables at the top of your app.R or global.R file
# (before the server function)

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

# ---------- ENVIRONMENT SETUP ----------
# Set up REDCap API URL
url <- "https://redcapsurvey.slu.edu/api/"

# Identify whether we are in a hosted environment
is_hosted <- Sys.getenv("EVAL_TOKEN") != ""

# Load tokens from environment variables or config file
if (is_hosted) {
  eval_token <- Sys.getenv("EVAL_TOKEN")
  rdm_token <- Sys.getenv("RDM_TOKEN")
  fac_token <- Sys.getenv("FAC_TOKEN")
  new_ass_token <- Sys.getenv("NEW_ASS_TOKEN")

  # Disable SSL verification in the hosted environment (NOT recommended for production)
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
} else {
  # Use config file for local development
  conf <- config::get(file = "config.yml")
  eval_token <- conf$eval_token
  rdm_token <- conf$rdm_token
  fac_token <- conf$fac_token
  new_ass_token <- conf$new_ass_token
}

# Debug token information
cat("=== Checking the tokens exist in Sys.getenv() ===\n")
cat("EVAL_TOKEN is set? ", "EVAL_TOKEN" %in% names(Sys.getenv()), "\n")
cat("RDM_TOKEN is set?  ", "RDM_TOKEN" %in% names(Sys.getenv()), "\n")
cat("FAC_TOKEN is set?  ", "FAC_TOKEN" %in% names(Sys.getenv()), "\n")

# ---------- DATA DICTIONARY RETRIEVAL ----------
# Get data dictionaries
rdm_dict <- get_data_dict(rdm_token, url)
ass_dict <- get_data_dict(eval_token, url)

# ---------- FUNCTIONS FOR DATA RETRIEVAL ----------
# Function to safely pull resident data from REDCap API
get_resident_data <- function() {
  tryCatch({
    ass_dat <- full_api_pull(eval_token, url)
    ass_dat <- wrangle_assessment_data(ass_dat)
    rdm_dat <- forms_api_pull(rdm_token, url, 'resident_data', 'faculty_evaluation', 'ilp', 's_eval', 'scholarship')
    return(create_res_data(ass_dat, rdm_dat))
  }, error = function(e) {
    cat("Error in API pull:", e$message, "\n")
    return(NULL)
  })
}

# ---------- DATA LOADING ----------
# Load resident data
resident_data <- get_resident_data()

# Load scholarship data
schol_data <- tryCatch({
  redcap_read(
    redcap_uri = url,
    token = rdm_token,
    forms = "scholarship"
  )$data
}, error = function(e) {
  cat("Error loading scholarship data:", e$message, "\n")
  return(NULL)
})

# ---------- MILESTONE DATA PROCESSING ----------
# Step 1: Pull all milestone data
miles <- get_all_milestones(rdm_token, url)

# Step 2: Fill in resident data (name, record_id)
miles <- fill_missing_resident_data(miles)

# Step 3: Process program milestones
p_miles <- process_milestones(miles, type = "program")

# Step 4: Process self milestones
s_miles <- process_milestones(miles, type = "self")

# ---------- RESOURCE PATHS ----------
# Set up image resource path for the imres package
# Find where the imres package is installed and set up the resource path
tryCatch({
  # Get the path to the www directory in the imres package
  img_path <- system.file("www", package = "imres")

  # Check if the path exists and is a directory
  if (dir.exists(img_path)) {
    message("Found imres www directory at: ", img_path)
    addResourcePath("imres-images", img_path)
  } else {
    # Try alternative approaches if the standard path doesn't work
    # Check if the package is installed in a different location
    lib_paths <- .libPaths()
    message("Checking library paths: ", paste(lib_paths, collapse = ", "))

    # Search in all library paths
    for (lib in lib_paths) {
      potential_path <- file.path(lib, "imres", "www")
      message("Checking potential path: ", potential_path)
      if (dir.exists(potential_path)) {
        message("Found imres www directory at: ", potential_path)
        addResourcePath("imres-images", potential_path)
        break
      }
    }
  }
}, error = function(e) {
  message("Error setting up imres-images resource path: ", e$message)
  # Provide a fallback mechanism or warning if images aren't available
})

# Verify that the resource path was added successfully
message("Current resource paths: ", paste(names(resourcePaths()), collapse = ", "))
