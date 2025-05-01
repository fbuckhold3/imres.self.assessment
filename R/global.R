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
