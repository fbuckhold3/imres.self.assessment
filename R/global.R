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
imres_www_path <- file.path(renv::paths$library(), "imres", "www")
addResourcePath("imres-images", imres_www_path)

# Debug resource path information
message("Image path: ", imres_www_path)
message("Path exists: ", dir.exists(imres_www_path))
message("Files: ", paste(list.files(imres_www_path), collapse=", "))
