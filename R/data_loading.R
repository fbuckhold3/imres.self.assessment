#' Initialize app configuration and tokens
#'
#' @return A list containing API URL and tokens
#' @export
initialize_app_config <- function() {
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

  # Return the environment with the tokens and URL
  list(
    url = url,
    eval_token = eval_token,
    rdm_token = rdm_token,
    fac_token = fac_token,
    new_ass_token = new_ass_token
  )
}

#' Load data dictionaries from REDCap
#'
#' @param config The configuration list from initialize_app_config
#' @return A list containing data dictionaries
#' @export
load_data_dictionaries <- function(config) {
  rdm_dict <- get_data_dict(config$rdm_token, config$url)
  ass_dict <- get_data_dict(config$eval_token, config$url)
  list(rdm_dict = rdm_dict, ass_dict = ass_dict)
}

#' Load resident data from REDCap
#'
#' @param config The configuration list from initialize_app_config
#' @return Processed resident data
#' @export
load_resident_data <- function(config) {
  tryCatch({
    ass_dat <- full_api_pull(config$eval_token, config$url)
    ass_dat <- wrangle_assessment_data(ass_dat)
    rdm_dat <- forms_api_pull(config$rdm_token, config$url, 'resident_data', 'faculty_evaluation', 'ilp', 's_eval', 'scholarship')
    return(create_res_data(ass_dat, rdm_dat))
  }, error = function(e) {
    cat("Error in API pull:", e$message, "\n")
    return(NULL)
  })
}

#' Load milestone data from REDCap
#'
#' @param config The configuration list from initialize_app_config
#' @return A list containing processed milestone data
#' @export
load_milestone_data <- function(config) {
  # Step 1: Pull all milestone data
  miles <- get_all_milestones(config$rdm_token, config$url)

  # Step 2: Fill in resident data (name, record_id)
  miles <- fill_missing_resident_data(miles)

  # Step 3: Process program milestones
  p_miles <- process_milestones(miles, type = "program")

  # Step 4: Process self milestones
  s_miles <- process_milestones(miles, type = "self")

  list(
    miles = miles,
    p_miles = p_miles,
    s_miles = s_miles
  )
}
