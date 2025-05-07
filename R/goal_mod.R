# Helper Functions
extract_previous_goal_by_domain <- function(prev_data, domain) {
  if(is.null(prev_data)) return(NULL)

  milestone_fields <- list(
    "pcmk" = c("pc1_r1", "pc1_r2", "pc2_r1", "pc2_r2", "pc3_r1", "pc3_r2",
               "pc4_r1", "pc4_r2", "pc5_r1", "pc5_r2", "pc5_r3", "pc6_r1",
               "pc6_r2", "mk1_r1", "mk2_r1", "mk3_r1", "mk3_r2"),
    "sbppbl" = c("sbp1_r1", "sbp1_r2", "sbp1_r3", "sbp2_r1", "sbp2_r2", "sbp2_r3",
                 "sbp3_r1", "sbp3_r2", "pbl1_r1", "pbl2_r1", "pbl2_r2", "pbl2_r3"),
    "profics" = c("prof1_r1", "prof2_r1", "prof3_r1", "prof4_r1", "prof4_r2",
                  "ics1_r1", "ics1_r2", "ics2_r1", "ics2_r2", "ics3_r1", "ics3_r2")
  )

  fields <- milestone_fields[[tolower(domain)]]
  if(is.null(fields)) return(NULL)

  for(field in fields) {
    if(field %in% names(prev_data) && !is.na(prev_data[[field]]) && prev_data[[field]] != "") {
      return(list(field = field, value = prev_data[[field]]))
    }
  }

  return(NULL)
}

# Custom radio buttons with no selection by default
safe_radio_buttons_empty <- function(inputId, label, choices, inline = FALSE) {
  if (length(choices) == 0) {
    return(div(class = "alert alert-warning", "No options available"))
  }
  radioButtons(inputId, label, choices, inline = inline, selected = character(0))
}

load_previous_goals <- function(ilp_data, record_id, current_period) {
  period_sequence <- c(
    "Entering Residency" = NA,
    "Mid Intern" = "Entering Residency",
    "End Intern" = "Mid Intern",
    "Mid PGY2" = "End Intern",
    "End PGY2" = "Mid PGY2",
    "Mid PGY3" = "End PGY2",
    "Graduating" = "Mid PGY3"
  )

  previous_period <- period_sequence[current_period]

  if(is.na(previous_period)) {
    return(NULL)
  }

  prev_data <- ilp_data %>%
    filter(record_id == !!record_id,
           year_resident == previous_period)

  if(nrow(prev_data) == 0) {
    return(NULL)
  }

  return(prev_data[1, ])
}

# Add this to your helper functions section
get_milestone_description <- function(rdm_dict_data, competency_prefix, row_num, level) {
  req(rdm_dict_data, competency_prefix, row_num, level)

  # Convert competency prefix to lowercase for matching
  prefix_lower <- tolower(competency_prefix)

  # Handle PBLI special case
  if (startsWith(toupper(competency_prefix), "PBLI")) {
    prefix_lower <- gsub("pbli", "pbl", prefix_lower)
    cat("Debug: Special handling for PBLI in description - searching for", prefix_lower, "\n")
  }

  # Create field name pattern based on competency prefix and row number
  # For example: "pc1_r1" for PC1 row 1
  field_pattern <- paste0(prefix_lower, "_r", row_num)

  # Get the choices text from dictionary
  field_data <- rdm_dict_data %>%
    filter(grepl(field_pattern, field_name, ignore.case = TRUE)) %>%
    pull(select_choices_or_calculations)

  if(length(field_data) == 0 || is.na(field_data[1])) {
    cat("No field data found for pattern:", field_pattern, "\n")
    return("No description available")
  }

  # Split the choices text and find the selected level's description
  choices <- strsplit(field_data[1], " \\| ")[[1]]
  for(choice in choices) {
    parts <- strsplit(choice, ", ")[[1]]
    if(parts[1] == as.character(level)) {
      # Return everything after the level number and comma
      return(paste(parts[-1], collapse = ", "))
    }
  }

  # If we didn't find a matching level
  cat("No description found for level:", level, "\n")
  return("Description not found")
}

# Milestone Data Processing ----
get_milestone_data <- function(rdm_dict_data, competency_code) {
  req(rdm_dict_data, competency_code)
  cat("Debug: Looking for milestone data for", competency_code, "\n")

  # Convert competency code to lowercase for field matching
  competency_code_lower <- tolower(competency_code)

  # Handle PBLI special case - convert to pbl
  if (startsWith(toupper(competency_code), "PBLI")) {
    competency_code_lower <- gsub("pbli", "pbl", competency_code_lower)
    cat("Debug: Special handling for PBLI - searching for", competency_code_lower, "\n")
  }

  # Find all rows for this competency in the dictionary
  milestone_fields <- rdm_dict_data %>%
    filter(grepl(paste0("^", competency_code_lower, "_r\\d+$"), field_name, ignore.case = TRUE)) %>%
    arrange(field_name)

  cat("Debug: Found", nrow(milestone_fields), "milestone fields for", competency_code, "\n")

  if(nrow(milestone_fields) == 0) {
    return(NULL)
  }

  # Create a data frame for the milestone table
  data <- data.frame(
    Milestone = character(),
    Level.1 = character(),
    Level.2 = character(),
    Level.3 = character(),
    Level.4 = character(),
    Level.5 = character(),
    stringsAsFactors = FALSE
  )

  # Process each milestone row
  for(i in 1:nrow(milestone_fields)) {
    field <- milestone_fields$field_name[i]
    choices_text <- milestone_fields$select_choices_or_calculations[i]

    if(is.na(choices_text) || choices_text == "") {
      next
    }

    # Parse row number from field name (e.g., pc1_r2 -> row 2)
    row_num <- as.numeric(gsub(".*_r(\\d+)$", "\\1", field))
    milestone_name <- paste0(toupper(competency_code), " Row ", row_num)

    # Parse the choices (levels) for this row
    choices <- strsplit(choices_text, " \\| ")[[1]]
    level_texts <- rep("", 5)

    for(choice in choices) {
      parts <- strsplit(choice, ", ", fixed = TRUE)[[1]]
      if(length(parts) < 2) next

      level <- as.numeric(parts[1])
      if(is.na(level) || level < 1 || level > 5) next

      description <- paste(parts[-1], collapse = ", ")
      level_texts[level] <- description
    }

    # Add row to data frame
    data[nrow(data) + 1,] <- c(milestone_name, level_texts)
  }

  cat("Debug: Returning milestone data with", nrow(data), "rows\n")
  return(data)
}

# Function to extract and parse the ILP module debug output
extract_ilp_module_output <- function(debug_output) {
  # Initialize an empty list to store the result
  result <- list()

  # Check if the input is a string
  if (!is.character(debug_output)) {
    return(NULL)
  }

  # Split by sections
  lines <- strsplit(debug_output, "\n")[[1]]

  # Find the start of each section
  pcmk_start <- grep("^\\$pcmk$", lines)
  sbppbl_start <- grep("^\\$sbppbl$", lines)
  profics_start <- grep("^\\$profics$", lines)

  # Extract each section
  extract_section <- function(start_line, end_line = NULL) {
    section_lines <- if(is.null(end_line)) {
      lines[start_line:length(lines)]
    } else {
      lines[start_line:(end_line-1)]
    }

    section_data <- list()
    current_key <- NULL

    for (line in section_lines) {
      # Skip section header lines
      if (grepl("^\\$[a-zA-Z]+$", line)) {
        next
      }

      # Detect key-value pairs
      if (grepl("^\\$[a-zA-Z]+\\$[a-zA-Z_]+$", line)) {
        current_key <- gsub("^\\$[a-zA-Z]+\\$", "", line)
        next
      }

      # Extract value and add to the current key
      if (!is.null(current_key) && grepl("^\\[\\d+\\]\\s+\".*\"$", line)) {
        value <- gsub("^\\[\\d+\\]\\s+\"(.*)\"$", "\\1", line)
        section_data[[current_key]] <- value
      }
    }

    return(section_data)
  }

  # Extract each section
  if (length(pcmk_start) > 0) {
    end_line <- if(length(sbppbl_start) > 0) sbppbl_start else NULL
    result$pcmk <- extract_section(pcmk_start, end_line)
  }

  if (length(sbppbl_start) > 0) {
    end_line <- if(length(profics_start) > 0) profics_start else NULL
    result$sbppbl <- extract_section(sbppbl_start, end_line)
  }

  if (length(profics_start) > 0) {
    result$profics <- extract_section(profics_start)
  }

  return(result)
}


# Simplified UI function
# Modified goalSettingUI function
goalSettingUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
    .previous-goal-panel {
      background-color: #f8f9fa;
      border-left: 4px solid #0072B2;
      padding: 15px;
      margin-bottom: 20px;
    }
    .milestone-table-container {
      border: 1px solid #dee2e6;
      border-radius: 8px;
      padding: 15px;
      margin-bottom: 20px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.05);
    }
    .milestone-description-box {
      margin-top: 15px;
      padding: 15px;
      background-color: #f8f9fa;
      border: 1px solid #dee2e6;
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.05);
    }
    .milestone-description-text {
      font-size: 1.1em;
      line-height: 1.5;
      color: #2c3e50;
      padding: 10px;
      background-color: white;
      border-radius: 4px;
      margin: 10px 0;
    }
    .how-to-achieve {
      margin-top: 15px;
      padding: 10px;
      border-top: 1px solid #dee2e6;
    }
    .selection-controls {
      background-color: white;
      padding: 15px;
      border-radius: 8px;
      margin: 15px 0;
      border: 1px solid #e9ecef;
    }
    .validation-alert {
    margin-top: 15px;
    margin-bottom: 15px;
    font-weight: bold;
    color: #856404;
    background-color: #fff3cd;
    border-color: #ffeeba;
    padding: 10px;
    border-radius: 5px;
  }
  ")),
    # Container for current page content
    div(id = ns("page_container"),
        fluidRow(
          # Left column - Milestone Plot only
          column(4,
                 div(class = "milestone-plot-panel",
                     plotOutput(ns("milestone_plot"), height = "400px")
                 )
          ),
          # Right column - Goal Setting
          column(8,
                 div(class = "goal-setting-panel",
                     uiOutput(ns("current_page"))
                 )
          )
        )
    )
  )
}

goalSettingServer <- function(id, rdm_dict_data, subcompetency_maps, competency_list,
                              milestone_levels, current_milestone_data, resident_info, selected_period) {
  moduleServer(id, function(input, output, session) {
    # Get namespace function at the top level of moduleServer
    ns <- session$ns

    # Verify reactives
    stopifnot(is.reactive(current_milestone_data))
    stopifnot(is.reactive(resident_info))
    stopifnot(is.reactive(selected_period))

    # Reactive values
    current_page <- reactiveVal("pcmk")
    stored_responses <- reactiveVal(list())
    previous_goals <- reactiveVal(NULL)
    # Define submission_ready at the module level
    submission_ready <- reactiveVal(FALSE)

    # Debug observer for data loading
    observe({
      cat("Debug: rdm_dict_data:\n")
      if(!is.null(rdm_dict_data)) {
        cat("- Has", nrow(rdm_dict_data), "rows\n")
        cat("- Column names:", paste(colnames(rdm_dict_data), collapse = ", "), "\n")

        # Test if we can find milestone fields
        milestone_fields <- rdm_dict_data %>%
          filter(grepl("^(pc|mk|sbp|pbl|prof|ics)\\d+_r\\d+$", field_name, ignore.case = TRUE)) %>%
          nrow()
        cat("- Found", milestone_fields, "milestone fields\n")
      } else {
        cat("- is NULL\n")
      }
    })

    # Create choices for initial competency selection dropdowns -----
    # PC/MK dropdown
    pcmk_choices <- c()
    for(domain in c("PC", "MK")) {
      for(i in names(subcompetency_maps[[domain]])) {
        pcmk_choices <- c(pcmk_choices,
                          setNames(paste0(domain, i),
                                   paste0(domain, i, ": ", subcompetency_maps[[domain]][[i]])))
      }
    }

    # SBP/PBLI dropdown
    sbppbl_choices <- c()
    for(domain in c("SBP", "PBLI")) {
      for(i in names(subcompetency_maps[[domain]])) {
        sbppbl_choices <- c(sbppbl_choices,
                            setNames(paste0(domain, i),
                                     paste0(domain, i, ": ", subcompetency_maps[[domain]][[i]])))
      }
    }

    # PROF/ICS dropdown
    profics_choices <- c()
    for(domain in c("PROF", "ICS")) {
      for(i in names(subcompetency_maps[[domain]])) {
        profics_choices <- c(profics_choices,
                             setNames(paste0(domain, i),
                                      paste0(domain, i, ": ", subcompetency_maps[[domain]][[i]])))
      }
    }

    # Milestone Plot ----
    output$milestone_plot <- renderPlot({
      req(current_milestone_data(), resident_info(), selected_period())

      tryCatch({
        miles_plot(
          data = current_milestone_data(),
          name = resident_info(),
          period = selected_period()
        )
      }, error = function(e) {
        message(paste("Error in milestone plot:", e$message))
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No milestone data available",
                   color = "darkgray", size = 5) +
          theme_void()
      })
    })

    # Helper Functions ----
    verify_data_dict <- function(rdm_dict_data) {
      required_cols <- c("field_name", "field_label", "field_type")
      missing_cols <- setdiff(required_cols, names(rdm_dict_data))

      if(length(missing_cols) > 0) {
        warning("Missing required columns in data dictionary: ",
                paste(missing_cols, collapse = ", "))
        return(FALSE)
      }

      cat("Data dictionary structure check:\n")
      print(head(rdm_dict_data[, c("field_name", "field_label", "field_type")]))
      return(TRUE)
    }

    get_field_label <- function(field_name, default_label = NULL) {
      req(rdm_dict_data)
      label <- rdm_dict_data %>%
        filter(field_name == !!field_name) %>%
        pull(field_label)

      if(length(label) > 0) label[1] else default_label %||% field_name
    }

    get_textarea_label <- function(base_name, response) {
      field_name <- if(response == "1") {
        paste0("review_q2_", base_name)
      } else {
        paste0("review_q_", base_name)
      }

      req(rdm_dict_data)
      cat("Looking for field name:", field_name, "\n")

      default_label <- if(response == "1") {
        "If you reached your goal, why do you think you were able to achieve it?"
      } else {
        "If you did not reach your goal, what reason(s) prevented you from achieving this?"
      }

      get_field_label(field_name, default_label)
    }

    # PC/MK validation function
    validate_pcmk_page <- function(input) {
      # Required fields on PC/MK page
      has_prior_goal_response <- !is.null(input$prior_goal_pcmk)

      # If they answered the prior goal question
      if (has_prior_goal_response) {
        has_review_response <- if(input$prior_goal_pcmk == "1") {
          !is.null(input$review_q2_pcmk) && nchar(trimws(input$review_q2_pcmk)) > 0
        } else {
          !is.null(input$review_q_pcmk) && nchar(trimws(input$review_q_pcmk)) > 0
        }
      } else {
        has_review_response <- TRUE  # Not applicable yet
      }

      # Check if they've selected a subcompetency
      has_subcompetency <- !is.null(input$pcmk_subcompetency) && input$pcmk_subcompetency != ""

      # Check if they've selected a milestone
      has_milestone_selection <- has_subcompetency &&
        !is.null(input$pcmk_row_select) &&
        !is.null(input$pcmk_level_select)

      # Check if they've described how they'll achieve it
      has_achievement_plan <- !is.null(input$how_pcmk) && nchar(trimws(input$how_pcmk)) > 0

      # All validations must pass
      return(has_prior_goal_response && has_review_response &&
               has_milestone_selection && has_achievement_plan)
    }

    # SBP/PBLI validation function
    validate_sbppbl_page <- function(input) {
      # Required fields on SBP/PBLI page
      has_prior_goal_response <- !is.null(input$prior_goal_sbppbl)

      # If they answered the prior goal question
      if (has_prior_goal_response) {
        has_review_response <- if(input$prior_goal_sbppbl == "1") {
          !is.null(input$review_q2_sbppbl) && nchar(trimws(input$review_q2_sbppbl)) > 0
        } else {
          !is.null(input$review_q_sbppbl) && nchar(trimws(input$review_q_sbppbl)) > 0
        }
      } else {
        has_review_response <- TRUE  # Not applicable yet
      }

      # Check if they've selected a subcompetency
      has_subcompetency <- !is.null(input$sbppbl_subcompetency) && input$sbppbl_subcompetency != ""

      # Check if they've selected a milestone
      has_milestone_selection <- has_subcompetency &&
        !is.null(input$sbppbl_row_select) &&
        !is.null(input$sbppbl_level_select)

      # Check if they've described how they'll achieve it
      has_achievement_plan <- !is.null(input$how_sbppbl) && nchar(trimws(input$how_sbppbl)) > 0

      # All validations must pass
      return(has_prior_goal_response && has_review_response &&
               has_milestone_selection && has_achievement_plan)
    }

    # PROF/ICS validation function
    validate_profics_page <- function(input) {
      # Required fields on PROF/ICS page
      has_prior_goal_response <- !is.null(input$prior_goal_profics)

      # If they answered the prior goal question
      if (has_prior_goal_response) {
        has_review_response <- if(input$prior_goal_profics == "1") {
          !is.null(input$review_q2_profics) && nchar(trimws(input$review_q2_profics)) > 0
        } else {
          !is.null(input$review_q_profics) && nchar(trimws(input$review_q_profics)) > 0
        }
      } else {
        has_review_response <- TRUE  # Not applicable yet
      }

      # Check if they've selected a subcompetency
      has_subcompetency <- !is.null(input$profics_subcompetency) && input$profics_subcompetency != ""

      # Check if they've selected a milestone
      has_milestone_selection <- has_subcompetency &&
        !is.null(input$profics_row_select) &&
        !is.null(input$profics_level_select)

      # Check if they've described how they'll achieve it
      has_achievement_plan <- !is.null(input$how_profics) && nchar(trimws(input$how_profics)) > 0

      # All validations must pass
      return(has_prior_goal_response && has_review_response &&
               has_milestone_selection && has_achievement_plan)
    }

    # Selected Competency Code Processing ----
    selected_pcmk_code <- reactive({
      input$pcmk_subcompetency
    })

    selected_sbppbl_code <- reactive({
      input$sbppbl_subcompetency
    })

    selected_profics_code <- reactive({
      input$profics_subcompetency
    })

    pcmk_milestone_data <- reactive({
      req(selected_pcmk_code())
      cat("Debug: Getting milestone data for", selected_pcmk_code(), "with column names format 'Level.X'\n")
      get_milestone_data(rdm_dict_data, selected_pcmk_code())
    })

    sbppbl_milestone_data <- reactive({
      req(selected_sbppbl_code())
      cat("Debug: Getting milestone data for", selected_sbppbl_code(), "with column names format 'Level.X'\n")
      get_milestone_data(rdm_dict_data, selected_sbppbl_code())
    })

    profics_milestone_data <- reactive({
      req(selected_profics_code())
      cat("Debug: Getting milestone data for", selected_profics_code(), "with column names format 'Level.X'\n")
      get_milestone_data(rdm_dict_data, selected_profics_code())
    })

    # Row Selection Processing ----
    # Fix the row selection observers
    observe({
      req(pcmk_milestone_data())
      if(nrow(pcmk_milestone_data()) > 0) {
        rows <- seq_len(nrow(pcmk_milestone_data()))
        choices <- setNames(rows, pcmk_milestone_data()$Milestone)
        updateSelectInput(session, "pcmk_row_select", choices = choices, selected = rows[1])
      }
    })

    observe({
      req(sbppbl_milestone_data())
      if(nrow(sbppbl_milestone_data()) > 0) {
        rows <- seq_len(nrow(sbppbl_milestone_data()))
        choices <- setNames(rows, sbppbl_milestone_data()$Milestone)
        updateSelectInput(session, "sbppbl_row_select", choices = choices, selected = rows[1])
      }
    })

    observe({
      req(profics_milestone_data())
      if(nrow(profics_milestone_data()) > 0) {
        rows <- seq_len(nrow(profics_milestone_data()))
        choices <- setNames(rows, profics_milestone_data()$Milestone)
        updateSelectInput(session, "profics_row_select", choices = choices, selected = rows[1])
      }
    })

    # Page Definitions with Button Validation ----
    pcmk_page <- function() {
      ns <- session$ns
      div(
        h4("Goal: Patient Care / Medical Knowledge"),
        # Previous Goal Display (Add this line)
        uiOutput(session$ns("previous_goal_pcmk")),
        # Review Section
        wellPanel(
          class = "previous-goal-panel",
          h5("Previous Goal Review"),
          htmlOutput(session$ns("prior_goal_pcmk_radio")),
          uiOutput(session$ns("pcmk_textarea"))
        ),
        # New Goal Section
        wellPanel(
          h4("Set New PC/MK Goal"),
          # Subcompetency selection
          div(class = "selection-controls",
              selectInput(session$ns("pcmk_subcompetency"),
                          "Select Subcompetency:",
                          choices = pcmk_choices,
                          width = "100%")
          ),
          # Milestone table will show immediately
          uiOutput(session$ns("pcmk_milestone_table"))
        ),

        # Required fields notice
        uiOutput(session$ns("pcmk_validation_message")),
        div(
          style = "text-align: right;",
          actionButton(session$ns("next_pcmk"), "Next →", class = "btn-primary")
        )
      )
    }

    # Fix the nested sbppbl_page function
    sbppbl_page <- function() {
      ns <- session$ns
      div(
        h4("Goal: Systems-Based Practice / Practice-Based Learning and Improvement"),
        # Previous Goal Display
        uiOutput(session$ns("previous_goal_sbppbl")),
        # Review Section
        wellPanel(
          class = "previous-goal-panel",
          h5("Previous Goal Review"),
          htmlOutput(session$ns("prior_goal_sbppbl_radio")),
          uiOutput(session$ns("sbppbl_textarea"))
        ),
        wellPanel(
          h4("Set New SBP/PBLI Goal"),
          # Subcompetency selection
          div(class = "selection-controls",
              selectInput(session$ns("sbppbl_subcompetency"),
                          "Select Subcompetency:",
                          choices = sbppbl_choices,
                          width = "100%")
          ),
          # Milestone table will show immediately
          uiOutput(session$ns("sbppbl_milestone_table"))
        ),
        # Required fields notice
        uiOutput(session$ns("sbppbl_validation_message")),
        div(
          style = "text-align: right;",
          actionButton(session$ns("prev_sbppbl"), "← Previous", class = "btn-secondary me-2"),
          actionButton(session$ns("next_sbppbl"), "Next →", class = "btn-primary")
        )
      )
    }

    profics_page <- function() {
      ns <- session$ns
      div(
        h4("Goal: Professionalism / Interpersonal and Communication Skills"),
        # Previous Goal Display (Add this line)
        uiOutput(session$ns("previous_goal_profics")),
        # Review Section
        wellPanel(
          class = "previous-goal-panel",
          h5("Previous Goal Review"),
          htmlOutput(session$ns("prior_goal_profics_radio")),
          uiOutput(session$ns("profics_textarea"))
        ),
        wellPanel(
          h4("Set New PROF/ICS Goal"),
          # Subcompetency selection
          div(class = "selection-controls",
              selectInput(session$ns("profics_subcompetency"),
                          "Select Subcompetency:",
                          choices = profics_choices,
                          width = "100%")
          ),
          # Milestone table will show immediately
          uiOutput(session$ns("profics_milestone_table"))
        ),
        # Required fields notice
        uiOutput(session$ns("profics_validation_message")),
        div(
          style = "text-align: right;",
          actionButton(session$ns("prev_profics"), "← Previous", class = "btn-secondary me-2"),
          actionButton(session$ns("submit_goals"), "Submit All Goals", class = "btn-success")
        )
      )
    }

    # Radio Button Renderers ----
    output$prior_goal_pcmk_radio <- renderUI({
      req(rdm_dict_data)
      verify_data_dict(rdm_dict_data)

      label <- get_field_label(
        "prior_goal_pcmk",
        "Did you reach your previous milestone goal for Patient Care/Medical Knowledge?"
      )

      safe_radio_buttons_empty(
        inputId = session$ns("prior_goal_pcmk"),
        label = label,
        choices = list("Yes" = "1", "No" = "0"),
        inline = TRUE
      )
    })

    output$prior_goal_sbppbl_radio <- renderUI({
      req(rdm_dict_data)
      label <- get_field_label(
        "prior_goal_sbppbl",
        "Did you reach your previous milestone goal for System Based Practice or Problem Based Learning/Improvement?"
      )
      safe_radio_buttons_empty(
        inputId = session$ns("prior_goal_sbppbl"),
        label = label,
        choices = list("Yes" = "1", "No" = "0"),
        inline = TRUE
      )
    })

    output$prior_goal_profics_radio <- renderUI({
      req(rdm_dict_data)
      label <- get_field_label(
        "prior_goal_profics",
        "Did you reach your previous milestone goal for Professionalism or Interpersonal/Communication Skills?"
      )
      safe_radio_buttons_empty(
        inputId = session$ns("prior_goal_profics"),
        label = label,
        choices = list("Yes" = "1", "No" = "0"),
        inline = TRUE
      )
    })

    # Textarea Renderers ----
    output$pcmk_textarea <- renderUI({
      req(input$prior_goal_pcmk)
      label <- get_textarea_label("pcmk", input$prior_goal_pcmk)
      textAreaInput(
        inputId = session$ns(if(input$prior_goal_pcmk == "1") "review_q2_pcmk" else "review_q_pcmk"),
        label = h5(label),
        width = "100%",
        rows = 5
      )
    })

    output$sbppbl_textarea <- renderUI({
      req(input$prior_goal_sbppbl)
      label <- get_textarea_label("sbppbl", input$prior_goal_sbppbl)
      textAreaInput(
        inputId = session$ns(if(input$prior_goal_sbppbl == "1") "review_q2_sbppbl" else "review_q_sbppbl"),
        label = h5(label),
        width = "100%",
        rows = 5
      )
    })

    output$profics_textarea <- renderUI({
      req(input$prior_goal_profics)
      label <- get_textarea_label("profics", input$prior_goal_profics)
      textAreaInput(
        inputId = session$ns(if(input$prior_goal_profics == "1") "review_q2_profics" else "review_q_profics"),
        label = h5(label),
        width = "100%",
        rows = 5
      )
    })

    # Previous Goals Rendering ----
    render_previous_goal <- function(domain_key, domain_name) {
      renderUI({
        req(previous_goals())
        prev_goal_data <- previous_goals()
        goal <- extract_previous_goal_by_domain(prev_goal_data, domain_key)

        if(is.null(goal)) return(NULL)

        wellPanel(
          class = "alert alert-info",
          h5(paste("Your Milestone goal for", domain_name, "from your last review was:")),
          p(goal$value, style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;"),
          hr()
        )
      })
    }

    output$previous_goal_pcmk <- render_previous_goal("pcmk", "Patient Care or Medical Knowledge")
    output$previous_goal_sbppbl <- render_previous_goal("sbppbl", "Systems-Based Practice or Practice-Based Learning and Improvement")
    output$previous_goal_profics <- render_previous_goal("profics", "Professionalism or Interpersonal and Communication Skills")

    # Milestone Table Renderers ----
    output$pcmk_milestone_table <- renderUI({
      req(pcmk_milestone_data())

      data <- pcmk_milestone_data()
      if(is.null(data) || nrow(data) == 0) {
        return(div(
          class = "alert alert-warning",
          "No milestone data available for this subcompetency."
        ))
      }

      display_data <- data

      # Replace Level.X column names with friendly names
      level_cols <- grep("^Level\\.", names(display_data), value = TRUE)
      new_col_names <- names(display_data)

      for (col in level_cols) {
        # Extract the level number from the column name
        level_num <- as.numeric(gsub("Level\\.", "", col))
        # Get the friendly name from milestone_levels
        friendly_name <- milestone_levels[[as.character(level_num)]]
        # Replace the column name
        new_col_names[which(names(display_data) == col)] <- friendly_name
      }

      # Set the new column names
      names(display_data) <- new_col_names


      tagList(
        div(class = "milestone-table-container",
            h5(paste(input$pcmk_subcompetency, "Milestone Levels"),
               style = "color: #0072B2; border-bottom: 2px solid #0072B2; padding-bottom: 8px;"),
            div(
              style = "overflow-x: auto;",
              renderTable({
                display_data
              },
              sanitize.text.function = function(x) x,
              align = 'c',
              bordered = TRUE,
              width = "100%",
              spacing = "s",
              striped = TRUE
              )
            ),
            div(class = "selection-controls",
                fluidRow(
                  column(6,
                         selectInput(session$ns("pcmk_row_select"),
                                     "Select Milestone Row:",
                                     choices = NULL,
                                     width = "100%")
                  ),
                  column(6,
                         selectInput(session$ns("pcmk_level_select"),
                                     "Target Level:",
                                     choices = setNames(1:5, paste0(1:5, ": ", unlist(milestone_levels))),
                                     width = "100%")
                  )
                )
            ),
            # Goal display area
            uiOutput(session$ns("pcmk_goal_display"))
        )
      )
    })

    # Add a separate renderer for the goal display
    output$pcmk_goal_display <- renderUI({
      req(input$pcmk_row_select, input$pcmk_level_select)

      # Use directly hardcoded column names since we know them
      col_name <- paste0("Level.", input$pcmk_level_select)

      # Get milestone data
      data <- pcmk_milestone_data()
      if(is.null(data)) return(NULL)

      # Get row index
      row_idx <- as.numeric(input$pcmk_row_select)
      if(is.na(row_idx) || row_idx < 1 || row_idx > nrow(data)) return(NULL)

      # Get the description text
      if(!(col_name %in% names(data))) return(NULL)
      goal_text <- data[row_idx, col_name]

      # Create the UI for the goal
      div(class = "milestone-description-box",
          strong("Milestone goal to achieve in the next 6 months:"),
          div(class = "milestone-description-text",
              p(goal_text, style = "background-color: #f8f9fa; padding: 10px; border-left: 3px solid #0072B2;")
          ),
          div(class = "how-to-achieve",
              strong("How will you achieve this milestone?"),
              textAreaInput(
                session$ns("how_pcmk"),
                label = NULL,
                width = "100%",
                rows = 3,
                placeholder = "Describe your plan to achieve this milestone..."
              )
          )
      )
    })

    # SBP/PBLI Section ------------------------------------------------------
    # Milestone table for SBP/PBLI
    output$sbppbl_milestone_table <- renderUI({
      req(sbppbl_milestone_data())

      data <- sbppbl_milestone_data()
      if(is.null(data) || nrow(data) == 0) {
        return(div(
          class = "alert alert-warning",
          "No milestone data available for this subcompetency."
        ))
      }

      display_data <- data

      # Replace Level.X column names with friendly names
      level_cols <- grep("^Level\\.", names(display_data), value = TRUE)
      new_col_names <- names(display_data)

      for (col in level_cols) {
        # Extract the level number from the column name
        level_num <- as.numeric(gsub("Level\\.", "", col))
        # Get the friendly name from milestone_levels
        friendly_name <- milestone_levels[[as.character(level_num)]]
        # Replace the column name
        new_col_names[which(names(display_data) == col)] <- friendly_name
      }

      # Set the new column names
      names(display_data) <- new_col_names


      tagList(
        div(class = "milestone-table-container",
            h5(paste(input$sbppbl_subcompetency, "Milestone Levels"),
               style = "color: #0072B2; border-bottom: 2px solid #0072B2; padding-bottom: 8px;"),
            div(
              style = "overflow-x: auto;",
              renderTable({
                display_data
              },
              sanitize.text.function = function(x) x,
              align = 'c',
              bordered = TRUE,
              width = "100%",
              spacing = "s",
              striped = TRUE
              )
            ),
            div(class = "selection-controls",
                fluidRow(
                  column(6,
                         selectInput(session$ns("sbppbl_row_select"),
                                     "Select Milestone Row:",
                                     choices = NULL,
                                     width = "100%")
                  ),
                  column(6,
                         selectInput(session$ns("sbppbl_level_select"),
                                     "Target Level:",
                                     choices = setNames(1:5, paste0(1:5, ": ", unlist(milestone_levels))),
                                     width = "100%")
                  )
                )
            ),
            # Goal display area
            uiOutput(session$ns("sbppbl_goal_display"))
        )
      )
    })

    # Goal display for SBP/PBLI
    output$sbppbl_goal_display <- renderUI({
      req(input$sbppbl_row_select, input$sbppbl_level_select)

      # Use directly hardcoded column names
      col_name <- paste0("Level.", input$sbppbl_level_select)

      # Get milestone data
      data <- sbppbl_milestone_data()
      if(is.null(data)) return(NULL)

      # Get row index
      row_idx <- as.numeric(input$sbppbl_row_select)
      if(is.na(row_idx) || row_idx < 1 || row_idx > nrow(data)) return(NULL)

      # Get the description text
      if(!(col_name %in% names(data))) return(NULL)
      goal_text <- data[row_idx, col_name]

      # Create the UI for the goal
      div(class = "milestone-description-box",
          strong("Milestone goal to achieve in the next 6 months:"),
          div(class = "milestone-description-text",
              p(goal_text, style = "background-color: #f8f9fa; padding: 10px; border-left: 3px solid #0072B2;")
          ),
          div(class = "how-to-achieve",
              strong("How will you achieve this milestone?"),
              textAreaInput(
                session$ns("how_sbppbl"),
                label = NULL,
                width = "100%",
                rows = 3,
                placeholder = "Describe your plan to achieve this milestone..."
              )
          )
      )
    })

    # PROF/ICS Section ------------------------------------------------------
    # Milestone table for PROF/ICS
    output$profics_milestone_table <- renderUI({
      req(profics_milestone_data())

      data <- profics_milestone_data()
      if(is.null(data) || nrow(data) == 0) {
        return(div(
          class = "alert alert-warning",
          "No milestone data available for this subcompetency."
        ))
      }

      display_data <- data

      # Replace Level.X column names with friendly names
      level_cols <- grep("^Level\\.", names(display_data), value = TRUE)
      new_col_names <- names(display_data)

      for (col in level_cols) {
        # Extract the level number from the column name
        level_num <- as.numeric(gsub("Level\\.", "", col))
        # Get the friendly name from milestone_levels
        friendly_name <- milestone_levels[[as.character(level_num)]]
        # Replace the column name
        new_col_names[which(names(display_data) == col)] <- friendly_name
      }

      # Set the new column names
      names(display_data) <- new_col_names


      tagList(
        div(class = "milestone-table-container",
            h5(paste(input$profics_subcompetency, "Milestone Levels"),
               style = "color: #0072B2; border-bottom: 2px solid #0072B2; padding-bottom: 8px;"),
            div(
              style = "overflow-x: auto;",
              renderTable({
                display_data
              },
              sanitize.text.function = function(x) x,
              align = 'c',
              bordered = TRUE,
              width = "100%",
              spacing = "s",
              striped = TRUE
              )
            ),
            div(class = "selection-controls",
                fluidRow(
                  column(6,
                         selectInput(session$ns("profics_row_select"),
                                     "Select Milestone Row:",
                                     choices = NULL,
                                     width = "100%")
                  ),
                  column(6,
                         selectInput(session$ns("profics_level_select"),
                                     "Target Level:",
                                     choices = setNames(1:5, paste0(1:5, ": ", unlist(milestone_levels))),
                                     width = "100%")
                  )
                )
            ),
            # Goal display area
            uiOutput(session$ns("profics_goal_display"))
        )
      )
    })

    # Goal display for PROF/ICS
    output$profics_goal_display <- renderUI({
      req(input$profics_row_select, input$profics_level_select)

      # Use directly hardcoded column names
      col_name <- paste0("Level.", input$profics_level_select)

      # Get milestone data
      data <- profics_milestone_data()
      if(is.null(data)) return(NULL)

      # Get row index
      row_idx <- as.numeric(input$profics_row_select)
      if(is.na(row_idx) || row_idx < 1 || row_idx > nrow(data)) return(NULL)

      # Get the description text
      if(!(col_name %in% names(data))) return(NULL)
      goal_text <- data[row_idx, col_name]

      # Create the UI for the goal
      div(class = "milestone-description-box",
          strong("Milestone goal to achieve in the next 6 months:"),
          div(class = "milestone-description-text",
              p(goal_text, style = "background-color: #f8f9fa; padding: 10px; border-left: 3px solid #0072B2;")
          ),
          div(class = "how-to-achieve",
              strong("How will you achieve this milestone?"),
              textAreaInput(
                session$ns("how_profics"),
                label = NULL,
                width = "100%",
                rows = 3,
                placeholder = "Describe your plan to achieve this milestone..."
              )
          )
      )
    })

    # Navigation and Page Rendering ----
    output$current_page <- renderUI({
      page <- current_page()
      switch(page,
             "pcmk" = pcmk_page(),
             "sbppbl" = sbppbl_page(),
             "profics" = profics_page())
    })

    observeEvent(input$next_pcmk, {
      responses <- stored_responses()
      responses$pcmk <- list(
        had_prior_goal = input$prior_goal_pcmk,
        review = if(input$prior_goal_pcmk == "1") input$review_q2_pcmk else input$review_q_pcmk,
        subcompetency = input$pcmk_subcompetency,
        selected_row = input$pcmk_row_select,
        selected_level = input$pcmk_level_select,
        milestone_description = if(!is.null(pcmk_milestone_data()) &&
                                   !is.na(input$pcmk_row_select) &&
                                   !is.na(input$pcmk_level_select)) {
          pcmk_milestone_data()[as.numeric(input$pcmk_row_select), paste0("Level.", input$pcmk_level_select)]
        } else {
          NA
        },
        how_to_achieve = input$how_pcmk
      )
      stored_responses(responses)

      runjs(sprintf("$('#%s').fadeOut(300, function() { Shiny.setInputValue('%s', 'sbppbl'); $(this).fadeIn(300); });",
                    session$ns("page_container"), session$ns("page_transition")))
      current_page("sbppbl")
    })

    observeEvent(input$next_sbppbl, {
      responses <- stored_responses()
      responses$sbppbl <- list(
        had_prior_goal = input$prior_goal_sbppbl,
        review = if(input$prior_goal_sbppbl == "1") input$review_q2_sbppbl else input$review_q_sbppbl,
        subcompetency = input$sbppbl_subcompetency,
        selected_row = input$sbppbl_row_select,
        selected_level = input$sbppbl_level_select,
        milestone_description = if(!is.null(sbppbl_milestone_data()) &&
                                   !is.na(input$sbppbl_row_select) &&
                                   !is.na(input$sbppbl_level_select)) {
          sbppbl_milestone_data()[as.numeric(input$sbppbl_row_select), paste0("Level.", input$sbppbl_level_select)]
        } else {
          NA
        },
        how_to_achieve = input$how_sbppbl
      )
      stored_responses(responses)

      runjs(sprintf("$('#%s').fadeOut(300, function() { Shiny.setInputValue('%s', 'profics'); $(this).fadeIn(300); });",
                    session$ns("page_container"), session$ns("page_transition")))
      current_page("profics")
    })

    observeEvent(input$prev_sbppbl, {
      runjs(sprintf("$('#%s').fadeOut(300, function() { Shiny.setInputValue('%s', 'pcmk'); $(this).fadeIn(300); });",
                    session$ns("page_container"), session$ns("page_transition")))
      current_page("pcmk")
    })

    observeEvent(input$prev_profics, {
      runjs(sprintf("$('#%s').fadeOut(300, function() { Shiny.setInputValue('%s', 'sbppbl'); $(this).fadeIn(300); });",
                    session$ns("page_container"), session$ns("page_transition")))
      current_page("sbppbl")
    })

    # Validation message outputs
    output$pcmk_validation_message <- renderUI({
      is_valid <- validate_pcmk_page(input)

      if (!is_valid) {
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          "Please complete all required fields before proceeding."
        )
      }
    })

    output$sbppbl_validation_message <- renderUI({
      is_valid <- validate_sbppbl_page(input)

      if (!is_valid) {
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          "Please complete all required fields before proceeding."
        )
      }
    })

    output$profics_validation_message <- renderUI({
      is_valid <- validate_profics_page(input)

      if (!is_valid) {
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          "Please complete all required fields before proceeding."
        )
      }
    })

    # Button validation observers
    observe({
      is_valid <- validate_pcmk_page(input)
      shinyjs::toggleState(id = session$ns("next_pcmk"), condition = is_valid)
    })

    observe({
      is_valid <- validate_sbppbl_page(input)
      shinyjs::toggleState(id = session$ns("next_sbppbl"), condition = is_valid)
    })

    observe({
      is_valid <- validate_profics_page(input)
      shinyjs::toggleState(id = session$ns("submit_goals"), condition = is_valid)
    })

    # When the user clicks “Submit All Goals” inside the module…
    observeEvent(input$submit_goals, {
      # 1) grab what’s already in there
      resp <- stored_responses()

      # 2) store PROF/ICS exactly like you did for the others
      resp$profics <- list(
        had_prior_goal       = input$prior_goal_profics,
        review               = if (input$prior_goal_profics=="1") input$review_q2_profics else input$review_q_profics,
        subcompetency        = input$profics_subcompetency,
        selected_row         = input$profics_row_select,
        selected_level       = input$profics_level_select,
        milestone_description= profics_milestone_data()[ as.integer(input$profics_row_select),
                                                         paste0("Level.", input$profics_level_select) ],
        how_to_achieve       = input$how_profics
      )

      # 3) write it back
      stored_responses(resp)

      # 4) now signal “ready” to the outer app
      submission_ready(TRUE)
    })

    # Expose exactly the bits the outer app needs, and only once
    return(list(
      current_page        = current_page,
      get_responses       = reactive({ stored_responses() }),
      set_previous_goals  = function(data) { previous_goals(data) },
      submission_ready    = reactive({ submission_ready() }),
      reset_submission    = function() { submission_ready(FALSE) }
    ))
  })
}









# Helper function to determine if program data exists for a period
has_program_data <- function(period) {
  return(period != "Entering Residency")
}

get_median_data <- function(s_miles_data, current_period) {
  # This function should be called within a reactive context
  # Extract median data for the current period
  if (is.null(s_miles_data) || !is.data.frame(s_miles_data)) {
    return(NULL)
  }

  median_data <- s_miles_data[s_miles_data$name == "Median" & s_miles_data$period == current_period, ]

  # If no median data exists for this period, return NULL
  if (nrow(median_data) == 0) {
    return(NULL)
  }
  return(median_data)
}

prepare_milestone_plot_data <- function(current_data, median_data) {
  # If median data exists, combine with current data
  if (!is.null(median_data) && nrow(median_data) > 0) {
    # Ensure consistent columns between the two
    common_cols <- intersect(names(current_data), names(median_data))
    plot_data <- bind_rows(
      current_data[, common_cols],
      median_data[, common_cols]
    )
  } else {
    # If no median data, just use current data
    plot_data <- current_data
  }

  return(plot_data)
}





