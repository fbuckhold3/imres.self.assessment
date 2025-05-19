server <- function(input, output, session) {
  redcap_url <- "https://redcapsurvey.slu.edu/api/"

  observe({
    # Debug the period selection
    req(selected_period())
    message("DEBUG: selected_period() currently returns '", selected_period(), "'")
  })

  # Show loading notification
  showNotification("Loading data... may take a sec", type = "message", duration = NULL, id = "loading")

  # Load data immediately at server start
  app_data <- ensure_data_loaded()

  ilp_data <- reactive({
    req(app_data$ilp_data)
    app_data$ilp_data
  })

  # Check if data loaded successfully
  if (!is.null(app_data) && !is.null(app_data$resident_data)) {
    # Create variables
    resident_data <- app_data$resident_data
    rdm_dict <- app_data$rdm_dict
    ass_dict <- app_data$ass_dict
    schol_data <- app_data$schol_data
    miles <- app_data$miles
    p_miles <- app_data$p_miles
    s_miles <- app_data$s_miles
    eval_token <- app_data$eval_token
    rdm_token <- app_data$rdm_token
    fac_token <- app_data$fac_token

    # Remove loading notification and show success
    removeNotification(id = "loading")
    showNotification("Data loaded successfully", type = "message", duration = 3)

    # Set a reactive to indicate data is loaded
    data_loaded <- reactiveVal(TRUE)

    # Debug output
    cat("DEBUG - After loading data:\n")
    cat("DEBUG - schol_data class:", class(schol_data), "\n")
    if (is.data.frame(schol_data)) {
      cat("DEBUG - schol_data dims:", nrow(schol_data), "x", ncol(schol_data), "\n")
    }
  } else {
    # Data loading failed
    removeNotification(id = "loading")
    showNotification(
      "Failed to load resident data. Please check your tokens and configuration.",
      type = "error",
      duration = NULL
    )
    data_loaded <- reactiveVal(FALSE)
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

  entering_fields <- c(
    "s_e_fac_assist","s_e_fac_member","s_e_fac_email",
    "s_e_ume_goal1","s_e_ume_goal2","s_e_ume_goal3",
    paste0("s_e_prep_", 1:17),
    "s_e_topic_sel","s_e_topic_oth",
    "s_e_learn_style","s_e_learn_oth",
    "s_e_ume_concern",
    "s_e_career_path","s_e_career_oth",
    "s_e_fellow","s_e_track","s_e_track_type",
    "s_e_discussion"
  )

  # Exactly the Graduating fields
  graduating_fields <- c(
    "s_e_discussion",
    "s_e_grad_next","s_e_grad_next_other","s_e_grad_fellow","s_e_grad_fellow_oth",
    "s_e_grad_where","s_e_grad_loc","s_e_grad_loc_other",
    "s_e_grad_fellow_loc","s_e_grad_fellow_loc_else",
    "s_e_grad_email","s_e_grad_phone"
  )

  # Everything‐else (e.g. "Mid Intern")
  other_fields <- c(
    "s_e_topic_sel","s_e_topic_oth",
    "s_e_learn_style","s_e_learn_oth",
    "s_e_career_path","s_e_career_oth",
    "s_e_fellow",
    "s_e_track", "s_e_track_type",
    "s_e_discussion",
    "s_e_step3","s_e_step3_contact","s_e_step3_date_set","s_e_step3_date",
    "s_e_board_concern","s_e_board_help","s_e_board_discu",
    "s_e_mksap_comp"
  )

  # Field set for program feedback (card 3)
  program_feedback_fields <- c(
    "s_e_prog_plus", "s_e_prog_delta", "s_e_progconf", "s_e_progfeed"
  )

  # Your existing access_code reactive
  access_code <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$code) && nzchar(query$code)) trimws(query$code)
    else trimws(input$access_code_input)
  })

  observeEvent(input$validate_code, {
    req(access_code())
    is_valid <- access_code() %in% resident_data$access_code
    if(is_valid) {
      # Hide access screen completely
      shinyjs::hide("access_code_screen")

      # Show main content
      shinyjs::show("main_app_content")

      # Make sure intro_card is hidden
      shinyjs::hide("intro_card")

      # Make sure period selection is visible
      shinyjs::show("period_selection_card")
      shinyjs::show("period_help_text")

      # Update validation state
      updateTextInput(session, "is_valid_code", value = TRUE)
    } else {
      # Update validation state
      updateTextInput(session, "is_valid_code", value = FALSE)
    }
  })

  # Resident info reactive
  # Resident info reactive
  resident_info <- reactive({
    req(data_loaded())
    validate(need(exists("resident_data") && !is.null(resident_data),
                  "Resident data not available"))

    req(access_code())
    df <- resident_data %>% filter(access_code == access_code())
    if (nrow(df) == 0) return(NULL)

    # Make sure the name is a character string
    name <- as.character(df$name[1])
    if (is.null(name) || is.na(name)) {
      name <- "Unknown Resident"  # Provide a default value
    }
    return(name)
  })

  # Coach info reactive
  coach_info <- reactive({
    req(access_code())
    df <- resident_data %>% filter(access_code == access_code())
    if (nrow(df) == 0) return(NULL)

    # Make sure the coach is a character string
    coach <- as.character(df$coach[1])
    if (is.null(coach) || is.na(coach)) {
      coach <- "Unknown Coach"  # Provide a default value
    }
    return(coach)
  })

  output$resident_name <- renderText({ req(resident_info()); paste("Self Evaluation for:", resident_info()) })
  output$coach_name <- renderText({ req(coach_info()); paste("Coach is Dr:", coach_info()) })

  # record_id reactive
  record_id <- reactive({
    req(access_code())
    df <- resident_data %>% filter(access_code == access_code())
    if (nrow(df) == 0) return(NULL)
    df$record_id[1]
  })

  # Add validation observer
  observe({
    req(record_id())
    validate(need(!is.null(record_id()) && length(record_id()) > 0, "No record_id found!"))
  })

  # ── Period Selection ───────────────────────────────────────────────
  selected_period <- mod_miles_select_server("period_select")

  responses <- reactiveValues()

  transition <- function(from, to) {
    shinyjs::hide(from)
    shinyjs::show(to)
  }

  observe({
    purrr::walk(
      c("intro_card","section1_card","section2_card","section3_card",
        "section4_card","section5_card","section6_card","completion_card"),
      shinyjs::hide
    )
  })

  session$onFlushed(function() {
    updateRadioButtons(session, "hs_mo", selected = character(0))
    updateRadioButtons(session, "college_mo", selected = character(0))
    updateRadioButtons(session, "med_mo", selected = character(0))
  }, once = TRUE)

  output$plus_delta_table <- DT::renderDT({
    req(resident_info())
    plus_delta_data <- generate_p_d(resident_data, resident_info())
    create_styled_dt(plus_delta_data, caption = "Plus/Delta Feedback")
  })

  showEvaluationModal <- function() {
    showModal(modalDialog(
      title = "Your Evaluations (Plus/Delta)",
      DT::dataTableOutput("plus_delta_table"),
      footer = modalButton("Close"),
      easyClose = TRUE,
      size = "l",
      class = "big-modal"
    ))
  }

  rv <- reactiveValues(
    flow_path = NULL,
    other_value = 0,
    # ... other reactive values
    current_milestone_data = NULL
  )

  # Render UI outputs
  output$card2UI <- renderUI({
    req(selected_period())
    render_card2_ui(
      selected_period(),  # The period the user selected
      rdm_dict,           # Your data dictionary
      entering_fields,    # Fields defined above
      graduating_fields,  # Fields defined above
      other_fields        # Fields defined above
    )
  })

  # Function to store the program feedback data
  store_program_feedback <- function() {
    feedback_data <- list(
      s_e_prog_plus = input$s_e_prog_plus,
      s_e_prog_delta = input$s_e_prog_delta,
      s_e_progconf = input$s_e_progconf,
      s_e_progfeed = input$s_e_progfeed
    )

    # Store each field in the responses reactiveValues
    for (field in names(feedback_data)) {
      responses[[field]] <- feedback_data[[field]]
    }
  }

  output$card3UI <- renderUI({
    render_card3_ui(rdm_dict)
  })

  output$scholarship_module_ui <- renderUI({
    req(selected_period())
    if (selected_period() != "Entering Residency")
      scholarship_module_ui("schol", rdm_dict)  # Pass rdm_dict explicitly
    else
      div(
        p("Scholarship tracking begins after entering residency."),
        actionButton("schol_skip", "Continue to Next Section", class = "btn-primary")
      )
  })

  scholarship_data <- reactive({
    # Return the data or NULL if not available
    if (exists("schol_data") && !is.null(schol_data)) {
      return(schol_data)  # Make sure to return the value
    } else {
      return(NULL)  # Make sure to return NULL
    }
  })

  # Scholarship module server
  scholarship_module_server(
    "schol",
    rdm_dict = rdm_dict,
    record_id = reactive({
      req(resident_info())
      fetch_record_id(resident_info(), resident_data, redcap_url, rdm_token)
    }),
    schol_data = scholarship_data,  # We're passing the reactive, not the result of calling it
    token = rdm_token
  )
  # Various button observers for navigation
  observeEvent(input[["schol-next_btn"]], {
    transition("section4_card", "section5_card")
  })

  observeEvent(input$schol_skip, {
    transition("section4_card", "section5_card")
  })

  observeEvent(input$open_modal, {
    req(resident_info())
    showEvaluationModal()
  })

  observeEvent(input$reopen_modal, {
    req(resident_info())
    showEvaluationModal()
  })

  # ── Navigation flow logic ─────────────────────────────────────────────
  # Period selection and navigation handler
  observeEvent(input$period_next, {
    req(selected_period())

    # Hide both period selection and help text
    shinyjs::hide("period_selection_card")
    shinyjs::hide("period_help_text")

    # Reset all cards before showing the next one
    purrr::walk(
      c("intro_card", "section1_card", "section2_card", "section3_card",
        "section4_card", "section5_card", "section6_card", "completion_card"),
      shinyjs::hide
    )

    # Set flow path and transition to appropriate card
    if (selected_period() == "Entering Residency") {
      rv$flow_path <- "entering_residency"
      transition("period_selection_card", "intro_card")
    } else {
      rv$flow_path <- "standard"
      transition("period_selection_card", "section1_card")
    }
  })

  # Intro next button observer
  # Intro next button observer
  observeEvent(input$intro_next, {
    # Check that Missouri education questions are answered
    req(input$hs_mo, input$college_mo, input$med_mo)

    # Store responses in the reactiveValues object
    responses$hs_mo <- input$hs_mo
    responses$college_mo <- input$college_mo
    responses$med_mo <- input$med_mo

    # Move to section 2 without submitting
    # (Data will be submitted at a later step)
    transition("intro_card", "section2_card")
  })

  # Section 1 next button observer
  observeEvent(input$section1_next, {
    req(input$plus, input$delta)

    # Store the responses
    responses$s_e_plus <- input$plus
    responses$s_e_delta <- input$delta

    # Continue to next section
    transition("section1_card", "section2_card")
  })

  # Section 2 next button observer
  observeEvent(input$section2_next, {
    # Check if this is the entering residency flow
    if (rv$flow_path == "entering_residency") {
      showNotification("Attempting to submit data for Entering Residency...", type = "message")
      validate(need(record_id(), "No record_id found!"))

      # Get valid fields and checkbox information
      s_eval_dict <- rdm_dict %>% filter(form_name == "s_eval")
      valid_fields <- s_eval_dict %>% pull(field_name)

      # Extract checkbox information
      checkbox_info <- extract_checkbox_info(rdm_dict, "s_eval")

      # Process all inputs
      all_inputs <- reactiveValuesToList(input)
      processed_inputs <- process_form_inputs(
        all_inputs,
        valid_fields,
        checkbox_info$fields,
        checkbox_info$options
      )

      # Use our reliable submission function with period-based instance
      res <- reliable_s_eval_submission(
        record_id = record_id(),
        period = "Entering Residency",
        processed_inputs = processed_inputs,
        redcap_url = redcap_url,
        token = rdm_token
      )

      if (!isTRUE(res$success)) {
        showNotification(paste("Save failed:", res$outcome_message), type = "error", duration = NULL)
        return()
      } else {
        showNotification("Self-assessment data saved!", type = "message", duration = 5)

        # Store all inputs for later use in the responses reactiveValues
        for (field_name in names(processed_inputs)) {
          responses[[field_name]] <- processed_inputs[[field_name]]
        }

        # Continue to next section
        transition("section2_card", "section5_card")
      }
    } else {
      # For standard flow, continue to section 3
      transition("section2_card", "section3_card")
    }
  })

  # Section 3 next button observer
  observeEvent(input$section3_next, {
    # This handles the standard flow submission after section 3
    if (rv$flow_path == "standard") {
      validate(need(record_id(), "No record_id found!"))

      # Get valid fields and checkbox information
      s_eval_dict <- rdm_dict %>% filter(form_name == "s_eval")
      valid_fields <- s_eval_dict %>% pull(field_name)

      # Extract checkbox information
      checkbox_info <- extract_checkbox_info(rdm_dict, "s_eval")

      # Process all inputs
      all_inputs <- reactiveValuesToList(input)
      processed_inputs <- process_form_inputs(
        all_inputs,
        valid_fields,
        checkbox_info$fields,
        checkbox_info$options
      )

      # Add data from previous cards if needed
      if (!is.null(responses$s_e_plus)) processed_inputs$s_e_plus <- responses$s_e_plus
      if (!is.null(responses$s_e_delta)) processed_inputs$s_e_delta <- responses$s_e_delta

      # Store program feedback data in responses
      store_program_feedback()

      # Add stored responses to the payload
      for (field in program_feedback_fields) {
        if (!is.null(responses[[field]])) {
          processed_inputs[[field]] <- responses[[field]]
        }
      }

      # Use our reliable submission function
      res <- reliable_s_eval_submission(
        record_id = record_id(),
        period = selected_period(),
        processed_inputs = processed_inputs,
        redcap_url = redcap_url,
        token = rdm_token
      )

      if (!isTRUE(res$success)) {
        showNotification(paste("Save failed:", res$outcome_message), type = "error", duration = NULL)
        return()
      } else {
        showNotification("Self-assessment successfully submitted!", type = "message", duration = 5)
      }

      # Continue to section 4 for everyone who completes section 3
      transition("section3_card", "section4_card")
    }
  })

  # Card 5 - Milestone Self assessment:
  observe({
    req(selected_period(), resident_info())

    # Get the previous period for the current selection
    prev_period <- get_previous_period(selected_period())

    # Only attempt to load previous data if a previous period exists
    if (!is.na(prev_period)) {
      # Debug logging
      message(paste("Loading milestone data for previous period:", prev_period))
    }
  })

  # Render the previous self-assessment plot
  output$previous_self_plot <- renderPlot({
    req(selected_period(), resident_info())

    # Get the previous period
    prev_period <- get_previous_period(selected_period())

    # Only attempt to render if previous period exists
    if (!is.na(prev_period)) {
      tryCatch({
        miles_plot(s_miles, resident_info(), prev_period)
      }, error = function(e) {
        message(paste("Error rendering previous self plot:", e$message))
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No previous self-assessment data available",
                   color = "darkgray", size = 5) +
          theme_void()
      })
    } else {
      # Create an empty plot for first-time assessments
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "No previous assessment period",
                 color = "darkgray", size = 5) +
        theme_void()
    }
  })

  # Render the previous program assessment plot
  output$previous_program_plot <- renderPlot({
    req(selected_period(), resident_info())

    # Get the previous period
    prev_period <- get_previous_period(selected_period())

    # Only attempt to render if previous period exists and should have program data
    if (!is.na(prev_period) && has_program_data(prev_period)) {
      tryCatch({
        miles_plot(p_miles, resident_info(), prev_period)
      }, error = function(e) {
        message(paste("Error rendering previous program plot:", e$message))
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No previous program assessment data available",
                   color = "darkgray", size = 5) +
          theme_void()
      })
    } else {
      # Create an empty plot when no previous program data exists or is expected
      reason <- if(is.na(prev_period)) {
        "No previous assessment period"
      } else {
        "No program assessment for this period"
      }

      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = reason,
                 color = "darkgray", size = 5) +
        theme_void()
    }
  })

  # Process current milestone data function
  process_current_milestone <- function(milestone_scores, resident_name, current_period) {
    # Extract the milestone scores
    scores <- milestone_scores$scores()

    # Convert scores from module format (PC_1) to data format (PC1)
    converted_scores <- list()
    for (milestone_name in names(scores)) {
      # Convert milestone names to match the expected format (e.g., "PC_1" to "PC1")
      column_name <- gsub("_", "", milestone_name)
      converted_scores[[column_name]] <- as.numeric(scores[[milestone_name]])
    }

    # Create a data frame with the current scores
    current_data <- data.frame(
      name = resident_name,
      period = current_period,
      stringsAsFactors = FALSE
    )

    # Add all milestone scores to the data frame
    for (milestone_name in names(converted_scores)) {
      current_data[[milestone_name]] <- converted_scores[[milestone_name]]
    }

    # Add required fields
    current_data$mile_date <- format(Sys.Date(), "%Y-%m-%d")
    current_data$record_id <- "current"

    return(current_data)
  }

  # Drop the milestone UI into card 5
  output$milestone_module_ui <- renderUI({
    req(selected_period())
    mod_miles_rating_ui("miles")
  })

  # Instantiate milestone module
  miles_mod <- mod_miles_rating_server(
    "miles",
    period = selected_period
  )

  observeEvent(input[["miles-done"]], {
    req(miles_mod$scores(), selected_period(), resident_info())
    raw_sel  <- miles_mod$scores()
    raw_desc <- miles_mod$desc()

    `%||%` <- function(a,b) if (is.null(a)) b else a

    # Mapping for field names remains the same
    mile_key2field <- c(
      "PC_1" = "rep_pc1_self",
      "PC_2" = "rep_pc2_self",
      "PC_3" = "rep_pc3_self",
      "PC_4" = "rep_pc4_self",
      "PC_5" = "rep_pc5_self",
      "PC_6" = "rep_pc6_self",
      "MK_1" = "rep_mk1_self",
      "MK_2" = "rep_mk2_self",
      "MK_3" = "rep_mk3_self",
      "SBP_1" = "rep_sbp1_self",
      "SBP_2" = "rep_sbp2_self",
      "SBP_3" = "rep_sbp3_self",
      "PBLI_1" = "rep_pbl1_self",
      "PBLI_2" = "rep_pbl2_self",
      "PROF_1" = "rep_prof1_self",
      "PROF_2" = "rep_prof2_self",
      "PROF_3" = "rep_prof3_self",
      "PROF_4" = "rep_prof4_self",
      "ICS_1" = "rep_ics1_self",
      "ICS_2" = "rep_ics2_self",
      "ICS_3" = "rep_ics3_self"
    )

    # List of fields that have description fields in REDCap
    desc_enabled_fields <- c(
      "PC_1", "PC_2", "PC_3", "PC_6",
      "PBLI_1", "PBLI_2",
      "PROF_1", "PROF_2", "PROF_3", "PROF_4",
      "ICS_1", "ICS_2", "ICS_3"
    )

    # Period mapping based on your dropdown values
    period_mapping <- c(
      "Entering Residency" = "7",
      "Mid Intern" = "1",
      "End Intern" = "2",
      "Mid PGY2" = "3",
      "End PGY2" = "4",
      "Mid PGY3" = "5",
      "Graduating" = "6"
    )

    # Define instance_number explicitly
    instance_number <- as.character(period_mapping[selected_period()])
    message("Using period-based instance number: ", instance_number, " for milestone_selfevaluation_c33c")

    # Build fields list with all milestone data
    fields <- list(
      prog_mile_date_self = as.character(Sys.Date()),
      prog_mile_period_self = period_mapping[selected_period()]
    )

    # Add milestone scores from raw_sel
    for(key in names(raw_sel)) {
      field_name <- mile_key2field[[key]]
      if (!is.null(field_name)) {
        fields[[field_name]] <- as.character(raw_sel[[key]])  # Convert to character

        # Add description if it exists and is enabled
        if (key %in% desc_enabled_fields &&
            !is.null(raw_desc[[key]]) &&
            nzchar(trimws(raw_desc[[key]]))) {
          desc_field <- paste0(field_name, "_desc")
          fields[[desc_field]] <- as.character(raw_desc[[key]])
        }
      }
    }

    # Make sure all values are properly escaped in our manually constructed JSON
    # Build data manually but DO NOT include the complete field
    data_str <- '[{'
    data_str <- paste0(data_str, '"record_id":"', escape_json_string(as.character(record_id())), '"')
    data_str <- paste0(data_str, ',"redcap_repeat_instrument":"milestone_selfevaluation_c33c"')
    data_str <- paste0(data_str, ',"redcap_repeat_instance":"', escape_json_string(instance_number), '"')

    # Add all fields with proper escaping
    for (field in names(fields)) {
      if (!is.null(fields[[field]]) && !is.na(fields[[field]])) {
        # Use our safe escaping function
        value <- escape_json_string(as.character(fields[[field]]))
        data_str <- paste0(data_str, ',"', field, '":"', value, '"')
      }
    }

    # Close the object and array WITHOUT adding the complete status field
    data_str <- paste0(data_str, "}]")

    message("Submitting milestone data (first 100 chars): ", substr(data_str, 1, 100))

    # Submit to REDCap with modified parameters to address the error
    response <- httr::POST(
      url = redcap_url,
      body = list(
        token = rdm_token,
        content = "record",
        action = "import",
        format = "json",
        type = "flat",
        overwriteBehavior = "normal",
        forceAutoNumber = "false",
        data = data_str,
        returnContent = "count",
        returnFormat = "json"
      ),
      encode = "form"
    )

    # Process response
    status_code <- httr::status_code(response)
    response_content <- httr::content(response, "text", encoding = "UTF-8")

    message("REDCap API response status: ", status_code)
    message("REDCap API response: ", response_content)

    # Check if submission was successful
    submission_success <- status_code == 200

    # Process current milestone data (this part remains unchanged)
    rv$current_milestone_data <- process_current_milestone(
      milestone_scores = miles_mod,
      resident_name = resident_info(),
      current_period = selected_period()
    )

    # Now add this code to combine with median data
    # After setting rv$current_milestone_data
    if (!is.null(s_miles)) {
      # Get median data for the same period
      median_data <- s_miles %>%
        filter(name == "Median", period == selected_period())

      if (nrow(median_data) > 0) {
        # Make sure both datasets have compatible columns
        common_cols <- intersect(names(rv$current_milestone_data), names(median_data))

        # Combine the data
        rv$current_milestone_data <- bind_rows(
          rv$current_milestone_data[, common_cols],
          median_data[, common_cols]
        )
      }
    }

    if (!submission_success) {
      # Check if the error is just about the form status field
      if (grepl("Form Status field", response_content)) {
        # If the error is just about the form status, consider this a success
        showNotification("Milestone data saved (form status unchanged)", type = "message", duration = 5)

        # Update flow control to handle both Entering and Graduating periods
        if (selected_period() == "Entering Residency" || selected_period() == "Graduating") {
          transition("section5_card", "completion_card")
        } else {
          transition("section5_card", "section6_card")
        }
      } else {
        # If it's another error, show it
        showNotification(paste("Save failed:", response_content), type = "error", duration = NULL)
        return()
      }
    } else {
      showNotification("Milestone self-evaluation saved!", type = "message", duration = 5)

      # Update flow control to handle both Entering and Graduating periods
      if (selected_period() == "Entering Residency" || selected_period() == "Graduating") {
        transition("section5_card", "completion_card")
      } else {
        transition("section5_card", "section6_card")
      }
    }
  })

  # Initialize the goals module
  goals_mod <- goalSettingServer(
    "goals",
    rdm_dict_data = rdm_dict,
    subcompetency_maps = subcompetency_maps,
    competency_list = competency_list,
    milestone_levels = milestone_levels,
    current_milestone_data = reactive({ rv$current_milestone_data }),
    resident_info = resident_info,
    selected_period = selected_period
  )

  # Load previous goals if available
  observe({
    req(app_data, record_id(), selected_period(), goals_mod)
    cat("Debug: Loading previous goals\n")

    # Get the previous period
    prev_period <- get_previous_period(selected_period())

    if(!is.na(prev_period) && !is.null(app_data$ilp_data)) {
      prev_goals <- load_previous_goals(
        ilp_data = app_data$ilp_data,
        record_id = record_id(),
        current_period = prev_period  # Use previous period to fetch previous goals
      )

      if(!is.null(prev_goals)) {
        goals_mod$set_previous_goals(prev_goals)
        cat("Debug: Previous goals set\n")
      } else {
        cat("Debug: No previous goals found\n")
      }
    }
  })

  process_ilp_submission <- function(goals_mod, record_id, period, redcap_url, token) {
    # Get responses from goals module
    resp <- goals_mod$get_responses()
    message("DEBUG: Got responses from goals module")

    # Get period code
    period_code <- get_ccc_session(period)

    # Use period code as instance number
    instance_num <- as.character(period_code)

    message("DEBUG: Using instance number ", instance_num, " for period ", period)

    # Create fields collection - only basic fields initially
    fields <- list(
      ilp_date = format(Sys.Date(), "%Y-%m-%d"),
      year_resident = as.character(period_code)
    )

    # Prepare mapping vectors
    pcmk_vec    <- c(names(subcompetency_maps$PC),    names(subcompetency_maps$MK))
    sbppbl_vec  <- c(names(subcompetency_maps$SBP),   names(subcompetency_maps$PBLI))
    profics_vec <- c(names(subcompetency_maps$PROF),  names(subcompetency_maps$ICS))

    # Process each group of competencies
    for (grp in c("pcmk", "sbppbl", "profics")) {
      g <- resp[[grp]]
      if (is.null(g)) {
        message("DEBUG: No data for group ", grp)
        next
      }

      message("DEBUG: Processing group ", grp)

      # Only add metadata fields (not the actual matrix cells yet)
      if (grp == "pcmk") {
        fields[["prior_goal_pcmk"]] <- g$had_prior_goal
        fields[["review_q_pcmk"]] <- g$review
        fields[["goal_pcmk"]] <- as.character(match(g$subcompetency, pcmk_vec))
        fields[["goal_level_pcmk"]] <- g$selected_level
        fields[["goal_level_r_pcmk"]] <- g$selected_row
        fields[["how_pcmk"]] <- g$how_to_achieve
      }
      else if (grp == "sbppbl") {
        fields[["prior_goal_sbppbl"]] <- g$had_prior_goal
        fields[["review_q_sbppbl"]] <- g$review
        fields[["goal_sbppbl"]] <- as.character(match(g$subcompetency, sbppbl_vec))
        fields[["goal_level_sbppbl"]] <- g$selected_level
        fields[["goal_r_sbppbl"]] <- g$selected_row
        fields[["how_sbppbl"]] <- g$how_to_achieve
      }
      else if (grp == "profics") {
        fields[["prior_goal_profics"]] <- g$had_prior_goal
        fields[["review_q_profics"]] <- g$review
        fields[["goal_subcomp_profics"]] <- as.character(match(g$subcompetency, profics_vec))
        fields[["goal_level_profics"]] <- g$selected_level
        fields[["goal_r_profics"]] <- g$selected_row
        fields[["how_profics"]] <- g$how_to_achieve
      }

      # Now set just the specific matrix cell based on competency and row
      if (grp == "pcmk") {
        if (startsWith(g$subcompetency, "PC")) {
          cell <- paste0("pc", substr(g$subcompetency, 3, 3), "_r", g$selected_row)
        } else if (startsWith(g$subcompetency, "MK")) {
          cell <- paste0("mk", substr(g$subcompetency, 3, 3), "_r", g$selected_row)
        }
        fields[[cell]] <- as.character(g$selected_level)
        message("DEBUG: Set matrix cell: ", cell, " = ", fields[[cell]])
      }
      else if (grp == "sbppbl") {
        if (startsWith(g$subcompetency, "SBP")) {
          cell <- paste0("sbp", substr(g$subcompetency, 4, 4), "_r", g$selected_row)
        } else if (startsWith(g$subcompetency, "PBLI")) {
          cell <- paste0("pbl", substr(g$subcompetency, 5, 5), "_r", g$selected_row)
        }
        fields[[cell]] <- as.character(g$selected_level)
        message("DEBUG: Set matrix cell: ", cell, " = ", fields[[cell]])
      }
      else if (grp == "profics") {
        if (startsWith(g$subcompetency, "PROF")) {
          cell <- paste0("prof", substr(g$subcompetency, 5, 5), "_r", g$selected_row)
        } else if (startsWith(g$subcompetency, "ICS")) {
          cell <- paste0("ics", substr(g$subcompetency, 4, 4), "_r", g$selected_row)
        }
        fields[[cell]] <- as.character(g$selected_level)
        message("DEBUG: Set matrix cell: ", cell, " = ", fields[[cell]])
      }
    }

    # Create proper JSON data structure first
    record_data <- list(
      record_id = as.character(record_id),
      redcap_repeat_instrument = "ilp",
      redcap_repeat_instance = as.character(instance_num),
      ilp_complete = "0"
    )

    # Add all the fields to our record data
    for (field_name in names(fields)) {
      if (!is.null(fields[[field_name]]) && !is.na(fields[[field_name]])) {
        record_data[[field_name]] <- as.character(fields[[field_name]])
      }
    }

    # Properly construct JSON as a string - ensure array format
    data_str <- '['

    # Add record data
    data_str <- paste0(data_str, '{')
    first_field <- TRUE
    for (field_name in names(record_data)) {
      # Add comma except for first field
      if (!first_field) {
        data_str <- paste0(data_str, ',')
      } else {
        first_field <- FALSE
      }

      # Properly escape any special characters in the value
      field_value <- gsub('"', '\\"', record_data[[field_name]])
      data_str <- paste0(data_str, '"', field_name, '":"', field_value, '"')
    }
    data_str <- paste0(data_str, '}')

    # Close JSON array
    data_str <- paste0(data_str, ']')

    message("DEBUG: Final JSON data (first 100 chars): ", substr(data_str, 1, 100), "...")

    # Submit to REDCap
    response <- httr::POST(
      url = redcap_url,
      body = list(
        token = token,
        content = "record",
        action = "import",
        format = "json",
        type = "flat",
        overwriteBehavior = "normal",
        forceAutoNumber = "false",
        data = data_str,
        returnContent = "count",
        returnFormat = "json"
      ),
      encode = "form"
    )

    # Process response
    status_code <- httr::status_code(response)
    response_content <- httr::content(response, "text", encoding = "UTF-8")

    message("REDCap API response status: ", status_code)
    message("REDCap API response: ", response_content)

    # Return result
    if (status_code == 200) {
      return(list(
        success = TRUE,
        outcome_message = paste("Successfully submitted ILP data for record", record_id)
      ))
    } else {
      return(list(
        success = FALSE,
        outcome_message = paste("Failed to submit ILP data for record", record_id, ":", response_content)
      ))
    }
  }
  # The observer handling the submission button click doesn't need to change
  observeEvent(goals_mod$submission_ready(), {
    message("🚀 DEBUG: submission_ready() triggered")
    req(goals_mod$submission_ready())    # only fire on a TRUE click

    # 1) show a "saving…" message
    notif_id <- showNotification("Saving ILP goals…", type="message", duration=NULL)

    # 2) do the REDCap push
    tryCatch({
      message("DEBUG: About to call process_ilp_submission")

      result <- process_ilp_submission(
        goals_mod   = goals_mod,
        record_id   = record_id(),
        period      = selected_period(),
        redcap_url  = redcap_url,
        token       = rdm_token
      )

      # 3) clear the "saving" message
      removeNotification(notif_id)

      # 4) reset the module's flag
      goals_mod$reset_submission()

      # 5) transition based on result
      if (isTRUE(result$success)) {
        message("DEBUG: Showing success notification")
        showNotification("✅ ILP goals submitted!", type="message", duration=5)

        # Force a delay before transition to ensure UI updates
        shinyjs::delay(500, {
          message("DEBUG: About to transition to completion_card")
          transition("section6_card", "completion_card")
          message("DEBUG: Transitioned to completion card")
        })
      } else {
        message("DEBUG: Showing error notification")
        showNotification(
          paste0("❌ Submission failed: ", result$outcome_message %||% "unknown error"),
          type="error", duration=10
        )
      }
    }, error = function(e) {
      # Handle any errors
      message("CRITICAL ERROR in submission process: ", e$message)
      removeNotification(notif_id)
      showNotification(
        paste0("❌ Critical error: ", e$message),
        type="error", duration=10
      )
      # Try to reset module state
      tryCatch({
        goals_mod$reset_submission()
      }, error = function(e2) {
        message("Could not reset goals module: ", e2$message)
      })
    })
  })
}

