server <- function(input, output, session) {
  # Make sure data is available
  observe({
    # This will run once when the server starts
    if (!exists("rdm_dict") || is.null(rdm_dict)) {
      showNotification("Loading required data...", type = "message", duration = NULL, id = "loading")

      # Try loading data again
      app_data <- ensure_data_loaded()

      if (!is.null(app_data)) {
        # Create variables in the server environment
        rdm_dict <<- app_data$rdm_dict
        ass_dict <<- app_data$ass_dict
        url <<- app_data$url
        eval_token <<- app_data$eval_token
        rdm_token <<- app_data$rdm_token
        fac_token <<- app_data$fac_token

        # Remove the notification
        removeNotification(id = "loading")
        showNotification("Data loaded successfully", type = "message", duration = 3)
      } else {
        # Show an error
        removeNotification(id = "loading")
        showNotification(
          "Critical error: Failed to load required data. Please check your tokens and configuration.",
          type = "error",
          duration = NULL
        )
      }
    }
  }, once = TRUE)  # Note: No comma here, this closes the observe() call


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

  # Handle the submit button click
  observeEvent(input$validate_code, {
    req(access_code())
    is_valid <- access_code() %in% resident_data$access_code
    if(is_valid) {
      shinyjs::hide("access_code_screen")
      shinyjs::show("main_app_content")
    } else {
      # Update validation state
      updateTextInput(session, "is_valid_code", value = FALSE)
    }
  })


  resident_info <- reactive({
    req(access_code())
    df <- resident_data %>% filter(access_code == access_code())
    if (nrow(df) == 0) return(NULL)
    df$name[1]
  })

  coach_info <- reactive({
    req(access_code())
    df <- resident_data %>% filter(access_code == access_code())
    if (nrow(df) == 0) return(NULL)
    df$coach[1]
  })

  output$resident_name <- renderText({ req(resident_info()); paste("Self Evaluation for:", resident_info()) })
  output$coach_name <- renderText({ req(coach_info()); paste("Coach is Dr:", coach_info()) })

  # record_id reactive - move this after resident_info is defined
  record_id <- reactive({
    req(access_code())
    df <- resident_data %>% filter(access_code == access_code())
    if (nrow(df) == 0) return(NULL)
    df$record_id[1]
  })

  # Add validation observer - move after record_id reactive
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

  # Main render function for Card 3 UI


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
      scholarship_module_ui("schol")
    else
      div(
        p("Scholarship tracking begins after entering residency."),
        actionButton("schol_skip", "Continue to Next Section", class = "btn-primary")
      )
  })

  # Call without the schol_data parameter
  scholarship_module_server(
    "schol",
    rdm_dict = rdm_dict,
    record_id = reactive({
      req(resident_info())
      fetch_record_id(resident_info(), resident_data, url, rdm_token)
    })
  )

  observeEvent(input[["schol-next_btn"]], {
    transition("section4_card", "section5_card")
  })

  observeEvent(input$schol_skip, {
    transition("section4_card", "section5_card")
  })


  observeEvent(input$open_modal, { req(resident_info()); showEvaluationModal() })
  observeEvent(input$reopen_modal, { req(resident_info()); showEvaluationModal() })

  # ── Navigation flow logic ─────────────────────────────────────────────
  # Consolidated period selection and navigation handler
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

  # Add this observer for the intro_next button (place it near your other observeEvent handlers)
  observeEvent(input$intro_next, {
    # Check that Missouri education questions are answered
    req(input$hs_mo, input$college_mo, input$med_mo)

    # Store responses in the reactiveValues object
    responses$hs_mo <- input$hs_mo
    responses$college_mo <- input$college_mo
    responses$med_mo <- input$med_mo

    # Move to section 2 (since this is entering residency flow)
    transition("intro_card", "section2_card")
  })

  observeEvent(input$section1_next, {
    req(input$plus, input$delta)
    responses$s_e_plus  <- input$plus
    responses$s_e_delta <- input$delta
    transition("section1_card", "section2_card")
  })

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

      # Get the period code (ensure this is included)
      period_code <- get_ccc_session(selected_period())

      # IMPORTANT: Explicitly add the period to the processed inputs
      processed_inputs$s_e_period <- period_code

      # Prepare the payload
      payload <- prepare_redcap_payload(
        record_id = record_id(),
        instrument = "s_eval",
        instance = NULL,  # Will be determined in submit function
        period = period_code,
        input_data = processed_inputs,
        valid_fields = valid_fields
      )

      # Make sure s_e_period is explicitly included in the payload
      payload$s_e_period <- period_code

      # Submit to REDCap with period check
      res <- submit_to_redcap_with_period_check(
        record_id = record_id(),
        instrument = "s_eval",
        period = period_code,
        data = payload,
        url = url,
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

        # CHANGED: Navigate to section5_card instead of completion_card for Entering Residency flow
        transition("section2_card", "section5_card")
      }
    } else {
      # For standard flow, continue to section 3
      transition("section2_card", "section3_card")
    }
  })

  # For standard flow - observe the section3_next button
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

      # Get the period code
      period_code <- get_ccc_session(selected_period())

      # Store program feedback data in responses
      store_program_feedback()

      # Add stored responses to the payload
      for (field in program_feedback_fields) {
        if (!is.null(responses[[field]])) {
          processed_inputs[[field]] <- responses[[field]]
        }
      }

      # Prepare the payload
      payload <- prepare_redcap_payload(
        record_id = record_id(),
        instrument = "s_eval",
        instance = NULL,  # Will be determined in submit function
        period = period_code,
        input_data = processed_inputs,
        valid_fields = valid_fields
      )

      # Submit to REDCap with period check
      res <- submit_to_redcap_with_period_check(
        record_id = record_id(),
        instrument = "s_eval",
        period = period_code,
        data = payload,
        url = url,
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


  ## Card 5 - Milestone Self assessment:

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

  # 2. Render the previous self-assessment plot
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

  # 3. Render the previous program assessment plot
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



  output$mainImage <- renderImage({
    req(currentImageFile())

    # The src should use the resource path we defined
    list(
      src = file.path("imres-images", currentImageFile()),
      width = 1200,
      height = "auto",
      alt = paste("Image", state$currentImageIndex)
    )
  }, deleteFile = FALSE)

  # 1) drop the milestone UI into card 5
  output$milestone_module_ui <- renderUI({
    req(selected_period())
    mod_miles_rating_ui("miles")
  })

  # 2) instantiate your module exactly once
  miles_mod <- mod_miles_rating_server(
    "miles",
    period = selected_period
  )

  observeEvent(input[["miles-done"]], {
    req(miles_mod$scores(), selected_period(), resident_info())
    raw_sel  <- miles_mod$scores()
    raw_desc <- miles_mod$desc()

    `%||%` <- function(a,b) if (is.null(a)) b else a

    # Mapping for field names - base fields that definitely exist
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


    # Create a direct submission dataframe (similar to your other app)
    submission_data <- data.frame(
      record_id = record_id(),
      redcap_repeat_instrument = "milestone_selfevaluation_c33c",
      prog_mile_date_self = as.character(Sys.Date()),
      prog_mile_period_self = period_mapping[selected_period()]
    )

    # Add milestone scores from raw_sel
    for(key in names(raw_sel)) {
      field_name <- mile_key2field[[key]]
      if (!is.null(field_name)) {
        submission_data[[field_name]] <- as.numeric(raw_sel[[key]])

        # Add description if it exists and is enabled
        if (key %in% desc_enabled_fields &&
            !is.null(raw_desc[[key]]) &&
            nzchar(trimws(raw_desc[[key]]))) {
          desc_field <- paste0(field_name, "_desc")
          submission_data[[desc_field]] <- as.character(raw_desc[[key]])
        }
      }
    }

    # Generate new instance or get existing one
    instance_number <- generate_new_instance(
      record_id = record_id(),
      instrument_name = "milestone_selfevaluation_c33c",
      coach_data = miles,
      redcap_uri = url,
      token = rdm_token
    )

    # Helper lists for competencies and milestones

    # Set the instance number
    submission_data$redcap_repeat_instance <- as.numeric(instance_number)

    # Ensure rownames don't get included
    rownames(submission_data) <- NULL

    # Submit the data
    submission_response <- direct_redcap_import(
      data = submission_data,
      record_id = record_id(),
      url = url,
      token = rdm_token
    )

    # Process the current milestone data for visualization in Card 6
    rv$current_milestone_data <- process_current_milestone(
      miles_mod,
      resident_info(),
      selected_period()
    )

    if (!submission_response$success) {
      showNotification(paste("Save failed:", submission_response$outcome_message), type = "error", duration = NULL)
      return()
    } else {
      showNotification("Milestone self-evaluation saved!", type = "message", duration = 5)
      if (selected_period() == "Entering Residency"){
        transition("section5_card", "completion_card")
      } else {
        transition("section5_card", "section6_card")
      }
    }
  })

  # Render the current milestone plot
  output$current_milestone_plot <- renderPlot({
    req(rv$current_milestone_data, selected_period())

    # Get median data for comparison
    median_data <- get_median_data(s_miles, selected_period())

    # Prepare the plot data
    plot_data <- prepare_milestone_plot_data(rv$current_milestone_data, median_data)

    # Check if we have valid data to plot
    if (nrow(plot_data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No milestone data available",
                   color = "red", size = 5) +
          theme_void()
      )
    }

    # Use the miles_plot function with the prepared data
    tryCatch({
      # Call miles_plot with the prepared dataset
      miles_plot(plot_data, resident_info(), selected_period())
    }, error = function(e) {
      message(paste("Error rendering current milestone plot:", e$message))

      # Return an error plot
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste("Error:", e$message),
                 color = "red", size = 4) +
        theme_void()
    })
  })

  goals_mod <- goalSettingServer(
    "goals",
    rdm_dict,
    subcompetency_maps,
    competency_list,
    milestone_levels
  )
  observe({
    req(selected_period(), resident_info())

    # Get the previous period
    prev_period <- get_previous_period(selected_period())

    if (!is.na(prev_period)) {
      record_id <- fetch_record_id(resident_info(), NULL, url, rdm_token)

      # Fetch previous goals from REDCap
      prev_goals <- fetch_previous_goals(
        record_id = record_id,
        period = prev_period,
        url = url,
        token = rdm_token
      )

      if (!is.null(prev_goals)) {
        # Update the goal module with previous data
        # This would require adding an update method to the module
        updateGoalModule(
          session = session,
          id = "goals",
          prev_data = prev_goals
        )
      }
    }
  })


  # Modify your submit handler to include the goals data
  observeEvent(input$submit, {
    req(goals_mod$all_selections())
    validate(need(record_id(), "No record_id found!"))

    # Get the period code
    period_code <- get_ccc_session(selected_period())

    # Get goals data from the module
    goals_data <- goals_mod$all_selections()

    # Create base submission data
    submission_data <- list(
      record_id = record_id(),
      ilp_period = period_code,

      # PC/MK Goal review data
      prior_goal_pcmk = goals_data$pcmk$review$had_prior_goal,
      review_q_pcmk = goals_data$pcmk$review$progress,
      review_q2_pcmk = goals_data$pcmk$review$factors,
      goal_pcmk = goals_data$pcmk$code,
      how_pcmk = goals_data$pcmk$goal$how,

      # SBP/PBLI Goal review data
      prior_goal_sbppbl = goals_data$sbp_pbli$review$had_prior_goal,
      review_q_sbppbl = goals_data$sbp_pbli$review$progress,
      review_q2_sbppbl = goals_data$sbp_pbli$review$factors,
      goal_sbppbl = goals_data$sbp_pbli$code,
      how_sbppbl = goals_data$sbp_pbli$goal$how,

      # PROF/ICS Goal review data
      prior_goal_profics = goals_data$prof_ics$review$had_prior_goal,
      review_q_profics = goals_data$prof_ics$review$progress,
      review_q2_profics = goals_data$prof_ics$review$factors,
      goal_profics = goals_data$prof_ics$code,
      how_profics = goals_data$prof_ics$goal$how
    )

    # Add milestone-specific level selections
    # For PC/MK
    if (!is.null(goals_data$pcmk$goal) && !is.null(goals_data$pcmk$subcomp_code)) {
      comp_code <- tolower(goals_data$pcmk$subcomp_code)
      row_id <- goals_data$pcmk$goal$row_id
      level <- goals_data$pcmk$goal$level
      field_name <- paste0(comp_code, "_r", row_id)
      submission_data[[field_name]] <- level
    }

    # For SBP/PBLI
    if (!is.null(goals_data$sbp_pbli$goal) && !is.null(goals_data$sbp_pbli$subcomp_code)) {
      comp_code <- tolower(goals_data$sbp_pbli$subcomp_code)
      row_id <- goals_data$sbp_pbli$goal$row_id
      level <- goals_data$sbp_pbli$goal$level
      field_name <- paste0(comp_code, "_r", row_id)
      submission_data[[field_name]] <- level
    }

    # For PROF/ICS
    if (!is.null(goals_data$prof_ics$goal) && !is.null(goals_data$prof_ics$subcomp_code)) {
      comp_code <- tolower(goals_data$prof_ics$subcomp_code)
      row_id <- goals_data$prof_ics$goal$row_id
      level <- goals_data$prof_ics$goal$level
      field_name <- paste0(comp_code, "_r", row_id)
      submission_data[[field_name]] <- level
    }

    # Add target dates and status if they exist
    if (!is.null(goals_data$pcmk$goal$target_date)) {
      submission_data$pcmk_target_date <- format(goals_data$pcmk$goal$target_date, "%Y-%m-%d")
      submission_data$pcmk_status <- goals_data$pcmk$goal$status
    }

    if (!is.null(goals_data$sbp_pbli$goal$target_date)) {
      submission_data$sbppbl_target_date <- format(goals_data$sbp_pbli$goal$target_date, "%Y-%m-%d")
      submission_data$sbppbl_status <- goals_data$sbp_pbli$goal$status
    }

    if (!is.null(goals_data$prof_ics$goal$target_date)) {
      submission_data$profics_target_date <- format(goals_data$prof_ics$goal$target_date, "%Y-%m-%d")
      submission_data$profics_status <- goals_data$prof_ics$goal$status
    }

    # Log what we're submitting (for debugging)
    message("Submitting goals data to RedCap: ", jsonlite::toJSON(submission_data))

    # Submit to RedCap using your existing function
    res <- submit_to_redcap_with_period_check(
      record_id = record_id(),
      instrument = "ilp",  # Your RedCap form name for goals
      period = period_code,
      data = submission_data,
      url = url,
      token = rdm_token
    )

    if (!isTRUE(res$success)) {
      showNotification(paste("Goal save failed:", res$outcome_message), type = "error", duration = NULL)
      return()
    } else {
      showNotification("Goals successfully saved!", type = "message", duration = 5)
      transition("section6_card", "completion_card")
    }
  })

}


