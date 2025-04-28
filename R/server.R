server <- function(input, output, session) {
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


  # ── Access code logic ─────────────────────────────────────────────
  access_code <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$code) && nzchar(query$code)) trimws(query$code)
    else trimws(input$access_code_input)
  })

  output$valid_code <- reactive({
    req(access_code())                # make sure we have *something*
    access_code() %in% resident_data$access_code
  })
  outputOptions(output, "valid_code", suspendWhenHidden = FALSE)

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

  output$resident_name <- renderText({ req(resident_info()); paste("Dashboard for:", resident_info()) })
  output$coach_name <- renderText({ req(coach_info()); paste("Coach is Dr:", coach_info()) })

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

  rv <- reactiveValues()


  # Main function to render the card based on period
  # Ensure render_card2_ui is passing the entire data dictionary
  render_card2_ui <- function(period, rdm_dict, entering_fields, graduating_fields, other_fields) {
    if (period == "Entering Residency") {
      ent <- rdm_dict %>% filter(form_name == "s_eval", field_name %in% entering_fields)
      return(tagList(
        render_faculty_block(ent), tags$hr(),
        render_goals_block(ent), tags$hr(),
        render_prep_table(ent), tags$hr(),
        render_topic_block(ent), tags$hr(),
        render_career_block(ent), tags$hr(),
        render_fellowship_block(ent), tags$hr(),
        render_track_block(ent)
      ))
    }
    else if (period == "Graduating") {
      return(tagList(
        render_next_steps_block(rdm_dict, graduating_fields),
        tags$hr(),
        render_contact_block(rdm_dict)
      ))
    }
    else {
      # For all other periods (regular check-ins)
      oth <- rdm_dict %>% filter(form_name == "s_eval", field_name %in% other_fields)
      return(tagList(
        render_other_topics_block(oth), tags$hr(),
        render_other_career_block(oth), tags$hr(),
        render_other_tracks_block(oth), tags$hr(),
        render_step3_block(oth), tags$hr(),
        render_other_board_block(oth), tags$hr(),
        render_mksap_block(oth), tags$hr(),
        render_discussion_block(oth)
      ))
    }
  }

  # Main render function for Card 3 UI
  render_card3_ui <- function(rdm_dict) {
    # Filter dictionary to get fields for this card
    card_data <- rdm_dict %>% filter(form_name == "s_eval", field_name %in% program_feedback_fields)

    tagList(
      tags$h3("Program Feedback"),

      # Introduction text with formatting
      HTML("
        <p>What are your thoughts about your experiences in the program?
        <em>Please note, although this is in your self-evaluation, this data will be extracted
        anonymously and collated for the entire program for the Program Evaluation Committee to review.</em></p>
      "),

      tags$hr(),

      # Positive experiences
      HTML("<p><span style='font-weight: normal;'>Are there certain rotations / conferences / or other experiences that positively contribute to your training?</span></p>"),
      textAreaInput(
        "s_e_prog_plus",
        NULL,  # Label is already in the HTML above
        "",
        width = "100%",
        height = "150px"
      ),

      tags$hr(),

      # Experiences needing improvement
      HTML("<p><span style='font-weight: normal;'>Are there certain rotations / conferences / or other experiences that need improvement?</span></p>"),
      textAreaInput(
        "s_e_prog_delta",
        NULL,  # Label is already in the HTML above
        "",
        width = "100%",
        height = "150px"
      ),

      tags$hr(),

      # Conference and Grand Rounds attendance
      HTML("<p><span style='font-weight: normal;'>Regarding conference and Grand Rounds attendence - in what circumstances is it easy to attend? Hard? What steps would you like to see the program try?</span></p>"),
      textAreaInput(
        "s_e_progconf",
        NULL,  # Label is already in the HTML above
        "",
        width = "100%",
        height = "150px"
      ),

      tags$hr(),

      # Other program feedback
      HTML("<p><span style='font-weight: normal;'>Are there any other systems issues within the program that you want to bring to attention or other feedback for improvement?</span></p>"),
      textAreaInput(
        "s_e_progfeed",
        NULL,  # Label is already in the HTML above
        "",
        width = "100%",
        height = "150px"
      )
    )
  }

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

  output$card3UI <- renderUI({
    render_card3_ui(rdm_dict)
  })

  output$scholarship_module_ui <- renderUI({
    req(selected_period())
    if (selected_period() != "Entering Residency")
      scholarship_module_ui("schol")
  })

  scholarship_module_server(
    "schol",
    rdm_dict = rdm_dict,
    record_id = fetch_record_id(resident_info(), resident_data, url, rdm_token)
  )

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

  observeEvent(input$open_modal, { req(resident_info()); showEvaluationModal() })
  observeEvent(input$reopen_modal, { req(resident_info()); showEvaluationModal() })

  # ── Navigation flow logic ─────────────────────────────────────────────
  observeEvent(input$period_next, {
    req(selected_period())

    # Reset all cards
    purrr::walk(
      c("intro_card","section1_card","section2_card","section3_card",
        "section4_card","section5_card","section6_card","completion_card"),
      shinyjs::hide
    )

    if (selected_period() == "Entering Residency") {
      # For Entering Residency: show intro, card 2, and card 5 only
      transition("period_selection_card", "intro_card")
      # Store the flow path for later navigation
      rv$flow_path <- "entering_residency"
    } else {
      # For all other periods: show cards 1-6 (no intro)
      transition("period_selection_card", "section1_card")
      rv$flow_path <- "standard"
    }
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

      record_id <- fetch_record_id(resident_info(), NULL, url, rdm_token)
      validate(need(record_id, "No record_id found!"))

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
        record_id = record_id,
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
        record_id = record_id,
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
      record_id <- fetch_record_id(resident_info(), NULL, url, rdm_token)
      validate(need(record_id, "No record_id found!"))

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
        record_id = record_id,
        instrument = "s_eval",
        instance = NULL,  # Will be determined in submit function
        period = period_code,
        input_data = processed_inputs,
        valid_fields = valid_fields
      )

      # Submit to REDCap with period check
      res <- submit_to_redcap_with_period_check(
        record_id = record_id,
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

  observeEvent(input[["schol-next_btn"]], {
    transition("section4_card", "section5_card")
  })

  ## Card 5 - Milestone Self assessment:

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

    record_id <- as.character(resident_data$record_id[resident_data$name == resident_info()][1])
    validate(need(!is.null(record_id) && length(record_id) > 0, "No record_id found!"))

    # Create a direct submission dataframe (similar to your other app)
    submission_data <- data.frame(
      record_id = record_id,
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
      record_id = record_id,
      instrument_name = "milestone_selfevaluation_c33c",
      coach_data = miles,
      redcap_uri = url,
      token = rdm_token
    )

    # Set the instance number
    submission_data$redcap_repeat_instance <- as.numeric(instance_number)

    # Ensure rownames don't get included
    rownames(submission_data) <- NULL

    # Submit the data
    submission_response <- direct_redcap_import(
      data = submission_data,
      record_id = record_id,
      url = url,
      token = rdm_token
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

}


