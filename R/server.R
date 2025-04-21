server <- function(input, output, session) {

  # ── Helper field sets ─────────────────────────────────────────────
  # Exactly the UME fields you want for "Entering Residency"
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
  rv <- reactiveValues()  # For storing intermediate data

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


  # Main render function for Card 2 UI
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
      grad <- rdm_dict %>% filter(form_name == "s_eval", field_name %in% graduating_fields)
      return(tagList(
        render_graduation_block(grad), tags$hr(),
        render_next_steps_block(grad), tags$hr(),
        render_fellowship_plans_block(grad), tags$hr(),
        render_practice_location_block(grad), tags$hr(),
        render_contact_block(grad)
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
        render_discussion_block(oth)  # Added discussion block
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

  # Add these outputs
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

  observeEvent(input$intro_next, {
    req(input$hs_mo, input$college_mo, input$med_mo)
    record_id <- fetch_record_id(resident_info(), NULL, url, rdm_token)
    validate(need(record_id, "No record_id found!"))

    hs_val      <- if (input$hs_mo      == "Yes") "1" else "0"
    college_val <- if (input$college_mo == "Yes") "1" else "0"
    med_val     <- if (input$med_mo     == "Yes") "1" else "0"

    payload_intro <- data.frame(
      record_id              = record_id,
      hs_mo                  = hs_val,
      college_mo             = college_val,
      med_mo                 = med_val,
      resident_data_complete = 0,
      stringsAsFactors       = FALSE
    )

    res_intro <- submit_to_redcap(payload_intro, record_id, url, rdm_token)
    if (!isTRUE(res_intro$success)) {
      showNotification(paste("Save failed:", res_intro$outcome_message), type = "error")
      return()
    }

    # Always go to section 2 after intro (for Entering Residency flow)
    transition("intro_card", "section2_card")
  })

  observeEvent(input$section1_next, {
    req(input$plus, input$delta)
    responses$s_e_plus  <- input$plus
    responses$s_e_delta <- input$delta
    transition("section1_card", "section2_card")
  })

  # For Entering Residency flow
  observeEvent(input$section2_next, {
    # Check if this is the entering residency flow
    if (rv$flow_path == "entering_residency") {
      showNotification("Attempting to submit data for Entering Residency...", type = "message")

      record_id <- fetch_record_id(resident_info(), NULL, url, rdm_token)
      validate(need(record_id, "No record_id found!"))

      # 1) Generate a new repeat instance
      next_inst <- generate_new_instance(record_id, "s_eval", NULL, url, rdm_token)

      # 2) Dynamically get valid fields and identify checkbox fields
      valid_fields <- NULL
      checkbox_fields <- NULL
      checkbox_options <- list()

      # Check if rdm_dict is available
      if (exists("rdm_dict") && !is.null(rdm_dict)) {
        # Get all fields for s_eval form
        s_eval_dict <- rdm_dict %>% filter(form_name == "s_eval")

        # Get regular fields (non-checkbox)
        valid_fields <- s_eval_dict %>%
          filter(field_type != "checkbox") %>%
          pull(field_name)

        # Identify checkbox fields separately
        checkbox_fields <- s_eval_dict %>%
          filter(field_type == "checkbox") %>%
          pull(field_name)

        # For each checkbox field, extract all possible options from dictionary
        for (cb_field in checkbox_fields) {
          # Get the select_choices_or_calculations field for this checkbox
          choices_text <- s_eval_dict %>%
            filter(field_name == cb_field) %>%
            pull(select_choices_or_calculations) %>%
            first()

          if (!is.null(choices_text) && choices_text != "") {
            # Parse the choices (format is usually "1, Option 1 | 2, Option 2 | ...")
            choices <- strsplit(choices_text, "\\|")[[1]]
            codes <- sapply(choices, function(x) {
              trim_code <- trimws(gsub(",.*$", "", x))
              return(trim_code)
            })
            checkbox_options[[cb_field]] <- codes
          }
        }

        print("Checkbox fields identified:")
        print(checkbox_fields)
        print("Checkbox options extracted:")
        print(checkbox_options)

        # Add the standard REDCap fields that are always needed
        valid_fields <- c(
          "record_id",
          "redcap_repeat_instrument",
          "redcap_repeat_instance",
          valid_fields
        )
      } else {
        # Fallback if dictionary is not available
        showNotification("Data dictionary not available. Using limited field set.", type = "warning")
        valid_fields <- c(
          "record_id", "redcap_repeat_instrument", "redcap_repeat_instance",
          "s_e_date", "s_e_period", "s_eval_complete"
        )
      }

      # 3) Start with base data as a list to build a complete data frame
      payload <- list(
        record_id                = record_id,
        redcap_repeat_instrument = "s_eval",
        redcap_repeat_instance   = next_inst,
        s_e_date                 = format(Sys.Date(), "%Y-%m-%d"),
        s_e_period               = get_ccc_session(selected_period()),
        s_eval_complete          = 0
      )

      # 4) Get all current input values
      all_inputs <- reactiveValuesToList(input)

      # 5) Process inputs and build payload for regular (non-checkbox) fields first
      for (field in names(all_inputs)) {
        # Skip fields with null values or UI-only fields
        if (is.null(all_inputs[[field]]) ||
            grepl("^(btn_|section|card)", field)) next

        # Add validation for email field if it exists
        if (field == "s_e_fac_email" && !is.null(all_inputs[[field]])) {
          # Check if it's a valid email format
          if (!grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", all_inputs[[field]])) {
            # If not valid, simply skip it
            next
          }
        }

        # Only process regular fields here (handle checkboxes separately)
        if (field %in% valid_fields && !(field %in% checkbox_fields)) {
          # Handle different types of inputs correctly
          if (length(all_inputs[[field]]) > 1) {
            # For multi-select inputs, join with commas
            payload[[field]] <- paste(all_inputs[[field]], collapse = ",")
          } else {
            # For single value inputs
            payload[[field]] <- all_inputs[[field]]
          }
        }
      }

      # 6) Now process checkbox fields
      for (cb_field in checkbox_fields) {
        if (!is.null(all_inputs[[cb_field]])) {
          print(paste("Checkbox field:", cb_field))
          print(paste("Values:", paste(all_inputs[[cb_field]], collapse=", ")))

          # Get the selected values
          selected_values <- all_inputs[[cb_field]]

          # If the selected values contain commas, they might be in "code, label" format
          # Extract just the code part
          if (is.character(selected_values)) {
            # Split by commas if they exist
            if (any(grepl(",", selected_values))) {
              selected_codes <- sapply(selected_values, function(val) {
                # Extract just the number part before the comma
                gsub("\\s*,.*$", "", val)
              })
            } else {
              selected_codes <- selected_values
            }
          } else {
            selected_codes <- as.character(selected_values)
          }

          print(paste("Cleaned codes:", paste(selected_codes, collapse=", ")))

          # Process each option for this checkbox field
          if (!is.null(checkbox_options[[cb_field]])) {
            for (option_code in checkbox_options[[cb_field]]) {
              # Create the REDCap field name with ___
              field_name <- paste0(cb_field, "___", option_code)

              # Clean option_code (strip quotes if they exist)
              clean_option <- gsub("\"", "", option_code)

              # Check if this option is in the selected values
              if (any(selected_codes %in% clean_option)) {
                payload[[field_name]] <- "1"
                print(paste(field_name, "= 1"))
              } else {
                payload[[field_name]] <- "0"
              }
            }
          }
        } else {
          # If this checkbox field isn't in the inputs, set all its options to 0
          if (!is.null(checkbox_options[[cb_field]])) {
            for (option_code in checkbox_options[[cb_field]]) {
              field_name <- paste0(cb_field, "___", option_code)
              payload[[field_name]] <- "0"
            }
          }
        }
      }

      # 7) Convert list to data frame - ensuring all fields have the same length
      for (name in names(payload)) {
        if (length(payload[[name]]) != 1) {
          payload[[name]] <- payload[[name]][1]
        }
      }

      final_data <- as.data.frame(payload, stringsAsFactors = FALSE)

      # Print debugging info
      print("Fields being submitted:")
      print(names(final_data))

      # 8) Submit to REDCap
      res <- submit_to_redcap(final_data, record_id, url, rdm_token)

      if (!isTRUE(res$success)) {
        showNotification(paste("Save failed:", res$outcome_message), type = "error", duration = NULL)
        return()
      } else {
        showNotification("Self-assessment successfully submitted!", type = "message", duration = 5)
      }

      # Navigate to completion card
      transition("section2_card", "completion_card")
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

      # 1) Generate a new repeat instance
      next_inst <- generate_new_instance(record_id, "s_eval", NULL, url, rdm_token)

      # 2) Dynamically get valid fields and identify checkbox fields
      valid_fields <- NULL
      checkbox_fields <- NULL

      # Check if rdm_dict is available
      if (exists("rdm_dict") && !is.null(rdm_dict)) {
        # Get all fields for s_eval form
        s_eval_dict <- rdm_dict %>% filter(form_name == "s_eval")

        # Get regular fields (non-checkbox)
        valid_fields <- s_eval_dict %>%
          filter(field_type != "checkbox") %>%
          pull(field_name)

        # Identify checkbox fields separately
        checkbox_fields <- s_eval_dict %>%
          filter(field_type == "checkbox") %>%
          pull(field_name)

        # Add the standard REDCap fields that are always needed
        valid_fields <- c(
          "record_id",
          "redcap_repeat_instrument",
          "redcap_repeat_instance",
          valid_fields
        )
      } else {
        # Fallback if dictionary is not available
        showNotification("Data dictionary not available. Using limited field set.", type = "warning")
        valid_fields <- c(
          "record_id", "redcap_repeat_instrument", "redcap_repeat_instance",
          "s_e_date", "s_e_period", "s_eval_complete"
        )
      }

      # 3) Start with base data as a list
      payload <- list(
        record_id                = record_id,
        redcap_repeat_instrument = "s_eval",
        redcap_repeat_instance   = next_inst,
        s_e_date                 = format(Sys.Date(), "%Y-%m-%d"),
        s_e_period               = get_ccc_session(selected_period()),
        s_eval_complete          = 0
      )

      # 4) Get all current input values
      all_inputs <- reactiveValuesToList(input)

      # 5) Process inputs and build payload
      for (field in names(all_inputs)) {
        # Skip fields with null values or UI-only fields
        if (is.null(all_inputs[[field]]) ||
            grepl("^(btn_|section|card)", field)) next

        # Add validation for email field if it exists
        if (field == "s_e_fac_email" && !is.null(all_inputs[[field]])) {
          # Check if it's a valid email format
          if (!grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", all_inputs[[field]])) {
            # If not valid, simply skip it
            next
          }
        }

        # Check if it's a regular (non-checkbox) field
        if (field %in% valid_fields) {
          # Handle different types of inputs correctly
          if (length(all_inputs[[field]]) > 1) {
            # For multi-select inputs, join with commas
            payload[[field]] <- paste(all_inputs[[field]], collapse = ",")
          } else {
            # For single value inputs
            payload[[field]] <- all_inputs[[field]]
          }
        }
        # Check if it's a checkbox option field (with ___N pattern)
        else if (grepl("___", field)) {
          # Extract the base field name
          base_field <- sub("___\\d+$", "", field)

          # Only include if the base field is a known checkbox field
          if (base_field %in% checkbox_fields) {
            # REDCap expects 0/1 values for checkboxes
            payload[[field]] <- as.character(as.integer(all_inputs[[field]]))
          }
        }
        # For checkbox base fields, we skip them (we'll handle their options directly)
        else if (field %in% checkbox_fields) {
          next
        }
      }

      # 6) Convert list to data frame - ensuring all fields have the same length
      for (name in names(payload)) {
        if (length(payload[[name]]) != 1) {
          payload[[name]] <- payload[[name]][1]
        }
      }

      final_data <- as.data.frame(payload, stringsAsFactors = FALSE)

      # Print debugging info
      print("Fields being submitted for section 3:")
      print(names(final_data))

      # 7) Submit to REDCap
      res <- submit_to_redcap(final_data, record_id, url, rdm_token)

      if (!isTRUE(res$success)) {
        showNotification(paste("Save failed:", res$outcome_message), type = "error", duration = NULL)
        return()
      } else {
        showNotification("Self-assessment successfully submitted!", type = "message", duration = 5)
      }

      # Continue to section 4 or completion card based on flow
      if (selected_period() == "Graduating") {
        transition("section3_card", "completion_card")
      } else {
        transition("section3_card", "section4_card")
      }
    }
  })

  observeEvent(input$section4_next, {
    transition("section4_card", "section5_card")
  })

  observeEvent(input$section5_next, {
    # Check the flow path to determine where to go next
    if (rv$flow_path == "entering_residency") {
      # For Entering Residency, skip card 6 and go directly to the final submission
      req(input$goal1, input$goal1_deadline)
      record_id <- fetch_record_id(resident_info(), NULL, url, rdm_token)
      validate(need(record_id, "No record_id found!"))

      # 1) Generate a new repeat instance
      next_inst <- generate_new_instance(record_id,
                                         "s_eval", NULL,
                                         url, rdm_token)

      # 2) Build base tibble
      base <- tibble(
        record_id                = record_id,
        redcap_repeat_instrument = "s_eval",
        redcap_repeat_instance   = next_inst,
        s_e_date                 = format(Sys.Date(), "%Y-%m-%d"),
        s_e_period               = get_ccc_session(selected_period()),
        s_eval_complete          = 0
      )

      # 3) Collect fields for Entering Residency (only card 2 and card 5 data)
      collected_fields <- entering_fields

      # 4) Pull in stored data from responses reactiveValues
      all_responses <- list()
      for (field in collected_fields) {
        if (!is.null(responses[[field]])) {
          all_responses[[field]] <- responses[[field]]
        } else if (!is.null(input[[field]])) {
          all_responses[[field]] <- input[[field]]
        }
      }

      # 5) Add in goal data from card 5
      goal_data <- data.frame(
        goal1                = input$goal1,
        goal1_deadline       = as.character(input$goal1_deadline),
        stringsAsFactors     = FALSE
      )

      # If goal2 and goal3 exist in the form, add them
      if (!is.null(input$goal2) && !is.null(input$goal2_deadline)) {
        goal_data$goal2 <- input$goal2
        goal_data$goal2_deadline <- as.character(input$goal2_deadline)
      }

      if (!is.null(input$goal3) && !is.null(input$goal3_deadline)) {
        goal_data$goal3 <- input$goal3
        goal_data$goal3_deadline <- as.character(input$goal3_deadline)
      }

      # 6) Combine all the data
      df_responses <- as_tibble(all_responses)
      payload <- bind_cols(base, df_responses, goal_data)

      # 7) Submit to REDCap
      res <- submit_to_redcap(payload, record_id, url, rdm_token)

      if (!isTRUE(res$success)) {
        showNotification(paste("Save failed:", res$outcome_message), type = "error")
        return()
      }

      # Update self-assessment completion status
      payload_complete <- data.frame(
        record_id            = record_id,
        self_assess_complete = 1,
        stringsAsFactors     = FALSE
      )
      res_complete <- submit_to_redcap(payload_complete, record_id, url, rdm_token)

      # Show success/error notification
      if (!isTRUE(res_complete$success)) {
        showNotification(paste("Failed to mark assessment as complete:", res_complete$outcome_message), type = "error")
      } else {
        showNotification("Self-assessment successfully completed and submitted!", type = "message")
      }

      # Go directly to completion card from card 5
      transition("section5_card", "completion_card")
    } else {
      # For standard flow, continue to section 6
      transition("section5_card", "section6_card")
    }
  })

  # Keep the original submit event for the standard flow
  observeEvent(input$submit, {
    # This is now only for the standard flow, not for Entering Residency
    req(input$goal1, input$goal1_deadline, rv$flow_path == "standard")
    record_id <- fetch_record_id(resident_info(), NULL, url, rdm_token)
    validate(need(record_id, "No record_id found!"))

    # 1) Generate a new repeat instance
    next_inst <- generate_new_instance(record_id,
                                       "s_eval", NULL,
                                       url, rdm_token)

    # 2) Build base tibble
    base <- tibble(
      record_id                = record_id,
      redcap_repeat_instrument = "s_eval",
      redcap_repeat_instance   = next_inst,
      s_e_date                 = format(Sys.Date(), "%Y-%m-%d"),
      s_e_period               = get_ccc_session(selected_period()),
      s_eval_complete          = 0
    )

    # 3) Collect fields for standard flow (cards 1-6)
    collected_fields <- c(
      # Card 1 fields
      "s_e_plus", "s_e_delta",

      # Card 2 fields (based on period)
      switch(selected_period(),
             "Graduating" = graduating_fields,
             other_fields),

      # Card 3 fields (program feedback)
      program_feedback_fields
    )

    # 4) Pull in stored data from responses reactiveValues
    all_responses <- list()
    for (field in collected_fields) {
      if (!is.null(responses[[field]])) {
        all_responses[[field]] <- responses[[field]]
      } else if (!is.null(input[[field]])) {
        all_responses[[field]] <- input[[field]]
      }
    }

    # 5) Add in goal data from the final goal section
    goal_data <- data.frame(
      goal1                = input$goal1,
      goal1_deadline       = as.character(input$goal1_deadline),
      stringsAsFactors     = FALSE
    )

    # If goal2 and goal3 exist in the form, add them
    if (!is.null(input$goal2) && !is.null(input$goal2_deadline)) {
      goal_data$goal2 <- input$goal2
      goal_data$goal2_deadline <- as.character(input$goal2_deadline)
    }

    if (!is.null(input$goal3) && !is.null(input$goal3_deadline)) {
      goal_data$goal3 <- input$goal3
      goal_data$goal3_deadline <- as.character(input$goal3_deadline)
    }

    # 6) Combine all the data
    df_responses <- as_tibble(all_responses)
    payload <- bind_cols(base, df_responses, goal_data)

    # 7) Submit to REDCap
    res <- submit_to_redcap(payload, record_id, url, rdm_token)

    if (!isTRUE(res$success)) {
      showNotification(paste("Save failed:", res$outcome_message), type = "error")
      return()
    }

    # Update self-assessment completion status
    payload_complete <- data.frame(
      record_id            = record_id,
      self_assess_complete = 1,
      stringsAsFactors     = FALSE
    )
    res_complete <- submit_to_redcap(payload_complete, record_id, url, rdm_token)

    # Show success/error notification
    if (!isTRUE(res_complete$success)) {
      showNotification(paste("Failed to mark assessment as complete:", res_complete$outcome_message), type = "error")
    } else {
      showNotification("Self-assessment successfully completed and submitted!", type = "message")
    }

    # Navigate to completion card
    transition("section6_card", "completion_card")
  })
}
