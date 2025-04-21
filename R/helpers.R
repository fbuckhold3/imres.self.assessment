# Helper functions for UI rendering

# Parse REDCap choice strings into named vectors with error handling
parse_choices <- function(raw) {
  if (is.null(raw) || is.na(raw) || raw == "") {
    warning("Empty choices string to parse")
    return(c("No options available" = "none"))
  }

  result <- tryCatch({
    items <- strsplit(raw, "\\|")[[1]]
    unlist(lapply(items, function(item) {
      parts <- trimws(strsplit(item, ",")[[1]])
      if (length(parts) < 2) return(setNames(parts[1], parts[1]))
      setNames(parts[1], paste(parts[-1], collapse = ","))
    }))
  }, error = function(e) {
    warning("Failed to parse choices: ", raw, " - ", e$message)
    return(c("No options available" = "none"))
  })

  if (length(result) == 0) {
    return(c("No options available" = "none"))
  }
  result
}

# For yes/no fields, provide standard choices
safe_radio_yesno <- function(inputId, label, selected = NULL) {
  safe_radio_buttons(
    inputId,
    label,
    choices = c("Yes" = "1", "No" = "0"),
    inline = TRUE
  )
}

# Safely get field label (fallback to field name)
get_label <- function(fn, dict = rdm_dict) {
  if (length(fn) == 0 || is.null(fn) || is.na(fn)) return("Field name missing")
  lab <- dict %>% filter(field_name == fn) %>% pull(field_label)
  if (length(lab) == 0) fn else lab
}

# Safe wrappers for input controls
safe_radio_buttons <- function(inputId, label, choices, inline = FALSE) {
  tryCatch({
    if (is.null(choices) || length(choices) == 0) {
      return(tags$div(
        class = "alert alert-warning",
        "No options available for", tags$b(inputId)
      ))
    }
    radioButtons(inputId, label, choices = choices, inline = inline)
  }, error = function(e) {
    tags$div(
      class = "alert alert-danger",
      "Error creating radio buttons for", tags$b(inputId), ":", e$message
    )
  })
}

safe_checkbox_group <- function(inputId, label, choices) {
  tryCatch({
    if (is.null(choices) || length(choices) == 0) {
      return(tags$div(
        class = "alert alert-warning",
        "No options available for", tags$b(inputId)
      ))
    }
    checkboxGroupInput(inputId, label, choices = choices)
  }, error = function(e) {
    tags$div(
      class = "alert alert-danger",
      "Error creating checkbox group for", tags$b(inputId), ":", e$message
    )
  })
}

# Conditional panel with safe fallbacks
safe_conditional_panel <- function(condition, content) {
  tryCatch({
    conditionalPanel(condition, content)
  }, error = function(e) {
    tags$div(
      class = "alert alert-warning",
      "Conditional content not shown due to error:", e$message
    )
  })
}

# Find option code by display value
find_option_code <- function(choices, display_value) {
  if (is.null(choices) || length(choices) == 0) return(NULL)
  idx <- which(choices == display_value)
  if (length(idx) == 0) return(NULL)
  names(choices)[idx[1]]
}

# ─── UI Renderers for Entering Residency ───────────────────────────


# Faculty block - modify to use yes/no handling
render_faculty_block <- function(ent) {
  tagList(
    tags$h3("Faculty"),
    safe_radio_yesno(
      "s_e_fac_assist",
      get_label("s_e_fac_assist", ent)
    ),
    safe_conditional_panel(
      "input['s_e_fac_assist'] == '1'",
      tagList(
        textInput("s_e_fac_member", get_label("s_e_fac_member", ent)),
        textInput("s_e_fac_email", get_label("s_e_fac_email", ent))
      )
    )
  )
}


# Goals block
render_goals_block <- function(ent) {
  tagList(
    tags$h3("Top 3 Goals for First 6 Months"),
    textAreaInput("s_e_ume_goal1", get_label("s_e_ume_goal1", ent), rows = 2),
    textAreaInput("s_e_ume_goal2", get_label("s_e_ume_goal2", ent), rows = 2),
    textAreaInput("s_e_ume_goal3", get_label("s_e_ume_goal3", ent), rows = 2)
  )
}

# Preparedness table
render_prep_table <- function(ent) {
  prep <- ent %>% filter(grepl("^s_e_prep_", field_name))

  prep_rows <- lapply(seq_len(nrow(prep)), function(i) {
    fn <- prep$field_name[i]
    lbl <- prep$field_label[i]
    choices <- parse_choices(prep$select_choices_or_calculations[i])

    tags$tr(
      tags$td(lbl, style = "vertical-align:middle; width:70%;"),
      tags$td(safe_radio_buttons(fn, NULL, choices = choices, inline = TRUE),
              style = "width:30%;")
    )
  })

  tagList(
    tags$h3("Preparedness for Internship - How prepared do you feel doing the following activities?"),
    tags$table(class = "table table-bordered", tags$tbody(prep_rows))
  )
}

# Render topic block with conditional Other field
render_topic_block <- function(ent) {
  topic_choices <- parse_choices(
    ent %>% filter(field_name == "s_e_topic_sel") %>% pull(select_choices_or_calculations)
  )

  learn_choices <- parse_choices(
    ent %>% filter(field_name == "s_e_learn_style") %>% pull(select_choices_or_calculations)
  )

  tagList(
    tags$h3("Core Learning Topics"),
    tags$div(
      tags$label(get_label("s_e_topic_sel", ent)),
      tags$p(class="help-block", "Please select up to 3 topics"),
      checkboxGroupInput(
        "s_e_topic_sel",
        NULL, # Label already added above
        choices = topic_choices
      ),
      # Add JavaScript to limit selection to 3 and show/hide Other field
      tags$script(HTML("
        $(document).ready(function() {
          var topicSel = $('#s_e_topic_sel');
          topicSel.on('change', 'input[type=\"checkbox\"]', function() {
            var checked = topicSel.find('input[type=\"checkbox\"]:checked');
            if (checked.length > 3) {
              $(this).prop('checked', false);
              alert('Please select at most 3 topics');
            }

            // Show/hide other field based on selection
            var otherSelected = false;
            checked.each(function() {
              if ($(this).val().indexOf('Other') >= 0 || $(this).parent().text().indexOf('Other') >= 0) {
                otherSelected = true;
              }
            });

            if (otherSelected) {
              $('#s_e_topic_oth').closest('.form-group').show();
            } else {
              $('#s_e_topic_oth').closest('.form-group').hide();
            }
          });

          // Initially hide other field
          $('#s_e_topic_oth').closest('.form-group').hide();
        });
      "))
    ),

    # Other text input field (will be shown/hidden by JavaScript)
    textInput("s_e_topic_oth", get_label("s_e_topic_oth", ent), ""),

    tags$br(),
    tags$h3("Learning Styles"),
    safe_checkbox_group(
      "s_e_learn_style",
      "",
      choices = learn_choices
    ),

    # Add JavaScript for learning styles other field
    tags$script(HTML("
      $(document).ready(function() {
        $('#s_e_learn_style').on('change', 'input[type=\"checkbox\"]', function() {
          var otherSelected = false;
          $('#s_e_learn_style input:checked').each(function() {
            if ($(this).val().indexOf('Other') >= 0 || $(this).parent().text().indexOf('Other') >= 0) {
              otherSelected = true;
            }
          });

          if (otherSelected) {
            $('#s_e_learn_oth').closest('.form-group').show();
          } else {
            $('#s_e_learn_oth').closest('.form-group').hide();
          }
        });

        // Initially hide other field
        $('#s_e_learn_oth').closest('.form-group').hide();
      });
    ")),

    # Other text input field (will be shown/hidden by JavaScript)
    textInput("s_e_learn_oth", get_label("s_e_learn_oth", ent), "")
  )
}

# Render career block with conditional Other field
render_career_block <- function(ent) {
  career_choices <- parse_choices(
    ent %>% filter(field_name == "s_e_career_path") %>% pull(select_choices_or_calculations)
  )

  tagList(
    tags$h3("Career Plans"),
    safe_checkbox_group(
      "s_e_career_path",
      get_label("s_e_career_path", ent),
      choices = career_choices
    ),

    # Add JavaScript for career path other field
    tags$script(HTML("
      $(document).ready(function() {
        $('#s_e_career_path').on('change', 'input[type=\"checkbox\"]', function() {
          var otherSelected = false;
          $('#s_e_career_path input:checked').each(function() {
            if ($(this).val().indexOf('Other') >= 0 || $(this).parent().text().indexOf('Other') >= 0) {
              otherSelected = true;
            }
          });

          if (otherSelected) {
            $('#s_e_career_oth').closest('.form-group').show();
          } else {
            $('#s_e_career_oth').closest('.form-group').hide();
          }
        });

        // Initially hide other field
        $('#s_e_career_oth').closest('.form-group').hide();
      });
    ")),

    # Other text input field (will be shown/hidden by JavaScript)
    textInput("s_e_career_oth", get_label("s_e_career_oth", ent), "")
  )
}

# Fellowship block with conditional Other field
render_fellowship_block <- function(ent) {
  fellowship_choices <- parse_choices(
    ent %>% filter(field_name == "s_e_fellow") %>% pull(select_choices_or_calculations)
  )

  # If fellowship choices not found in dictionary, use default
  if (length(fellowship_choices) == 0) {
    fellowship_choices <- c(
      "Addiction Medicine" = "1",
      "Cardiology" = "2",
      "Critical Care" = "3",
      "Endocrinology" = "4",
      "Gastroenterology" = "5",
      "Geriatrics" = "6",
      "Hematology/Oncology" = "7",
      "Hospice & Palliative" = "8",
      "Hospital Medicine" = "9",
      "ID" = "10",
      "Nephrology" = "11",
      "Pulmonary" = "12",
      "Rheumatology" = "13",
      "Other" = "14"
    )
  }

  tagList(
    # Show fellowship section only when career path includes option "2" (academic)
    safe_conditional_panel(
      "Array.isArray(input['s_e_career_path']) && input['s_e_career_path'].indexOf('2') >= 0",
      tagList(
        tags$h3("Fellowship Interest"),
        safe_checkbox_group(
          "s_e_fellow",
          get_label("s_e_fellow", ent),
          choices = fellowship_choices
        ),

        # Add JavaScript for fellowship other field
        tags$script(HTML("
          $(document).ready(function() {
            $('#s_e_fellow').on('change', 'input[type=\"checkbox\"]', function() {
              var otherSelected = false;
              $('#s_e_fellow input:checked').each(function() {
                if ($(this).val().indexOf('Other') >= 0 || $(this).parent().text().indexOf('Other') >= 0) {
                  otherSelected = true;
                }
              });

              if (otherSelected) {
                $('#s_e_fellow_oth').closest('.form-group').show();
              } else {
                $('#s_e_fellow_oth').closest('.form-group').hide();
              }
            });

            // Initially hide other field
            $('#s_e_fellow_oth').closest('.form-group').hide();
          });
        ")),

        # Other text input field (will be shown/hidden by JavaScript)
        textInput("s_e_fellow_oth", "Please specify:", "")
      )
    )
  )
}
# Track block
render_track_block <- function(ent) {
  track_choices <- parse_choices(
    ent %>% filter(field_name == "s_e_track_type") %>% pull(select_choices_or_calculations)
  )

  tagList(
    tags$h3("Formal Program Tracks"),
    safe_radio_buttons(
      "s_e_track",
      get_label("s_e_track", ent),
      choices = c("Yes" = "1", "No" = "0"),
      inline = TRUE
    ),
    safe_conditional_panel(
      "input['s_e_track'] == '1'",
      safe_checkbox_group(
        "s_e_track_type",
        get_label("s_e_track_type", ent),
        choices = track_choices
      )
    )
  )
}

# ─── UI Renderers for Graduating ───────────────────────────


# Render graduation block with conditional fields based on future plans
render_graduation_block <- function(grad) {
  # Parse the choices for what will be doing in July
  july_choices <- parse_choices(
    grad %>% filter(field_name == "s_e_grad_next") %>% pull(select_choices_or_calculations)
  )

  # Parse fellowship choices
  fellowship_choices <- parse_choices(
    grad %>% filter(field_name == "s_e_grad_fellow") %>% pull(select_choices_or_calculations)
  )

  # Parse location choices for practice
  practice_where_choices <- parse_choices(
    grad %>% filter(field_name == "s_e_grad_where") %>% pull(select_choices_or_calculations)
  )

  # Parse location choices for practice location
  location_choices <- parse_choices(
    grad %>% filter(field_name == "s_e_grad_loc") %>% pull(select_choices_or_calculations)
  )

  # Yes/No choices for SLU fellowship
  slu_choices <- c("Yes" = "1", "No" = "0")

  # Find the exact value for "Other" in the fellowship choices
  other_fellowship_value <- names(fellowship_choices)[grepl("Other", fellowship_choices, ignore.case = TRUE)]
  if (length(other_fellowship_value) == 0) other_fellowship_value <- "Other" # fallback

  tagList(
    tags$h3("Graduation Plans"),

    # What will you be doing in July?
    safe_radio_buttons_empty(
      "s_e_grad_next",
      get_label("s_e_grad_next", grad),
      choices = july_choices
    ),

    # If option "3" (fellowship) is selected
    safe_conditional_panel(
      "input['s_e_grad_next'] === '3'",
      tagList(
        tags$h4("Fellowship Details"),

        # Radio buttons for single selection
        safe_radio_buttons_empty(
          "s_e_grad_fellow",
          get_label("s_e_grad_fellow", grad),
          choices = fellowship_choices
        ),

        # Add a div container for the "Other" text field
        tags$div(
          id = "fellowship-other-container",
          textInput("s_e_grad_fellow_oth", "Please specify fellowship:", "")
        ),

        # Add JavaScript to control the visibility of the "Other" text field
        tags$script(HTML(sprintf("
          $(document).ready(function() {
            // Function to check for Other and show/hide field
            function checkFellowshipOther() {
              var selected = $('input[name=\"s_e_grad_fellow\"]:checked').val();
              if (selected && (selected.indexOf('Other') >= 0 ||
                  $('input[name=\"s_e_grad_fellow\"]:checked').parent().text().indexOf('Other') >= 0)) {
                $('#fellowship-other-container').show();
              } else {
                $('#fellowship-other-container').hide();
              }
            }

            // Run on load and when selection changes
            $('#s_e_grad_fellow input[type=\"radio\"]').on('change', checkFellowshipOther);

            // Initial check
            checkFellowshipOther();

            // Hide initially
            $('#fellowship-other-container').hide();
          });
        "))),

        # Are you at SLU for fellowship?
        safe_radio_buttons_empty(
          "s_e_grad_fellow_at_slu",
          "Are you at SLU for fellowship?",
          choices = slu_choices,
          inline = TRUE
        ),

        # Show location field if not at SLU
        safe_conditional_panel(
          "input['s_e_grad_fellow_at_slu'] === '0'",
          textInput("s_e_grad_fellow_loc_else", "Where is your fellowship?", "")
        )
      )
    ),

    # If option "3" is NOT selected
    safe_conditional_panel(
      "input['s_e_grad_next'] !== '3' && input['s_e_grad_next']",
      tagList(
        tags$h4("Practice/Job Details"),

        # What best describes where you are working? (dropdown)
        selectInput(
          "s_e_grad_where",
          get_label("s_e_grad_where", grad),
          choices = practice_where_choices
        ),

        # Where is your practice? (dropdown)
        selectInput(
          "s_e_grad_loc",
          get_label("s_e_grad_loc", grad),
          choices = location_choices
        ),

        # Show text field if "Somewhere else" (value 4) is selected
        safe_conditional_panel(
          "input['s_e_grad_loc'] === '4'",
          textInput("s_e_grad_loc_other", "Please specify location:", "")
        )
      )
    ),

    # Contact information (always visible)
    tags$h4("Contact Information"),
    textInput("s_e_grad_email", get_label("s_e_grad_email", grad), ""),
    textInput("s_e_grad_phone", get_label("s_e_grad_phone", grad), "")
  )
}

# Remove the unused functions for board-related topics
# (No need to include render_board_concern, render_board_help, render_board_discu)

# Update the main render function to exclude board topics
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
    # Remove board-related fields from graduating_fields if needed
    graduating_fields <- graduating_fields[!graduating_fields %in%
                                             c("s_e_board_concern", "s_e_board_help", "s_e_board_discu")]

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
      render_discussion_block(oth)
    ))
  }
}
# ─── UI Renderers for Other Periods ───────────────────────────

# Conditional panel with safe fallbacks
safe_conditional_panel <- function(condition, content) {
  tryCatch({
    conditionalPanel(condition, content)
  }, error = function(e) {
    tags$div(
      class = "alert alert-warning",
      "Conditional content not shown due to error:", e$message
    )
  })
}

# Render topic block with limited selection (3 max) and conditional Other field
render_other_topics_block <- function(oth) {
  topic_choices <- parse_choices(
    oth %>% filter(field_name == "s_e_topic_sel") %>% pull(select_choices_or_calculations)
  )

  learn_choices <- parse_choices(
    oth %>% filter(field_name == "s_e_learn_style") %>% pull(select_choices_or_calculations)
  )

  # Find the exact value for "Other" in the topic choices
  other_topic_value <- names(topic_choices)[grepl("Other", topic_choices, ignore.case = TRUE)]
  if (length(other_topic_value) == 0) other_topic_value <- "Other" # fallback

  # Find the exact value for "Other" in the learning choices
  other_learn_value <- names(learn_choices)[grepl("Other", learn_choices, ignore.case = TRUE)]
  if (length(other_learn_value) == 0) other_learn_value <- "Other" # fallback

  tagList(
    # Core topics selection with 3 max
    tags$div(
      tags$label(get_label("s_e_topic_sel", oth)),
      tags$p(class="help-block", "Please select exactly 3 topics"),
      checkboxGroupInput(
        "s_e_topic_sel",
        NULL, # Label already added above
        choices = topic_choices
      ),
      # Add JavaScript to limit selection to 3
      tags$script(HTML("
        $(document).ready(function() {
          var topicSel = $('#s_e_topic_sel');
          topicSel.on('change', 'input[type=\"checkbox\"]', function() {
            var checked = topicSel.find('input[type=\"checkbox\"]:checked');
            if (checked.length > 3) {
              $(this).prop('checked', false);
              alert('Please select at most 3 topics');
            }

            // Show/hide other field based on selection
            var otherSelected = false;
            checked.each(function() {
              if ($(this).val().indexOf('Other') >= 0 || $(this).parent().text().indexOf('Other') >= 0) {
                otherSelected = true;
              }
            });

            if (otherSelected) {
              $('#s_e_topic_oth').closest('.form-group').show();
            } else {
              $('#s_e_topic_oth').closest('.form-group').hide();
            }
          });

          // Initially hide other field
          $('#s_e_topic_oth').closest('.form-group').hide();
        });
      "))
    ),

    # Other text input field (will be shown/hidden by JavaScript)
    textInput("s_e_topic_oth", get_label("s_e_topic_oth", oth), ""),

    tags$br(),
    tags$h3("Learning Styles"),
    safe_checkbox_group(
      "s_e_learn_style",
      "",
      choices = learn_choices
    ),

    # Add JavaScript for learning styles other field
    tags$script(HTML("
      $(document).ready(function() {
        $('#s_e_learn_style').on('change', 'input[type=\"checkbox\"]', function() {
          var otherSelected = false;
          $('#s_e_learn_style input:checked').each(function() {
            if ($(this).val().indexOf('Other') >= 0 || $(this).parent().text().indexOf('Other') >= 0) {
              otherSelected = true;
            }
          });

          if (otherSelected) {
            $('#s_e_learn_oth').closest('.form-group').show();
          } else {
            $('#s_e_learn_oth').closest('.form-group').hide();
          }
        });

        // Initially hide other field
        $('#s_e_learn_oth').closest('.form-group').hide();
      });
    ")),

    # Other text input field (will be shown/hidden by JavaScript)
    textInput("s_e_learn_oth", get_label("s_e_learn_oth", oth), "")
  )
}

# Fix career block with conditional Other
render_other_career_block <- function(oth) {
  career_choices <- parse_choices(
    oth %>% filter(field_name == "s_e_career_path") %>% pull(select_choices_or_calculations)
  )

  # Find the exact value for "Other" in the career choices
  other_career_value <- names(career_choices)[grepl("Other", career_choices, ignore.case = TRUE)]
  if (length(other_career_value) == 0) other_career_value <- "Other" # fallback

  tagList(
    tags$h3("Career Path"),
    safe_checkbox_group(
      "s_e_career_path",
      get_label("s_e_career_path", oth),
      choices = career_choices
    ),

    # Add JavaScript for career path other field
    tags$script(HTML("
      $(document).ready(function() {
        $('#s_e_career_path').on('change', 'input[type=\"checkbox\"]', function() {
          var otherSelected = false;
          $('#s_e_career_path input:checked').each(function() {
            if ($(this).val().indexOf('Other') >= 0 || $(this).parent().text().indexOf('Other') >= 0) {
              otherSelected = true;
            }
          });

          if (otherSelected) {
            $('#s_e_career_oth').closest('.form-group').show();
          } else {
            $('#s_e_career_oth').closest('.form-group').hide();
          }
        });

        // Initially hide other field
        $('#s_e_career_oth').closest('.form-group').hide();
      });
    ")),

    # Other text input field (will be shown/hidden by JavaScript)
    textInput("s_e_career_oth", get_label("s_e_career_oth", oth), "")
  )
}

# Fix fellowship block with conditional Other
render_other_tracks_block <- function(oth) {
  fellowship_choices <- parse_choices(
    oth %>% filter(field_name == "s_e_fellow") %>% pull(select_choices_or_calculations)
  )

  # Define track choices
  track_choices <- c("Yes" = "1", "No" = "0")

  # Dynamically pull track type choices from data dictionary
  track_type_choices <- parse_choices(
    oth %>% filter(field_name == "s_e_track_type") %>% pull(select_choices_or_calculations)
  )

  # If no choices found, provide a warning or default
  if (length(track_type_choices) == 0) {
    track_type_choices <- c("Track type options not found in dictionary" = "not_found")
  }

  # Find the exact value for "Other" in the fellowship choices
  other_fellow_value <- names(fellowship_choices)[grepl("Other", fellowship_choices, ignore.case = TRUE)]
  if (length(other_fellow_value) == 0) other_fellow_value <- "Other" # fallback

  tagList(
    # Fellowship section when career path includes option "2"
    safe_conditional_panel(
      "Array.isArray(input['s_e_career_path']) && input['s_e_career_path'].indexOf('2') >= 0",
      tagList(
        tags$h3("Fellowship Interest"),
        safe_checkbox_group(
          "s_e_fellow",
          get_label("s_e_fellow", oth),
          choices = fellowship_choices
        ),

        # Add JavaScript for fellowship other field
        tags$script(HTML("
          $(document).ready(function() {
            $('#s_e_fellow').on('change', 'input[type=\"checkbox\"]', function() {
              var otherSelected = false;
              $('#s_e_fellow input:checked').each(function() {
                if ($(this).val().indexOf('Other') >= 0 || $(this).parent().text().indexOf('Other') >= 0) {
                  otherSelected = true;
                }
              });

              if (otherSelected) {
                $('#s_e_fellow_oth').closest('.form-group').show();
              } else {
                $('#s_e_fellow_oth').closest('.form-group').hide();
              }
            });

            // Initially hide other field
            $('#s_e_fellow_oth').closest('.form-group').hide();
          });
        ")),

        # Other text input field (will be shown/hidden by JavaScript)
        textInput("s_e_fellow_oth", "Please specify:", "")
      )
    ),

    tags$h3("Track Interest"),
    safe_radio_buttons_empty(
      "s_e_track",
      get_label("s_e_track", oth),
      choices = track_choices,
      inline = TRUE
    ),

    # If Yes selected, show multi-select options from dictionary
    safe_conditional_panel(
      "input['s_e_track'] === '1'",
      checkboxGroupInput(
        "s_e_track_type",
        get_label("s_e_track_type", oth),
        choices = track_type_choices
      )
    )
  )
}
# Custom radio buttons with no selection by default
safe_radio_buttons_empty <- function(inputId, label, choices, inline = FALSE) {
  if (length(choices) == 0) {
    return(div(class = "alert alert-warning", "No options available"))
  }
  radioButtons(inputId, label, choices, inline = inline, selected = character(0))
}

# Updated yes/no radio buttons with no default
safe_radio_yesno <- function(inputId, label) {
  safe_radio_buttons_empty(
    inputId,
    label,
    choices = c("Yes" = "1", "No" = "0"),
    inline = TRUE
  )
}



# Add section for mentor discussion
render_discussion_block <- function(oth) {
  tagList(
    tags$h3("Anything else you want to discuss with your mentor"),
    textAreaInput(
      "s_e_discussion",
      "",  # No label needed as the heading is sufficient
      rows = 4,
      value = ""
    )
  )
}

# Updated Step 3 block with no default selections
render_step3_block <- function(oth) {
  tagList(
    tags$h3("USMLE/COMLEX Step 3"),

    # Have you completed Step 3?
    safe_radio_yesno(
      "s_e_step3",
      "Have you completed Step 3 (USMLE or COMLEX)?"
    ),

    # Have you emailed your score? - only if step3 = yes
    safe_conditional_panel(
      "input['s_e_step3'] === '1'",
      safe_radio_yesno(
        "s_e_step3_contact",
        "Have you emailed your score to the program?"
      )
    ),

    # Have you set a date? - only if step3 = no or not answered
    safe_conditional_panel(
      "input['s_e_step3'] !== '1'",
      tagList(
        safe_radio_yesno(
          "s_e_step3_date_set",
          "Have you set a date for Step 3?"
        ),

        # When are you scheduled? (only if date_set is yes)
        safe_conditional_panel(
          "input['s_e_step3_date_set'] === '1'",
          dateInput(
            "s_e_step3_date",
            "When are you scheduled to take Step 3?",
            format = "yyyy-mm-dd",
            value = NULL
          )
        )
      )
    )
  )
}

# Updated Board concerns block with no default selections
render_other_board_block <- function(oth) {
  tagList(
    tags$h3("Board Concerns"),

    # Concerns about failing boards?
    safe_radio_yesno(
      "s_e_board_concern",
      "Do you have any concerns about possibly failing boards at the end of residency?"
    ),

    # Have you discussed this previously? - only if concerns = yes
    safe_conditional_panel(
      "input['s_e_board_concern'] === '1'",
      tagList(
        safe_radio_yesno(
          "s_e_board_help",
          "Have you discussed this prior with the program or reached out for help with standardized exams?"
        ),

        # Who have you discussed with? (only if discussed = yes)
        safe_conditional_panel(
          "input['s_e_board_help'] === '1'",
          textAreaInput(
            "s_e_board_discu",
            "Who have you discussed this with and what steps are being taken to improve your standardized exam performance?",
            rows = 3,
            value = ""
          )
        )
      )
    )
  )
}

# Updated MKSAP block with no default selection
render_mksap_block <- function(oth) {
  mksap_choices <- parse_choices(
    oth %>% filter(field_name == "s_e_mksap_comp") %>% pull(select_choices_or_calculations)
  )

  tagList(
    tags$h3("MKSAP Progress"),
    safe_radio_buttons_empty(
      "s_e_mksap_comp",
      get_label("s_e_mksap_comp", oth),
      choices = mksap_choices,
      inline = TRUE
    )
  )
}


#' Handle the submission of s_eval fields
#'
#' @param fields_to_collect Character vector of field names to submit
#' @param record_id        The REDCap record_id to which these belong
#' @return TRUE on success, FALSE (with notification) on failure
handle_s_eval_submission <- function(fields_to_collect, record_id) {
  # 1) validate record_id
  if (is.null(record_id) || length(record_id)==0) {
    showNotification("Missing record_id—check access code.", type="error")
    return(FALSE)
  }

  # 2) compute next instance via your package
  next_inst <- tryCatch({
    imres::generate_new_instance(
      record_id       = record_id,
      instrument_name = "s_eval",
      redcap_uri      = url,
      token           = rdm_token
    )
  }, error = function(e) {
    message("generate_new_instance error: ", e$message)
    showNotification("Could not calculate form instance; retry.", type="error")
    return(NULL)
  })
  if (is.null(next_inst)) return(FALSE)

  # 3) build the payload
  base <- tibble::tibble(
    record_id                = record_id,
    redcap_repeat_instrument = "s_eval",
    redcap_repeat_instance   = next_inst,
    s_e_date                 = as.character(Sys.Date()),
    s_e_period               = selected_period(),
    s_eval_complete          = 0
  )
  vals <- lapply(fields_to_collect, function(fld) {
    if (!is.null(responses[[fld]])) responses[[fld]] else input[[fld]]
  }) %>% setNames(fields_to_collect)
  payload <- dplyr::bind_cols(base, tibble::as_tibble(vals))

  # 4) expand checkboxes
  prep <- prepare_data_for_submission(payload, record_id)
  df   <- jsonlite::fromJSON(prep$content)

  # 5) submit
  res <- submit_to_redcap(df, record_id, url, rdm_token)
  if (!isTRUE(res$success)) {
    showNotification(paste("Save failed:", res$outcome_message), type="error")
    return(FALSE)
  }
  TRUE
}

#' Mark s_eval complete
#'
#' @param record_id The REDCap record_id
mark_self_assess_complete <- function(record_id) {
  tryCatch({
    comp <- submit_to_redcap(
      data.frame(record_id = record_id, self_assess_complete = 1,
                 stringsAsFactors = FALSE),
      record_id, url, rdm_token
    )
    if (!isTRUE(comp$success)) {
      showNotification(paste("Failed to mark complete:", comp$outcome_message),
                       type="error")
    }
  }, error = function(e) {
    message("mark_self_assess_complete error: ", e$message)
    showNotification("Could not set complete flag.", type="error")
  })
}
