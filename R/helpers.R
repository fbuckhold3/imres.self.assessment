# Helper functions for UI rendering
#
#
# ── Helper field sets ─────────────────────────────────────────────
# Exactly the UME fields you want for "Entering Residency"


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
    tags$h3("Top 3 Goals for First 6 Months - please take a few moments to outline a few goals you have entering residency"),
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
      tags$p(class="help-block", "The following are a number of core internal medicine topics; please select the top 3 which you feel least confident about in your practice? (Select three)"),
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
    tags$h3("What types of paths are you considering in your career. You may select multiple"),
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
        tags$h3("What fellowships are you interested in?"),
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
    # Define the subpages
    subpages <- list(
      list(
        title = "Career Planning and Mentorship",
        description = "This section focuses on your career goals, mentorship needs, and program involvement.",
        content = tagList(
          div(
            class = "section-container",
            div(
              class = "section-header",
              h4("Faculty Mentorship and Career Goals"),
              p("Your responses here help us match you with faculty mentors and track your professional development.")
            ),
            div(
              class = "question-group",
              render_faculty_block(ent)
            ),
            tags$hr(),
            div(
              class = "section-header mt-4",
              h4("Goals and Career Planning"),
              p("Setting clear goals helps guide your residency experience and future career path.")
            ),
            div(
              class = "question-group",
              render_goals_block(ent)
            ),
            div(
              class = "question-group",
              render_career_block(ent)
            ),
            div(
              class = "question-group",
              render_fellowship_block(ent)
            ),
            div(
              class = "question-group",
              render_track_block(ent)
            )
          )
        )
      ),
      list(
        title = "Clinical Preparedness and Learning Preferences",
        description = "Help us understand your comfort level with various clinical scenarios and preferred learning methods.",
        content = tagList(
          div(
            class = "section-container",
            div(
              class = "section-header",
              h4("Clinical Preparedness Assessment"),
              p("Rate your comfort level with various clinical scenarios to help us tailor your learning experience.")
            ),
            div(
              class = "question-group",
              render_prep_table(ent)
            ),
            tags$hr(),
            div(
              class = "section-header mt-4",
              h4("Learning Topics and Preferences"),
              p("Your input helps us customize educational content to your needs and learning style.")
            ),
            div(
              class = "question-group",
              render_topic_block(ent)
            )
          )
        )
      )
    )
    return(subpages)
  } else if (period == "Graduating") {
    return(list(
      list(
        title = "Graduation Planning",
        description = "Help us understand your post-graduation plans and maintain contact.",
        content = tagList(
          div(
            class = "section-container",
            div(
              class = "question-group",
              render_next_steps_block(rdm_dict, graduating_fields)
            ),
            tags$hr(),
            div(
              class = "question-group",
              render_contact_block(rdm_dict)
            )
          )
        )
      )
    ))
  } else {
    # Regular check-ins
    oth <- rdm_dict %>% filter(form_name == "s_eval", field_name %in% other_fields)
    return(list(
      list(
        title = "Academic Progress",
        description = "The following are a number of questions about your learning progress in the Program.",
        content = tagList(
          div(
            class = "section-container",
            div(
              class = "question-group",
              render_other_topics_block(oth)
            ),
            div(
              class = "question-group",
              render_step3_block(oth)
            ),
            div(
              class = "question-group",
              render_other_board_block(oth)
            ),
            div(
              class = "question-group",
              render_mksap_block(oth)
            )
          )
        )
      ),
      list(
        title = "Career Development",
        description = "Review and update your career goals and training track preferences.",
        content = tagList(
          div(
            class = "section-container",
            div(
              class = "question-group",
              render_other_career_block(oth)
            ),
            div(
              class = "question-group",
              render_other_tracks_block(oth)
            )
          )
        )
      ),
      list(
        title = "Additional Discussion Topics",
        description = "Share any other topics you'd like to discuss with your mentor.",
        content = tagList(
          div(
            class = "section-container",
            div(
              class = "question-group",
              render_discussion_block(oth)
            )
          )
        )
      )
    ))
  }
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
      tags$p(class="help-block", "The following are a number of core internal medicine topics; please select the top 3 which you feel least confident about in your practice? (Select three)"),
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
    tags$h3("What career(s) aspects of IM are you considering or planning to go into"),
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
        tags$h3("What Fellowship(s) are you interested in or planning to go into (may select more than one)"),
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

render_card3_ui <- function(rdm_dict) {
  # Filter dictionary to get fields for this card
  card_data <- rdm_dict %>% filter(form_name == "s_eval", field_name %in% program_feedback_fields)

  div(
    class = "section-container",
    # Title and introduction
    div(
      class = "section-header",
      h3("Program Feedback"),
      HTML("
        <p>What are your thoughts about your experiences in the program?
        <em>Please note, although this is in your self-evaluation, this data will be extracted
        anonymously and collated for the entire program for the Program Evaluation Committee to review.</em></p>
      ")
    ),

    # Positive experiences
    div(
      class = "question-group",
      HTML("<p class='question-header'>Are there certain rotations / conferences / or other experiences that positively contribute to your training?</p>"),
      textAreaInput(
        "s_e_prog_plus",
        NULL,
        "",
        width = "100%",
        height = "150px"
      )
    ),

    # Experiences needing improvement
    div(
      class = "question-group",
      HTML("<p class='question-header'>Are there certain rotations / conferences / or other experiences that need improvement?</p>"),
      textAreaInput(
        "s_e_prog_delta",
        NULL,
        "",
        width = "100%",
        height = "150px"
      )
    ),

    # Conference and Grand Rounds attendance
    div(
      class = "question-group",
      HTML("<p class='question-header'>Regarding conference and Grand Rounds attendence - in what circumstances is it easy to attend? Hard? What steps would you like to see the program try?</p>"),
      textAreaInput(
        "s_e_progconf",
        NULL,
        "",
        width = "100%",
        height = "150px"
      )
    ),

    # Other program feedback
    div(
      class = "question-group",
      HTML("<p class='question-header'>Are there any other systems issues within the program that you want to bring to attention or other feedback for improvement?</p>"),
      textAreaInput(
        "s_e_progfeed",
        NULL,
        "",
        width = "100%",
        height = "150px"
      )
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
  # look up the numeric period
  period_code <- period_map[selected_period()]
  if (is.na(period_code)) {
    # fallback if somehow the label isn’t in the map
    period_code <- as.numeric(selected_period())
  }

  # build the base tibble with numeric code
  base <- tibble::tibble(
    record_id                = record_id,
    redcap_repeat_instrument = "s_eval",
    redcap_repeat_instance   = next_inst,
    s_e_date                 = as.character(Sys.Date()),
    s_e_period               = period_code,
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



# Process form inputs for REDCap submission
process_form_inputs <- function(inputs, valid_fields, checkbox_fields = NULL, checkbox_options = NULL) {
  payload <- list()

  # Process regular fields
  for (field in names(inputs)) {
    # Skip fields with null values or UI-only fields
    if (is.null(inputs[[field]]) || grepl("^(btn_|section|card)", field))
      next

    # Add validation for email field if it exists
    if (field == "s_e_fac_email" && !is.null(inputs[[field]])) {
      # Check if it's a valid email format
      if (!grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", inputs[[field]])) {
        next
      }
    }

    # Process regular (non-checkbox) fields
    if (field %in% valid_fields && !(field %in% checkbox_fields)) {
      # Handle different types of inputs correctly
      if (length(inputs[[field]]) > 1) {
        # For multi-select inputs, join with commas
        payload[[field]] <- paste(inputs[[field]], collapse = ",")
      } else {
        # For single value inputs
        payload[[field]] <- inputs[[field]]
      }
    }
  }

  # Process checkbox fields
  if (!is.null(checkbox_fields) && !is.null(checkbox_options)) {
    for (cb_field in checkbox_fields) {
      if (!is.null(inputs[[cb_field]])) {
        # Get the selected values
        selected_values <- inputs[[cb_field]]

        # Clean up the values
        if (is.character(selected_values)) {
          if (any(grepl(",", selected_values))) {
            selected_codes <- sapply(selected_values, function(val) {
              gsub("\\s*,.*$", "", val)
            })
          } else {
            selected_codes <- selected_values
          }
        } else {
          selected_codes <- as.character(selected_values)
        }

        # Process each option for this checkbox field
        if (!is.null(checkbox_options[[cb_field]])) {
          for (option_code in checkbox_options[[cb_field]]) {
            field_name <- paste0(cb_field, "___", option_code)
            clean_option <- gsub("\"", "", option_code)

            if (any(selected_codes %in% clean_option)) {
              payload[[field_name]] <- "1"
            } else {
              payload[[field_name]] <- "0"
            }
          }
        }
      } else {
        # Set all options to 0 if not selected
        if (!is.null(checkbox_options[[cb_field]])) {
          for (option_code in checkbox_options[[cb_field]]) {
            field_name <- paste0(cb_field, "___", option_code)
            payload[[field_name]] <- "0"
          }
        }
      }
    }
  }

  return(payload)
}

# Extract checkbox information from REDCap dictionary
extract_checkbox_info <- function(rdm_dict, form_name) {
  # Get the dictionary for this form
  form_dict <- rdm_dict %>% filter(form_name == form_name)

  # Get checkbox fields
  checkbox_fields <- form_dict %>%
    filter(field_type == "checkbox") %>%
    pull(field_name)

  # For each checkbox field, extract options
  checkbox_options <- list()
  for (cb_field in checkbox_fields) {
    # Get the select_choices_or_calculations
    choices_text <- form_dict %>%
      filter(field_name == cb_field) %>%
      pull(select_choices_or_calculations) %>%
      first()

    if (!is.null(choices_text) && choices_text != "") {
      # Parse the choices
      choices <- strsplit(choices_text, "\\|")[[1]]
      codes <- sapply(choices, function(x) {
        trim_code <- trimws(gsub(",.*$", "", x))
        return(trim_code)
      })
      checkbox_options[[cb_field]] <- codes
    }
  }

  return(list(
    fields = checkbox_fields,
    options = checkbox_options
  ))
}

# Prepare REDCap payload
prepare_redcap_payload <- function(record_id, instrument, instance, period, input_data, valid_fields, responses = NULL) {
  # Base data
  payload <- list(
    record_id = record_id,
    redcap_repeat_instrument = instrument,
    redcap_repeat_instance = instance,
    s_e_date = format(Sys.Date(), "%Y-%m-%d"),
    s_e_period = period
  )

  # Add form complete status
  form_complete_field <- paste0(instrument, "_complete")
  payload[[form_complete_field]] <- 0

  # Combine with input data
  for (field in names(input_data)) {
    if (field %in% valid_fields) {
      payload[[field]] <- input_data[[field]]
    }
  }

  # Add responses if provided
  if (!is.null(responses)) {
    for (field in names(responses)) {
      if (!is.null(responses[[field]]) && field %in% valid_fields) {
        payload[[field]] <- responses[[field]]
      }
    }
  }

  # Ensure all fields have length 1
  for (name in names(payload)) {
    if (length(payload[[name]]) != 1) {
      payload[[name]] <- payload[[name]][1]
    }
  }

  return(as.data.frame(payload, stringsAsFactors = FALSE))
}

submit_to_redcap_with_period_check <- function(record_id, instrument, period_text, data, url, token) {
  # Create correct mapping between text periods and their numeric equivalents
  period_mapping <- c(
    "Entering Residency" = 1,  # This was missing in your original mapping
    "Mid Intern" = 2,          # These values were all off by 1
    "End Intern" = 3,
    "Mid PGY2" = 4,
    "End PGY2" = 5,
    "Mid PGY3" = 6,
    "Graduating" = 7,
    "Interim Review" = 8       # Added this if needed
  )

  # Convert the text period to its numeric equivalent
  # Fix: Use exact matching to prevent vector results
  period_numeric <- as.numeric(period_mapping[period_text])

  # If period_text isn't in our mapping, try to convert it directly
  if (length(period_numeric) != 1 || is.na(period_numeric)) {
    if (!is.na(suppressWarnings(as.numeric(period_text)))) {
      period_numeric <- as.numeric(period_text)
    } else {
      message("Warning: Unknown period text '", period_text, "'. Using as-is.")
      period_numeric <- period_text
    }
  }

  message("Checking for existing instances with period: ", period_text,
          " (numeric value: ", period_numeric, ") for record_id: ", record_id)

  # Make sure the period is set correctly in the data
  data$s_e_period <- period_numeric

  # Get all existing data for this record/instrument
  response <- httr::POST(
    url = url,
    body = list(
      token = token,
      content = "record",
      action = "export",
      format = "json",
      type = "flat",
      records = record_id,
      forms = instrument,
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      exportSurveyFields = "false",
      exportDataAccessGroups = "false",
      returnFormat = "json"
    ),
    encode = "form"
  )

  # Initialize variables
  existing_data <- NULL
  existing_instances <- data.frame(instance = integer(0), period = integer(0))
  matching_instance <- NULL

  # Process response
  if (httr::status_code(response) == 200) {
    response_text <- httr::content(response, "text", encoding = "UTF-8")

    if (response_text != "" && response_text != "[]") {
      tryCatch({
        existing_data <- jsonlite::fromJSON(response_text)
        message("Retrieved existing data from REDCap:")
        print(existing_data)

        # Build a list of existing instances and their periods
        if (is.data.frame(existing_data) && nrow(existing_data) > 0) {
          for (i in 1:nrow(existing_data)) {
            instance_num <- i  # Default to row number

            # If redcap_repeat_instance exists, use that
            if ("redcap_repeat_instance" %in% names(existing_data)) {
              instance_num <- as.numeric(existing_data$redcap_repeat_instance[i])
            }

            # If s_e_period exists, record it
            if ("s_e_period" %in% names(existing_data)) {
              # Fix: Ensure we're dealing with a single value for each row
              period_num <- as.numeric(existing_data$s_e_period[i])

              # Add to our tracking dataframe
              existing_instances <- rbind(existing_instances,
                                          data.frame(instance = instance_num,
                                                     period = period_num))
            }
          }
        }

        # Look for a match on period
        if (nrow(existing_instances) > 0) {
          message("Found existing instances:")
          print(existing_instances)

          # Find the matching period
          for (i in 1:nrow(existing_instances)) {
            # Fix: Ensure we're comparing single values
            current_period <- existing_instances$period[i]
            if (!is.na(current_period) &&
                !is.na(period_numeric) &&
                length(current_period) == 1 &&
                length(period_numeric) == 1 &&
                current_period == period_numeric) {
              matching_instance <- existing_instances$instance[i]
              message("Found matching period ", period_numeric, " in instance ", matching_instance)
              break
            }
          }
        }
      }, error = function(e) {
        message("Error processing REDCap data: ", e$message)
      })
    } else {
      message("No data returned from REDCap API")
    }
  } else {
    message("Error fetching data: ", httr::status_code(response), " - ",
            httr::content(response, "text"))
  }

  # If we found a matching instance, update it
  if (!is.null(matching_instance)) {
    message("Found matching instance with period ", period_numeric, ": ", matching_instance, ". Updating this instance.")

    # Set the instance number in the data
    data$redcap_repeat_instance <- matching_instance
    data$redcap_repeat_instrument <- instrument

    # Submit the data to REDCap using direct API call
    return(direct_redcap_import(data, record_id, url, token))
  }

  # If we didn't find a matching instance, create a new one
  message("No matching instance found for period ", period_numeric, ". Creating a new instance.")

  # Find the next available instance number
  next_instance <- 1
  if (nrow(existing_instances) > 0) {
    next_instance <- max(existing_instances$instance) + 1
  }

  message("Using instance number: ", next_instance, " for new record")

  # Set the instance number in the data
  data$redcap_repeat_instance <- next_instance
  data$redcap_repeat_instrument <- instrument

  # Submit the new instance to REDCap
  return(direct_redcap_import(data, record_id, url, token))
}

# Function to directly import data to REDCap without relying on REDCapR
direct_redcap_import <- function(data, record_id, url, token) {
  # Ensure data has the correct structure for repeating instruments
  if (!"redcap_repeat_instrument" %in% names(data)) {
    message("Adding redcap_repeat_instrument to data")
    data$redcap_repeat_instrument <- "s_eval"  # Make sure this matches your form name exactly
  }

  if (!"redcap_repeat_instance" %in% names(data)) {
    message("Adding redcap_repeat_instance to data")
    data$redcap_repeat_instance <- 1  # Default to instance 1 if not specified
  }

  # Ensure record_id is in the data
  if (!"record_id" %in% names(data)) {
    message("Adding record_id to data")
    data$record_id <- record_id
  }

  # Ensure all data is character type for REDCap
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  for (col in names(data)) {
    data[[col]] <- as.character(data[[col]])
  }

  # Convert data frame to JSON
  data_json <- jsonlite::toJSON(data, auto_unbox = TRUE)

  # Log the data being sent to REDCap for debugging
  message("Sending data to REDCap:")
  print(data)

  # Submit to REDCap API
  response <- httr::POST(
    url = url,
    body = list(
      token = token,
      content = "record",
      action = "import",
      format = "json",
      type = "flat",
      overwriteBehavior = "normal",
      forceAutoNumber = "false",
      data = data_json,
      returnContent = "count",
      returnFormat = "json"
    ),
    encode = "form"
  )

  # Check response
  if (httr::status_code(response) == 200) {
    response_content <- httr::content(response, "text", encoding = "UTF-8")
    message("REDCap API response: ", response_content)

    return(list(
      success = TRUE,
      outcome_message = paste("Successfully submitted data for record", record_id)
    ))
  } else {
    error_message <- httr::content(response, "text", encoding = "UTF-8")
    message("Error submitting to REDCap: ", error_message)

    return(list(
      success = FALSE,
      outcome_message = paste("Failed to submit data for record", record_id, ":", error_message)
    ))
  }
}





#' Render the “Next Steps” block for the Graduating section
#'
#' @param rdm_dict A data frame of your REDCap dictionary
#' @param graduating_fields A character vector of field_names to include for Graduating
#' @return A tagList of UI elements
render_next_steps_block <- function(rdm_dict, graduating_fields) {
  gdf <- rdm_dict %>%
    filter(form_name == "s_eval", field_name %in% graduating_fields)

  tagList(
    h3("Graduation Plans"),
    selectInput(
      "s_e_grad_next",
      gdf$field_label[gdf$field_name == "s_e_grad_next"],
      choices = parse_choices(
        gdf$select_choices_or_calculations[gdf$field_name == "s_e_grad_next"]
      )
    ),
    conditionalPanel(
      condition = "input.s_e_grad_next == '3'",
      checkboxGroupInput(
        "s_e_grad_fellow",
        gdf$field_label[gdf$field_name == "s_e_grad_fellow"],
        choices = parse_choices(
          gdf$select_choices_or_calculations[gdf$field_name == "s_e_grad_fellow"]
        )
      )
    ),
    conditionalPanel(
      condition = "input.s_e_grad_fellow.indexOf('12') >= 0",
      textInput(
        "s_e_grad_fellow_oth",
        gdf$field_label[gdf$field_name == "s_e_grad_fellow_oth"]
      )
    ),
    conditionalPanel(
      condition = "input.s_e_grad_next == '4'",
      textInput(
        "s_e_grad_next_othe",
        gdf$field_label[gdf$field_name == "s_e_grad_next_othe"]
      )
    ),
    conditionalPanel(
      condition = "input.s_e_grad_next == '1' || input.s_e_grad_next == '2'",
      tagList(
        selectInput(
          "s_e_grad_where",
          gdf$field_label[gdf$field_name == "s_e_grad_where"],
          choices = parse_choices(
            gdf$select_choices_or_calculations[gdf$field_name == "s_e_grad_where"]
          )
        ),
        selectInput(
          "s_e_grad_loc",
          gdf$field_label[gdf$field_name == "s_e_grad_loc"],
          choices = parse_choices(
            gdf$select_choices_or_calculations[gdf$field_name == "s_e_grad_loc"]
          )
        ),
        conditionalPanel(
          condition = "input.s_e_grad_loc == '4'",
          textInput(
            "s_e_grad_loc_other",
            gdf$field_label[gdf$field_name == "s_e_grad_loc_other"]
          )
        )
      )
    )
  )
}

# The contact-block function doesn’t need the graduating_fields argument:
render_contact_block <- function(rdm_dict) {
  cdf <- rdm_dict %>%
    filter(form_name == "s_eval",
           field_name %in% c("s_e_grad_email","s_e_grad_phone"))

  tagList(
    h3("Future Contact Information"),
    textInput(
      "s_e_grad_email",
      cdf$field_label[cdf$field_name == "s_e_grad_email"]
    ),
    textInput(
      "s_e_grad_phone",
      cdf$field_label[cdf$field_name == "s_e_grad_phone"]
    )
  )
}




# Helper function to convert Yes/No to numeric values
convert_yes_no <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NULL)
  }
  if (is.numeric(value)) {
    return(value)  # Already numeric
  }
  if (tolower(as.character(value)) == "yes") {
    return(1)
  } else if (tolower(as.character(value)) == "no") {
    return(0)
  }
  return(value)  # Return original if not Yes/No
}

# Modified handle_scholarship_submission to convert Yes/No fields and always create new instances
handle_scholarship_submission <- function(record_id, values) {
  # Convert any Yes/No values to numeric
  yes_no_fields <- c("schol_pres", "schol_pub", "schol_ps", "schol_rca")
  for (field in yes_no_fields) {
    if (field %in% names(values)) {
      values[[field]] <- convert_yes_no(values[[field]])
    }
  }

  # Check for and fix the field name issue
  if ("type" %in% names(values)) {
    # Rename "type" to "schol_type" if it exists
    type_value <- values[["type"]]
    values[["type"]] <- NULL  # Remove the incorrect field

    # Only add schol_type if it doesn't already exist
    if (!("schol_type" %in% names(values))) {
      values[["schol_type"]] <- type_value
    }
  }

  # 1) figure out next instance - ALWAYS CREATE A NEW INSTANCE FOR SCHOLARSHIP
  next_inst <- tryCatch({
    generate_instance_2(
      record_id = record_id,
      instrument_name = "scholarship",
      coach_data = NULL,
      redcap_uri = url,
      token = rdm_token
    )
  }, error = function(e) {
    message("Error generating instance: ", e$message)
    return(1) # Default to 1 if there's an error
  })

  # 2) build a one‐row data.frame
  df <- tibble::tibble(
    record_id = record_id,
    redcap_repeat_instrument = "scholarship",
    redcap_repeat_instance = next_inst
  )

  # Ensure all values are vectors before binding
  values_list <- list()
  for (name in names(values)) {
    if (is.null(values[[name]])) {
      # Skip NULL values
      next
    } else if (!is.vector(values[[name]])) {
      # Convert non-vectors to character
      values_list[[name]] <- as.character(values[[name]])
    } else {
      values_list[[name]] <- values[[name]]
    }
  }

  # Convert to a tibble only if we have valid values
  if (length(values_list) > 0) {
    values_tibble <- tibble::as_tibble(values_list)
    df2 <- dplyr::bind_cols(df, values_tibble)
  } else {
    df2 <- df
  }

  # Log what we're submitting for debugging
  message("Submitting scholarship data with instance ", next_inst)
  print(df2)

  # 3) submit directly to REDCap without using submit_to_redcap_with_period_check
  result <- tryCatch({
    redcap_write(
      ds = df2,
      redcap_uri = url,
      token = rdm_token
    )
  }, error = function(e) {
    message("Error writing to REDCap: ", e$message)
    return(list(success = FALSE, outcome_message = e$message))
  })

  # Process result
  if (isTRUE(result$success)) {
    message("Successfully submitted scholarship data, created instance ", next_inst)
    return(list(
      success = TRUE,
      outcome_message = paste("Successfully created scholarship entry (instance ", next_inst, ")", sep = "")
    ))
  } else {
    message("Failed to submit scholarship data: ", result$outcome_message)
    return(list(
      success = FALSE,
      outcome_message = result$outcome_message
    ))
  }
}

# Improved helper function that creates a new instance every time for scholarship items
generate_instance_2 <- function(record_id, instrument_name, coach_data = NULL,
                                redcap_uri, token) {
  message("Generating new instance for record_id: ", record_id,
          " and instrument: ", instrument_name)

  if (!is.null(coach_data)) {
    message("Using provided coach_data")
    # Filter to just the relevant instrument
    inst_data <- coach_data[coach_data$redcap_repeat_instrument == instrument_name, ]

    if (nrow(inst_data) > 0) {
      next_instance <- max(inst_data$redcap_repeat_instance, na.rm = TRUE) + 1
      return(next_instance)
    } else {
      return(1)
    }
  } else {
    message("coach_data is NULL. Falling back to API call.")
    # Pull data directly from REDCap without the problematic parameters
    tryCatch({
      data <- redcap_read(
        redcap_uri = redcap_url,
        token = token,
        records = record_id
      )$data

      if (nrow(data) == 0) {
        return(1)
      }

      # Filter to just the instances for this instrument
      inst_data <- data[data$redcap_repeat_instrument == instrument_name, ]

      if (nrow(inst_data) == 0) {
        return(1)
      } else {
        # Always increment by 1 from the highest instance number
        next_instance <- max(inst_data$redcap_repeat_instance, na.rm = TRUE) + 1
        message("Determined next instance: ", next_instance)
        return(next_instance)
      }
    }, error = function(e) {
      message("Error pulling REDCap data: ", e$message)
      return(1)
    })
  }
}


# Function to get the previous period based on current selection
get_previous_period <- function(current_period) {
  period_mapping <- c(
    "Entering Residency" = NA,  # No previous period
    "Mid Intern" = "Entering Residency",
    "End Intern" = "Mid Intern",
    "Mid PGY2" = "End Intern",
    "End PGY2" = "Mid PGY2",
    "Mid PGY3" = "End PGY2",
    "Graduating" = "Mid PGY3"
  )

  return(period_mapping[current_period])
}

# Function to determine if program data exists for a period
# Entering Residency has no p_miles data
has_program_data <- function(period) {
  return(period != "Entering Residency")
}

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

get_median_data <- function(s_miles, current_period) {
  # Extract median data for the current period
  median_data <- s_miles %>%
    filter(name == "Median" & period == current_period)

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


# Function to process scholarship data
process_scholarship_data <- function(data, record_id, rdm_dict) {
  # Filter by record_id
  filtered_data <- data %>%
    filter(record_id == record_id)

  # Skip processing if no data is found
  if (nrow(filtered_data) == 0) {
    return(list(
      table_data = data.frame(
        Scholarship_Type = character(),
        Description = character(),
        stringsAsFactors = FALSE
      ),
      completed_ps = FALSE,
      completed_rca = FALSE
    ))
  }

  # Get the labels for schol_type from data dictionary
  schol_type_labels <- rdm_dict %>%
    filter(field_name == "schol_type") %>%
    pull(select_choices_or_calculations) %>%
    strsplit("\\|") %>%
    unlist() %>%
    trimws() %>%
    sapply(function(x) {
      parts <- strsplit(x, ", ")[[1]]
      code <- as.numeric(parts[1])
      label <- parts[2]
      return(c(code = code, label = label))
    }) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)

  # Convert codes to numeric for matching
  schol_type_labels$code <- as.numeric(schol_type_labels$code)

  # Create a new dataframe with the processed data
  result <- filtered_data %>%
    mutate(
      # Map schol_type to labels
      Scholarship_Type = sapply(schol_type, function(code) {
        if (is.na(code)) return(NA)
        label_row <- schol_type_labels[schol_type_labels$code == code, ]
        if (nrow(label_row) > 0) return(label_row$label[1])
        return(paste("Type", code))
      }),

      # Process PS and RCA fields - use vectorized operations
      PS_Text = ifelse(is.na(schol_ps), NA,
                       ifelse(schol_ps == 1, "You have completed a Patient Safety Review", NA)),

      RCA_Text = ifelse(is.na(schol_rca), NA,
                        ifelse(schol_rca == 1, "You have completed a Root Cause Analysis", NA)),

      # Coalesce other fields - need to handle NA values properly
      Other_Description = pmap_chr(list(schol_qi, schol_res, schol_cit), function(qi, res, cit) {
        values <- c(qi, res, cit)
        for (val in values) {
          if (!is.na(val) && val != "") return(val)
        }
        return("")
      })
    ) %>%
    # Combine Description fields
    mutate(
      Description = apply(cbind(PS_Text, RCA_Text, Other_Description), 1, function(row) {
        parts <- row[!is.na(row) & row != ""]
        if(length(parts) == 0) return("")
        paste(parts, collapse = " ")
      })
    ) %>%
    # Select only the fields we need
    select(Scholarship_Type, Description) %>%
    # Remove rows with NA in Scholarship_Type
    filter(!is.na(Scholarship_Type))

  # Determine if user has completed Patient Safety Review or Root Cause Analysis
  completed_ps <- any(!is.na(filtered_data$schol_ps) & filtered_data$schol_ps == 1)
  completed_rca <- any(!is.na(filtered_data$schol_rca) & filtered_data$schol_rca == 1)

  # Create a clean table without the PS/RCA messages (they'll be shown separately)
  clean_table <- result %>%
    mutate(
      Description = gsub("You have completed a Patient Safety Review", "", Description),
      Description = gsub("You have completed a Root Cause Analysis", "", Description),
      Description = trimws(Description)
    )

  # Return both the processed data and the completion flags
  return(list(
    table_data = clean_table,
    completed_ps = completed_ps,
    completed_rca = completed_rca
  ))
}

# Helper lists for competencies and milestones


goalSettingUI <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Goal Setting"),

    # PC/MK Section
    div(
      class = "goal-section",
      h4("Goal 1: Patient Care / Medical Knowledge"),

      # Check-in section
      wellPanel(
        h4("Previous Goal Review"),
        checkboxInput(ns("prior_goal_pcmk"), "Did you have a previous goal in this domain?", FALSE),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("prior_goal_pcmk")),
          textAreaInput(ns("review_q_pcmk"),
                        "What progress did you make on your previous goal in this domain?",
                        width = "100%"),
          textAreaInput(ns("review_q2_pcmk"),
                        "What factors helped or hindered your progress?",
                        width = "100%")
        )
      ),

      # Goal selection section - only show after check-in is completed
      conditionalPanel(
        condition = sprintf("input['%s'] == false || (input['%s'] == true && input['%s'] != '' && input['%s'] != '')",
                            ns("prior_goal_pcmk"), ns("prior_goal_pcmk"), ns("review_q_pcmk"), ns("review_q2_pcmk")),
        selectInput(
          ns("goal_pcmk"),
          label = "Based on the subcompetencies for Patient Care and Medical Knowledge,
                  which subcompetency do you want to focus on improving over the next 6 months?",
          choices = NULL,
          selected = NULL
        ),
        uiOutput(ns("selected_pcmk_name")),
        uiOutput(ns("pcmk_milestone_table")),

        # Show "how" text after goal selection
        conditionalPanel(
          condition = sprintf("input['%s'] != null", ns("goal_pcmk")),
          textOutput(ns("how_pcmk"))
        ),

        wellPanel(
          h4("Set PC/MK Goal"),
          fluidRow(
            column(6,
                   selectInput(ns("pcmk_row_select"), "Select Milestone Row:", choices = NULL)
            ),
            column(6,
                   selectInput(ns("pcmk_level_select"), "Target Level:",
                               choices = setNames(1:5, paste0(1:5, ": ", unlist(milestone_levels))))
            )
          ),
          div(
            style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 10px;",
            strong("Selected Goal Text:"),
            textOutput(ns("pcmk_selected_goal")),
            dateInput(ns("pcmk_target_date"), "Target Achievement Date:", value = Sys.Date() + 180),
            selectInput(ns("pcmk_status"), "Goal Status:",
                        choices = c("Not Started", "In Progress", "Achieved", "Revised"))
          )
        )
      )
    ),

    hr(),

    # SBP/PBLI Section (similar structure)
    div(
      class = "goal-section",
      h4("Goal 2: Systems-Based Practice / Practice-Based Learning and Improvement"),

      wellPanel(
        h4("Previous Goal Review"),
        checkboxInput(ns("prior_goal_sbppbl"), "Did you have a previous goal in this domain?", FALSE),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("prior_goal_sbppbl")),
          textAreaInput(ns("review_q_sbppbl"),
                        "What progress did you make on your previous goal in this domain?",
                        width = "100%"),
          textAreaInput(ns("review_q2_sbppbl"),
                        "What factors helped or hindered your progress?",
                        width = "100%")
        )
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == false || (input['%s'] == true && input['%s'] != '' && input['%s'] != '')",
                            ns("prior_goal_sbppbl"), ns("prior_goal_sbppbl"), ns("review_q_sbppbl"), ns("review_q2_sbppbl")),
        # Rest of SBP/PBLI selection UI...
        selectInput(
          ns("goal_sbppbl"),
          label = "Based on the subcompetencies for Systems-Based Practice and Practice-Based Learning and Improvement,
                  which subcompetency do you want to focus on improving over the next 6 months?",
          choices = NULL,
          selected = NULL
        ),
        uiOutput(ns("selected_sbp_pbli_name")),
        uiOutput(ns("sbppbl_milestone_table")),

        conditionalPanel(
          condition = sprintf("input['%s'] != null", ns("goal_sbppbl")),
          textOutput(ns("how_sbppbl"))
        ),

        # Rest of the UI elements...
        wellPanel(
          h4("Set SBP/PBLI Goal"),
          fluidRow(
            column(6,
                   selectInput(ns("sbppbl_row_select"), "Select Milestone Row:", choices = NULL)
            ),
            column(6,
                   selectInput(ns("sbppbl_level_select"), "Target Level:",
                               choices = setNames(1:5, paste0(1:5, ": ", unlist(milestone_levels))))
            )
          ),
          div(
            style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 10px;",
            strong("Selected Goal Text:"),
            textOutput(ns("sbppbl_selected_goal")),
            dateInput(ns("sbppbl_target_date"), "Target Achievement Date:", value = Sys.Date() + 180),
            selectInput(ns("sbppbl_status"), "Goal Status:",
                        choices = c("Not Started", "In Progress", "Achieved", "Revised"))
          )
        )
      )
    ),

    hr(),

    # PROF/ICS Section (similar structure)
    div(
      class = "goal-section",
      h4("Goal 3: Professionalism / Interpersonal and Communication Skills"),

      wellPanel(
        h4("Previous Goal Review"),
        checkboxInput(ns("prior_goal_profics"), "Did you have a previous goal in this domain?", FALSE),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("prior_goal_profics")),
          textAreaInput(ns("review_q_profics"),
                        "What progress did you make on your previous goal in this domain?",
                        width = "100%"),
          textAreaInput(ns("review_q2_profics"),
                        "What factors helped or hindered your progress?",
                        width = "100%")
        )
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == false || (input['%s'] == true && input['%s'] != '' && input['%s'] != '')",
                            ns("prior_goal_profics"), ns("prior_goal_profics"), ns("review_q_profics"), ns("review_q2_profics")),
        # Rest of PROF/ICS selection UI...
        selectInput(
          ns("goal_subcomp_profics"),
          label = "Based on the subcompetencies for Professionalism and Interpersonal and Communication Skills,
                  which subcompetency do you want to focus on improving over the next 6 months?",
          choices = NULL,
          selected = NULL
        ),
        uiOutput(ns("selected_prof_ics_name")),
        uiOutput(ns("profics_milestone_table")),

        conditionalPanel(
          condition = sprintf("input['%s'] != null", ns("goal_subcomp_profics")),
          textOutput(ns("how_profics"))
        ),

        # Rest of the UI elements...
        wellPanel(
          h4("Set PROF/ICS Goal"),
          fluidRow(
            column(6,
                   selectInput(ns("profics_row_select"), "Select Milestone Row:", choices = NULL)
            ),
            column(6,
                   selectInput(ns("profics_level_select"), "Target Level:",
                               choices = setNames(1:5, paste0(1:5, ": ", unlist(milestone_levels))))
            )
          ),
          div(
            style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 10px;",
            strong("Selected Goal Text:"),
            textOutput(ns("profics_selected_goal")),
            dateInput(ns("profics_target_date"), "Target Achievement Date:", value = Sys.Date() + 180),
            selectInput(ns("profics_status"), "Goal Status:",
                        choices = c("Not Started", "In Progress", "Achieved", "Revised"))
          )
        )
      )
    )
  )
}

fetch_previous_goals <- function(record_id, period, url, token) {
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

  # Reverse mapping for finding previous period
  reverse_period_mapping <- setNames(names(period_mapping), period_mapping)

  # Get the numeric code for the current period
  current_period_code <- period_mapping[period]

  # For certain periods, there's no previous data needed
  if (period %in% c("Entering Residency", "Mid Intern", "Graduating")) {
    message(paste("No previous goal data needed for period:", period))
    return(NULL)
  }

  # Define the previous period based on the current one
  previous_period_mapping <- list(
    "2" = "1",  # End Intern -> Mid Intern
    "3" = "2",  # Mid PGY2 -> End Intern
    "4" = "3",  # End PGY2 -> Mid PGY2
    "5" = "4"   # Mid PGY3 -> End PGY2
  )

  previous_period_code <- previous_period_mapping[[current_period_code]]

  if (is.null(previous_period_code)) {
    message(paste("No mapping found for previous period of:", period))
    return(NULL)
  }

  message(paste("Looking for previous goals from period:", reverse_period_mapping[previous_period_code]))

  # Define the milestone fields
  milestone_fields <- c(
    "pc1_r1", "pc1_r2", "pc2_r1", "pc2_r2", "pc3_r1", "pc3_r2", "pc4_r1", "pc4_r2",
    "pc5_r1", "pc5_r2", "pc5_r3", "pc6_r1", "pc6_r2", "mk1_r1", "mk2_r1", "mk3_r1",
    "mk3_r2", "sbp1_r1", "sbp1_r2", "sbp1_r3", "sbp2_r1", "sbp2_r2", "sbp2_r3",
    "sbp3_r1", "sbp3_r2", "pbl1_r1", "pbl2_r1", "pbl2_r2", "pbl2_r3", "prof1_r1",
    "prof2_r1", "prof3_r1", "prof4_r1", "prof4_r2", "ics1_r1", "ics1_r2", "ics2_r1",
    "ics2_r2", "ics3_r1", "ics3_r2"
  )

  # Add basic fields needed for display
  basic_fields <- c(
    "record_id", "ilp_period", "year_resident",
    "how_pcmk", "how_sbppbl", "how_profics"
  )

  # Combine all fields
  all_fields <- c(basic_fields, milestone_fields)

  # Try to pull from the ilp form
  tryCatch({
    # Use forms_api_pull to retrieve data from the "ilp" form
    data <- forms_api_pull(
      token = token,
      url = url,
      "ilp",  # The form name for goals
      fields = all_fields
    )

    # Filter for the specific record_id and previous period
    if (!is.null(record_id) && record_id != "") {
      data <- data %>% dplyr::filter(record_id == record_id)
    }

    data <- data %>% dplyr::filter(ilp_period == previous_period_code)

    # If no data found for the previous period
    if (nrow(data) == 0) {
      message(paste("No goal data found for previous period:", reverse_period_mapping[previous_period_code]))
      return(NULL)
    }

    # Return the filtered data
    return(data)
  }, error = function(e) {
    message(paste("Error fetching previous goals:", e$message))
    return(NULL)
  })
}


goalSettingServer <- function(id, rdm_dict_data, subcompetency_maps, competency_list, milestone_levels) {
  moduleServer(id, function(input, output, session) {

    # Update all dropdowns when dictionary is loaded
    observe({
      update_goal_dropdown(session, "goal_pcmk", rdm_dict_data, "goal_pcmk")
      update_goal_dropdown(session, "goal_sbppbl", rdm_dict_data, "goal_sbppbl")
      update_goal_dropdown(session, "goal_subcomp_profics", rdm_dict_data, "goal_subcomp_profics")
    })

    # Competency code parsing functions
    get_subcomp_code <- function(selected, competency_types) {
      if(is.null(selected) || selected == "") return(NULL)

      if(grepl("^\\d+$", selected)) {
        num_val <- as.numeric(selected)

        if(identical(competency_types, c("PC", "MK"))) {
          if(num_val <= 6) {
            return(paste0("PC", num_val))
          } else if(num_val <= 9) {
            return(paste0("MK", num_val - 6))
          }
        }
        else if(identical(competency_types, c("SBP", "PBLI"))) {
          if(num_val <= 3) {
            return(paste0("SBP", num_val))
          } else if(num_val <= 5) {
            return(paste0("PBLI", num_val - 3))
          }
        }
        else if(identical(competency_types, c("PROF", "ICS"))) {
          if(num_val <= 4) {
            return(paste0("PROF", num_val))
          } else if(num_val <= 7) {
            return(paste0("ICS", num_val - 4))
          }
        }
      } else if(grepl("^([A-Z]+)(\\d+)$", selected)) {
        return(selected)
      }
      return(NULL)
    }

    # Get "how" text from dictionary
    get_how_text <- function(competency_code) {
      req(competency_code, rdm_dict_data)

      field_name <- paste0("how_", tolower(competency_code))
      how_field <- rdm_dict_data %>%
        filter(field_name == !!field_name) %>%
        pull(field_label)

      if(length(how_field) > 0) how_field[1] else ""
    }

    # Process selected competency codes
    selected_pcmk_code <- reactive({
      get_subcomp_code(input$goal_pcmk, c("PC", "MK"))
    })

    selected_sbppbl_code <- reactive({
      get_subcomp_code(input$goal_sbppbl, c("SBP", "PBLI"))
    })

    selected_profics_code <- reactive({
      get_subcomp_code(input$goal_subcomp_profics, c("PROF", "ICS"))
    })

    # Process competency names
    selected_pcmk_name <- reactive({
      parse_competency(input$goal_pcmk, c("PC", "MK"), subcompetency_maps, competency_list)
    })

    selected_sbp_pbli_name <- reactive({
      parse_competency(input$goal_sbppbl, c("SBP", "PBLI"), subcompetency_maps, competency_list)
    })

    selected_prof_ics_name <- reactive({
      parse_competency(input$goal_subcomp_profics, c("PROF", "ICS"), subcompetency_maps, competency_list)
    })

    # Get milestone data
    pcmk_milestone_data <- reactive({
      req(selected_pcmk_code())
      get_milestone_data(rdm_dict_data, selected_pcmk_code())
    })

    sbppbl_milestone_data <- reactive({
      req(selected_sbppbl_code())
      get_milestone_data(rdm_dict_data, selected_sbppbl_code())
    })

    profics_milestone_data <- reactive({
      req(selected_profics_code())
      get_milestone_data(rdm_dict_data, selected_profics_code())
    })

    # Display competency names
    output$selected_pcmk_name <- renderUI({
      name <- selected_pcmk_name()
      if (name != "") {
        div(
          h5("Selected Competency:"),
          p(strong(name))
        )
      }
    })

    output$selected_sbp_pbli_name <- renderUI({
      name <- selected_sbp_pbli_name()
      if (name != "") {
        div(
          h5("Selected Competency:"),
          p(strong(name))
        )
      }
    })

    output$selected_prof_ics_name <- renderUI({
      name <- selected_prof_ics_name()
      if (name != "") {
        div(
          h5("Selected Competency:"),
          p(strong(name))
        )
      }
    })

    # Update milestone row selections
    observe({
      req(pcmk_milestone_data())
      updateSelectInput(session, "pcmk_row_select",
                        choices = setNames(seq_len(nrow(pcmk_milestone_data())),
                                           pcmk_milestone_data()$Milestone))
    })

    observe({
      req(sbppbl_milestone_data())
      updateSelectInput(session, "sbppbl_row_select",
                        choices = setNames(seq_len(nrow(sbppbl_milestone_data())),
                                           sbppbl_milestone_data()$Milestone))
    })

    observe({
      req(profics_milestone_data())
      updateSelectInput(session, "profics_row_select",
                        choices = setNames(seq_len(nrow(profics_milestone_data())),
                                           profics_milestone_data()$Milestone))
    })

    # Render "how" text outputs
    output$how_pcmk <- renderText({
      req(selected_pcmk_code())
      get_how_text(selected_pcmk_code())
    })

    output$how_sbppbl <- renderText({
      req(selected_sbppbl_code())
      get_how_text(selected_sbppbl_code())
    })

    output$how_profics <- renderText({
      req(selected_profics_code())
      get_how_text(selected_profics_code())
    })

    # Selected goal reactives
    selected_pcmk_goal <- reactive({
      req(pcmk_milestone_data(), input$pcmk_row_select, input$pcmk_level_select)
      data <- pcmk_milestone_data()
      row <- as.numeric(input$pcmk_row_select)
      col <- paste("Level", input$pcmk_level_select)

      list(
        subcompetency = selected_pcmk_code(),
        row_id = row,
        milestone = data$Milestone[row],
        level = input$pcmk_level_select,
        goal_text = data[row, col],
        target_date = input$pcmk_target_date,
        status = input$pcmk_status,
        how = get_how_text(selected_pcmk_code())
      )
    })

    selected_sbppbl_goal <- reactive({
      req(sbppbl_milestone_data(), input$sbppbl_row_select, input$sbppbl_level_select)
      data <- sbppbl_milestone_data()
      row <- as.numeric(input$sbppbl_row_select)
      col <- paste("Level", input$sbppbl_level_select)

      list(
        subcompetency = selected_sbppbl_code(),
        row_id = row,
        milestone = data$Milestone[row],
        level = input$sbppbl_level_select,
        goal_text = data[row, col],
        target_date = input$sbppbl_target_date,
        status = input$sbppbl_status,
        how = get_how_text(selected_sbppbl_code())
      )
    })

    selected_profics_goal <- reactive({
      req(profics_milestone_data(), input$profics_row_select, input$profics_level_select)
      data <- profics_milestone_data()
      row <- as.numeric(input$profics_row_select)
      col <- paste("Level", input$profics_level_select)

      list(
        subcompetency = selected_profics_code(),
        row_id = row,
        milestone = data$Milestone[row],
        level = input$profics_level_select,
        goal_text = data[row, col],
        target_date = input$profics_target_date,
        status = input$profics_status,
        how = get_how_text(selected_profics_code())
      )
    })

    # Render milestone tables
    output$pcmk_milestone_table <- renderUI({
      req(pcmk_milestone_data())
      tagList(
        h5("Milestone Levels"),
        div(
          style = "overflow-x: auto;",
          renderTable({
            df <- pcmk_milestone_data()
            col_names <- c("Milestone")
            for(i in 1:5) {
              col_names <- c(col_names, paste0(i, ": ", milestone_levels[[as.character(i)]]))
            }
            names(df) <- col_names
            df
          }, sanitize.text.function = function(x) x)
        )
      )
    })

    output$sbppbl_milestone_table <- renderUI({
      req(sbppbl_milestone_data())
      tagList(
        h5("Milestone Levels"),
        div(
          style = "overflow-x: auto;",
          renderTable({
            df <- sbppbl_milestone_data()
            col_names <- c("Milestone")
            for(i in 1:5) {
              col_names <- c(col_names, paste0(i, ": ", milestone_levels[[as.character(i)]]))
            }
            names(df) <- col_names
            df
          }, sanitize.text.function = function(x) x)
        )
      )
    })

    output$profics_milestone_table <- renderUI({
      req(profics_milestone_data())
      tagList(
        h5("Milestone Levels"),
        div(
          style = "overflow-x: auto;",
          renderTable({
            df <- profics_milestone_data()
            col_names <- c("Milestone")
            for(i in 1:5) {
              col_names <- c(col_names, paste0(i, ": ", milestone_levels[[as.character(i)]]))
            }
            names(df) <- col_names
            df
          }, sanitize.text.function = function(x) x)
        )
      )
    })

    # Output renderers for selected goals
    output$pcmk_selected_goal <- renderText({
      req(selected_pcmk_goal())
      selected_pcmk_goal()$goal_text
    })

    output$sbppbl_selected_goal <- renderText({
      req(selected_sbppbl_goal())
      selected_sbppbl_goal()$goal_text
    })

    output$profics_selected_goal <- renderText({
      req(selected_profics_goal())
      selected_profics_goal()$goal_text
    })

    # Return all data including review responses and how text
    return(list(
      pcmk = list(
        code = reactive(input$goal_pcmk),
        name = selected_pcmk_name,
        subcomp_code = selected_pcmk_code,
        goal = selected_pcmk_goal,
        review = reactive(list(
          had_prior_goal = input$prior_goal_pcmk,
          progress = input$review_q_pcmk,
          factors = input$review_q2_pcmk
        ))
      ),
      sbp_pbli = list(
        code = reactive(input$goal_sbppbl),
        name = selected_sbp_pbli_name,
        subcomp_code = selected_sbppbl_code,
        goal = selected_sbppbl_goal,
        review = reactive(list(
          had_prior_goal = input$prior_goal_sbppbl,
          progress = input$review_q_sbppbl,
          factors = input$review_q2_sbppbl
        ))
      ),
      prof_ics = list(
        code = reactive(input$goal_subcomp_profics),
        name = selected_prof_ics_name,
        subcomp_code = selected_profics_code,
        goal = selected_profics_goal,
        review = reactive(list(
          had_prior_goal = input$prior_goal_profics,
          progress = input$review_q_profics,
          factors = input$review_q2_profics
        ))
      ),
      all_selections = reactive({
        list(
          pcmk = list(
            code = input$goal_pcmk,
            name = selected_pcmk_name(),
            subcomp_code = selected_pcmk_code(),
            goal = selected_pcmk_goal(),
            review = list(
              had_prior_goal = input$prior_goal_pcmk,
              progress = input$review_q_pcmk,
              factors = input$review_q2_pcmk
            )
          ),
          sbp_pbli = list(
            code = input$goal_sbppbl,
            name = selected_sbp_pbli_name(),
            subcomp_code = selected_sbppbl_code(),
            goal = selected_sbppbl_goal(),
            review = list(
              had_prior_goal = input$prior_goal_sbppbl,
              progress = input$review_q_sbppbl,
              factors = input$review_q2_sbppbl
            )
          ),
          prof_ics = list(
            code = input$goal_subcomp_profics,
            name = selected_prof_ics_name(),
            subcomp_code = selected_profics_code(),
            goal = selected_profics_goal(),
            review = list(
              had_prior_goal = input$prior_goal_profics,
              progress = input$review_q_profics,
              factors = input$review_q2_profics
            )
          )
        )
      })
    ))
  })
}


update_goal_dropdown <- function(session, input_id, rdm_dict_data, field_name_pattern) {
  req(rdm_dict_data)

  # Filter the dictionary for the specific dropdown field
  goal_fields <- rdm_dict_data %>%
    filter(
      field_type == "dropdown",
      grepl(field_name_pattern, field_name, ignore.case = TRUE)
    )

  if(nrow(goal_fields) > 0) {
    goal_field <- goal_fields %>% slice(1)

    if(nrow(goal_field) > 0 && !is.na(goal_field$select_choices_or_calculations)) {
      # Extract choices
      choices_text <- goal_field$select_choices_or_calculations

      # Parse choices (assuming format like "1, Patient Care 1 | 2, Patient Care 2")
      choices_list <- strsplit(choices_text, " \\| ")[[1]]
      choices_pairs <- lapply(choices_list, function(x) {
        parts <- strsplit(x, ", ")[[1]]
        c(value = parts[1], label = paste(parts[-1], collapse = ", "))
      })

      choices_values <- sapply(choices_pairs, function(x) x[1])
      choices_labels <- sapply(choices_pairs, function(x) x[2])
      choices <- setNames(choices_values, choices_labels)

      # Update the selectInput
      updateSelectInput(session, input_id, choices = choices)
    }
  }
}

get_milestone_data <- function(rdm_dict_data, subcomp_code) {
  req(rdm_dict_data, subcomp_code)

  # Extract competency type and number
  comp_match <- regexec("^([A-Z]+)(\\d+)$", subcomp_code)
  comp_type <- regmatches(subcomp_code, comp_match)[[1]][2]
  comp_num <- regmatches(subcomp_code, comp_match)[[1]][3]

  # Create pattern based on competency type
  pattern <- NULL
  if(comp_type == "PC") {
    pattern <- paste0("^pc", comp_num, "_r\\d+$")
  } else if(comp_type == "MK") {
    pattern <- paste0("^mk", comp_num, "_r\\d+$")
  } else if(comp_type == "SBP") {
    pattern <- paste0("^sbp", comp_num, "_r\\d+$")
  } else if(comp_type == "PBLI") {
    # For PBLI, field names use "pbl" instead of "pbli"
    pattern <- paste0("^pbl", comp_num, "_r\\d+$")
  } else if(comp_type == "PROF") {
    pattern <- paste0("^prof", comp_num, "_r\\d+$")
  } else if(comp_type == "ICS") {
    pattern <- paste0("^ics", comp_num, "_r\\d+$")
  } else {
    return(NULL)
  }

  # Find all fields related to this subcompetency
  milestone_fields <- rdm_dict_data %>%
    filter(
      grepl(pattern, field_name, ignore.case = TRUE)
    ) %>%
    arrange(field_name)  # Sort by field name to maintain row order

  # If no milestone fields found, return NULL
  if(nrow(milestone_fields) == 0) {
    print(paste("No milestone fields found for pattern:", pattern))
    return(NULL)
  }

  # Process each milestone field to extract the 5 levels
  milestone_rows <- lapply(1:nrow(milestone_fields), function(i) {
    field <- milestone_fields[i, ]

    # Extract the row label/name (often in field_label)
    row_label <- field$field_label
    if(is.na(row_label) || row_label == "") {
      # Use field name as fallback
      row_label <- field$field_name
    }

    # Parse the milestone levels (format: "1, Description | 2, Description | ...")
    choices_text <- field$select_choices_or_calculations
    if(is.na(choices_text) || choices_text == "") {
      # No data available
      return(c(row_label, rep("", 5)))
    }

    # Split by pipe and extract descriptions
    levels <- strsplit(choices_text, " \\| ")[[1]]

    # Create a vector with 5 empty strings
    level_descriptions <- rep("", 5)

    # Fill in the descriptions for the available levels
    for(level_text in levels) {
      parts <- strsplit(level_text, ", ")[[1]]
      if(length(parts) > 1) {
        level_num <- as.numeric(parts[1])
        if(!is.na(level_num) && level_num >= 1 && level_num <= 5) {
          # Add the description for this level
          level_descriptions[level_num] <- paste(parts[-1], collapse = ", ")
        }
      }
    }

    # Return row label and level descriptions
    c(row_label, level_descriptions)
  })

  # Convert to data frame
  milestone_df <- do.call(rbind, milestone_rows)

  # Add column names
  colnames(milestone_df) <- c("Milestone", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5")

  return(as.data.frame(milestone_df))
}

# Updated Scholarship Module UI
# Updated Scholarship Module UI
scholarship_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("4. Scholarship & QI Projects"),

    # Descriptive text
    div(
      class = "mb-4 p-3 bg-light rounded",
      p("To aid in data collection and to create a place for you to centrally gather the work you do as a resident, please take the time to update any presentation, publication, quality improvement work, research, committee you have taken part of. If you have done a safety event or root cause analysis, you do not need to enter any further ones."),
      p("Below this entry is a list of what you have entered thus far, plus whether you have participated in safety reviews."),
      p("Obviously, if this is the first time you have done this, you will need to enter a bit more. It only needs to be what has happened since you started residency."),
      p(HTML("Last, please enter <strong>complete citations</strong>. You can use this to develop your CV, and helps us in displaying what our residents do."))
    ),

    # Form for adding new scholarship
    div(
      class = "mb-4",
      h4("Add New Activity"),
      selectInput(ns("type"), "Project type:",
                  choices = c("",
                              parse_choices(rdm_dict$select_choices_or_calculations[
                                rdm_dict$field_name=="schol_type"
                              ])
                  )
      ),
      uiOutput(ns("fields")),  # will render type-specific inputs
      actionButton(ns("add"), "Add This Project")
    ),

    hr(),

    # Section to display existing scholarship data
    div(
      class = "mt-4",
      h4("Your Scholarship Activities", class = "mb-3"),
      p("This table shows your scholarship activities recorded in the system:"),
      DTOutput(ns("scholarship_table")),

      # Patient Safety Achievements without extra div
      uiOutput(ns("achievement_notifications"))
    ),

    # Next button at the bottom
    div(
      class = "mt-4 text-center",
      actionButton(ns("next_btn"), "Next Section", class="btn-primary btn-lg")
    )
  )
}


scholarship_module_server <- function(id, rdm_dict, record_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Add debugging JavaScript
    shinyjs::runjs('
      $(document).ready(function() {
        console.log("DOM loaded - checking for cards");
        console.log("section4_card exists:", $("#section4_card").length > 0);
        console.log("section5_card exists:", $("#section5_card").length > 0);
      });
    ')

    # ---- TABLE DISPLAY SECTION (from scholarship_table_server) ----

    # Process the scholarship data
    scholarship_results <- reactive({
      req(record_id())
      if (is.null(schol_data)) {
        # Fetch scholarship data from REDCap if not provided
        # (implement logic to fetch data)
        return(list(
          table_data = data.frame(),
          completed_ps = FALSE,
          completed_rca = FALSE
        ))
      } else {
        process_scholarship_data(schol_data, record_id(), rdm_dict)
      }
    })

    # Render the table
    output$scholarship_table <- renderDT({
      req(scholarship_results())
      if (nrow(scholarship_results()$table_data) == 0) {
        # Return an empty styled table with a message
        empty_df <- data.frame(
          Scholarship_Type = "No scholarship activities found",
          Description = "",
          stringsAsFactors = FALSE
        )
        create_styled_dt(empty_df, caption = "Scholarship Activities")
      } else {
        # Return the processed data in a styled table
        create_styled_dt(scholarship_results()$table_data, caption = "Scholarship Activities")
      }
    })

    # Render achievement notifications
    # Update the achievement notifications rendering:
    # Update this part in your scholarship_module_server function
    output$achievement_notifications <- renderUI({
      req(scholarship_results())
      # Get completion status
      completed_ps <- scholarship_results()$completed_ps
      completed_rca <- scholarship_results()$completed_rca
      if (!completed_ps && !completed_rca) {
        return(NULL)  # Don't show anything if neither is completed
      }

      # Create notification messages directly without nesting in additional divs
      tagList(
        if(completed_ps) {
          tags$p(
            tags$span(icon("check-circle"), class = "text-success"),
            tags$strong("Achievement: "),
            "You have completed a Patient Safety Review",
            class = "alert alert-success p-2 mt-3"
          )
        },
        if(completed_rca) {
          tags$p(
            tags$span(icon("check-circle"), class = "text-success"),
            tags$strong("Achievement: "),
            "You have completed a Root Cause Analysis",
            class = "alert alert-success p-2 mt-3"
          )
        }
      )
    })

    # Render the type-specific fields UI
    output$fields <- renderUI({
      req(input$type)
      sd <- rdm_dict %>% filter(form_name == "scholarship")
      ns <- session$ns

      if (input$type == "1") {
        # Quality Improvement
        tagList(
          textAreaInput(ns("schol_qi"),
                        sd$field_label[sd$field_name == "schol_qi"],
                        width = "100%", height = "100px"
          ),
          textInput(ns("schol_res_mentor"),
                    sd$field_label[sd$field_name == "schol_res_mentor"],
                    value = ""
          ),
          selectInput(ns("schol_res_status"),
                      sd$field_label[sd$field_name == "schol_res_status"],
                      choices = c("", parse_choices(
                        sd$select_choices_or_calculations[sd$field_name == "schol_res_status"]
                      )),
                      selected = ""
          ),
          selectInput(ns("schol_div"),
                      sd$field_label[sd$field_name == "schol_div"],
                      choices = c("", parse_choices(
                        sd$select_choices_or_calculations[sd$field_name == "schol_div"]
                      )),
                      selected = ""
          ),
          radioButtons(ns("schol_pres"),
                       sd$field_label[sd$field_name == "schol_pres"],
                       choices = c("Yes", "No"),
                       selected = character(0),
                       inline = TRUE
          ),
          radioButtons(ns("schol_pub"),
                       sd$field_label[sd$field_name == "schol_pub"],
                       choices = c("Yes", "No"),
                       selected = character(0),
                       inline = TRUE
          )
        )

      } else if (input$type == "2") {
        # Patient Safety Review
        tagList(
          radioButtons(ns("schol_ps"),
                       sd$field_label[sd$field_name == "schol_ps"],
                       choices = c("Yes", "No"),
                       selected = character(0),
                       inline = TRUE
          ),
          radioButtons(ns("schol_rca"),
                       sd$field_label[sd$field_name == "schol_rca"],
                       choices = c("Yes", "No"),
                       selected = character(0),
                       inline = TRUE
          )
        )

      } else if (input$type %in% c("3", "6")) {
        # Research or Education
        tagList(
          textAreaInput(ns("schol_res"),
                        sd$field_label[sd$field_name == "schol_res"],
                        width = "100%", height = "100px"
          ),
          textInput(ns("schol_res_mentor"),
                    sd$field_label[sd$field_name == "schol_res_mentor"],
                    value = ""
          ),
          selectInput(ns("schol_res_status"),
                      sd$field_label[sd$field_name == "schol_res_status"],
                      choices = c("", parse_choices(
                        sd$select_choices_or_calculations[sd$field_name == "schol_res_status"]
                      )),
                      selected = ""
          ),
          selectInput(ns("schol_div"),
                      sd$field_label[sd$field_name == "schol_div"],
                      choices = c("", parse_choices(
                        sd$select_choices_or_calculations[sd$field_name == "schol_div"]
                      )),
                      selected = ""
          ),
          radioButtons(ns("schol_pres"),
                       sd$field_label[sd$field_name == "schol_pres"],
                       choices = c("Yes", "No"),
                       selected = character(0),
                       inline = TRUE
          ),
          radioButtons(ns("schol_pub"),
                       sd$field_label[sd$field_name == "schol_pub"],
                       choices = c("Yes", "No"),
                       selected = character(0),
                       inline = TRUE
          )
        )

      } else if (input$type %in% c("4", "5")) {
        # Presentation (4) or Publication (5) only
        if (input$type == "4") {
          # Presentation
          tagList(
            selectInput(ns("schol_pres_type"),
                        sd$field_label[sd$field_name == "schol_pres_type"],
                        choices = c("", parse_choices(
                          sd$select_choices_or_calculations[sd$field_name == "schol_pres_type"]
                        )),
                        selected = ""
            ),
            textAreaInput(ns("schol_pres_conf"),
                          sd$field_label[sd$field_name == "schol_pres_conf"],
                          width = "100%", height = "50px"
            ),
            textAreaInput(ns("schol_cit"),
                          sd$field_label[sd$field_name == "schol_cit"],
                          width = "100%", height = "80px"
            )
          )
        } else {
          # Publication
          tagList(
            textAreaInput(ns("schol_cit"),
                          sd$field_label[sd$field_name == "schol_cit"],
                          width = "100%", height = "80px"
            )
          )
        }

      } else if (input$type == "7") {
        # Committee
        tagList(
          textAreaInput(ns("schol_comm"),
                        sd$field_label[sd$field_name == "schol_comm"],
                        width = "100%", height = "80px"
          ),
          selectInput(ns("schol_comm_type"),
                      sd$field_label[sd$field_name == "schol_comm_type"],
                      choices = c("", parse_choices(
                        sd$select_choices_or_calculations[sd$field_name == "schol_comm_type"]
                      )),
                      selected = ""
          ),
          conditionalPanel(
            condition = paste0("input['", ns("schol_comm_type"), "']=='5'"),
            textAreaInput(ns("schol_comm_other"),
                          sd$field_label[sd$field_name == "schol_comm_other"],
                          width = "100%", height = "50px"
            )
          )
        )

      } else {
        NULL
      }
    })

    # Helpers to show the modals:
    showPresModal <- function() {
      showModal(modalDialog(
        title = "Add Presentation",
        selectInput(ns("p_type"),
                    "Where presented?",
                    parse_choices(
                      rdm_dict$select_choices_or_calculations[
                        rdm_dict$field_name == "schol_pres_type"
                      ]
                    )
        ),
        textAreaInput(ns("p_conf"), "Conference / Details"),
        textAreaInput(ns("p_cit"),  "Full citation"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_pres"), "Save Presentation")
        ),
        easyClose = FALSE
      ))
    }

    showConfirmPresModal <- function() {
      showModal(modalDialog(
        title = "Presentation saved",
        p("Would you like to add another presentation?"),
        footer = tagList(
          modalButton("No"),                # closes dialog
          actionButton(ns("again_pres"), "Yes")
        ),
        easyClose = FALSE
      ))
    }

    showPubModal <- function() {
      showModal(modalDialog(
        title = "Add Publication",
        textAreaInput(ns("pub_cit"), "Full citation"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_pub"), "Save Publication")
        ),
        easyClose = FALSE
      ))
    }

    showConfirmPubModal <- function() {
      showModal(modalDialog(
        title = "Publication saved",
        p("Would you like to add another publication?"),
        footer = tagList(
          modalButton("No"),
          actionButton(ns("again_pub"), "Yes")
        ),
        easyClose = FALSE
      ))
    }

    # When user says "Yes" to schol_pres, show first presentation modal:
    observeEvent(input$schol_pres, {
      if (input$schol_pres == "Yes") {
        showPresModal()
      }
    })

    # Save a presentation
    observeEvent(input$save_pres, {
      removeModal()
      handle_scholarship_submission(
        record_id,
        list(
          schol_type = input$type,
          schol_pres_type = input$p_type,
          schol_pres_conf = input$p_conf,
          schol_cit = input$p_cit,
          schol_pres = 1  # Explicitly set this to 1 (Yes)
        )
      )
      showConfirmPresModal()
    })

    # If they click "Yes" on the confirm, show the presentation modal again
    observeEvent(input$again_pres, {
      removeModal()
      showPresModal()
    })

    # Same flow for publications:
    observeEvent(input$schol_pub, {
      if (input$schol_pub == "Yes") {
        showPubModal()
      }
    })

    # Save a publication
    observeEvent(input$save_pub, {
      removeModal()
      handle_scholarship_submission(
        record_id,
        list(
          schol_type = input$type,
          schol_cit = input$pub_cit,
          schol_pub = 1  # Explicitly set this to 1 (Yes)
        )
      )
      showConfirmPubModal()
    })

    observeEvent(input$again_pub, {
      removeModal()
      showPubModal()
    })

    observeEvent(input$add, {
      vals <- reactiveValuesToList(input)

      ## pick out exactly the REDCap field names you want:
      keep <- c(
        "schol_qi",           # QI text
        "schol_res",          # research/edu description
        "schol_res_mentor",
        "schol_res_status",
        "schol_div",
        "schol_ps",           # patient safety
        "schol_rca",
        "schol_pres",         # Added these Yes/No fields
        "schol_pub"
      )

      # Filter only the keys that actually exist in vals
      keep <- keep[keep %in% names(vals)]

      # Get only the values that exist and ensure they're all vectors
      project_data <- list()
      for (k in keep) {
        if (!is.null(vals[[k]])) {
          # Convert Yes/No values to numeric
          if (k %in% c("schol_ps", "schol_rca", "schol_pres", "schol_pub")) {
            project_data[[k]] <- convert_yes_no(vals[[k]])
          } else if (is.vector(vals[[k]])) {
            project_data[[k]] <- vals[[k]]
          } else {
            # Convert non-vectors to character strings if possible
            project_data[[k]] <- as.character(vals[[k]])
          }
        }
      }

      ## put the master dropdown on it too - using the correct field name
      if (!is.null(input$type)) {
        project_data$schol_type <- input$type
      }

      ## Only submit if we have data to submit
      if (length(project_data) > 0) {
        ## now submit
        tryCatch({
          handle_scholarship_submission(
            record_id = record_id,
            values = project_data
          )
          showNotification("Project saved", type = "message")
        }, error = function(e) {
          showNotification(paste("Error saving project:", e$message), type = "error")
        })
      } else {
        showNotification("No data to save", type = "warning")
      }

      ## reset the dropdown
      updateSelectInput(session, "type", selected = "")
    })
  })
}


# Helper function for Yes/No conversion
convert_yes_no <- function(value) {
  if (is.character(value)) {
    if (value == "Yes") return(1)
    if (value == "No") return(0)
  }
  return(value)
}

# Handle scholarship submission
handle_scholarship_submission <- function(record_id, values) {
  if (is.reactive(record_id)) {
    record_id <- record_id()
  }

  # Create the submission payload
  payload <- list(
    record_id = record_id,
    redcap_repeat_instrument = "scholarship"
  )

  # Add all values to the payload
  for (field_name in names(values)) {
    payload[[field_name]] <- values[[field_name]]
  }

  # Generate a new instance number or use an existing one if appropriate
  instance_number <- generate_new_instance(
    record_id = record_id,
    instrument_name = "scholarship",
    coach_data = NULL,  # Adjust based on your needs
    redcap_uri = url,
    token = rdm_token
  )

  # Set the instance number
  payload$redcap_repeat_instance <- as.numeric(instance_number)

  # Submit to REDCap
  result <- direct_redcap_import(
    data = payload,
    record_id = record_id,
    url = url,
    token = rdm_token
  )

  return(result)
}
