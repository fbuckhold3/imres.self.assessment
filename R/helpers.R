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
    tags$p(class="help-block", "What styles/types of learning to prefer? We use this data both in your guidance and to look systemically at this data to improve the program. You may choose as many as you wish. "),
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

submit_to_redcap_with_period_check <- function(record_id, instrument, period, data, url, token) {
  message("DEBUG: submit_to_redcap_with_period_check called with period = '", period, "' of class ", class(period))

  # Direct mapping for period text to numeric
  if (is.character(period)) {
    period_mapping <- c(
      "Entering Residency" = 7,
      "Mid Intern" = 1,
      "End Intern" = 2,
      "Mid PGY2" = 3,
      "End PGY2" = 4,
      "Mid PGY3" = 5,
      "Graduating" = 6
    )

    if (period %in% names(period_mapping)) {
      period_numeric <- period_mapping[period]
      message("DEBUG: Found exact match for '", period, "' -> ", period_numeric)
    } else {
      if (grepl("Graduat", period)) {
        period_numeric <- 6
      } else if (grepl("Enter", period)) {
        period_numeric <- 7
      } else {
        period_numeric <- 1
      }
    }
  } else if (is.numeric(period)) {
    period_numeric <- period
  } else {
    period_numeric <- 1
  }

  # Create a new data frame with record_id FIRST
  new_data <- data.frame(
    record_id = as.character(record_id),
    stringsAsFactors = FALSE
  )

  # Add the period fields
  new_data$redcap_repeat_instrument <- instrument
  new_data$s_e_period <- as.character(period_numeric)
  new_data$s_e_date <- format(Sys.Date(), "%Y-%m-%d")

  # Now add all the fields from the original data
  # Exclude any fields we've already set to avoid duplicates
  fields_to_exclude <- c("record_id", "redcap_repeat_instrument", "s_e_period", "s_e_date")
  for (col in names(data)) {
    if (!(col %in% fields_to_exclude)) {
      new_data[[col]] <- data[[col]]
    }
  }

  # Set a specific instance number - use period_numeric or a timestamp
  # This helps ensure uniqueness and proper organization
  instance_number <- as.numeric(period_numeric) # or use a timestamp approach
  new_data$redcap_repeat_instance <- as.character(instance_number)

  # Add form complete status if not present
  form_complete_field <- paste0(instrument, "_complete")
  if (!form_complete_field %in% names(new_data)) {
    new_data[[form_complete_field]] <- "0"
  }

  # Ensure all fields are character type for REDCap
  for (col in names(new_data)) {
    if (!is.character(new_data[[col]])) {
      new_data[[col]] <- as.character(new_data[[col]])
    }
  }

  # Prepare data for REDCap - must be a LIST of data frames
  submission_list <- list(new_data)

  # Convert to JSON with auto_unbox = FALSE to ensure it's formatted correctly
  json_data <- jsonlite::toJSON(submission_list, auto_unbox = FALSE)

  # Log the JSON data for debugging
  message("DEBUG: JSON data (first 200 chars): ", substr(json_data, 1, 200))

  # Submit to REDCap
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
      data = json_data,
      returnContent = "count",
      returnFormat = "json"
    ),
    encode = "form"
  )

  # Process response
  status_code <- httr::status_code(response)
  message("REDCap API response status: ", status_code)

  response_content <- httr::content(response, "text", encoding = "UTF-8")
  message("REDCap API response: ", response_content)

  if (status_code == 200) {
    return(list(
      success = TRUE,
      outcome_message = paste("Successfully submitted data for record", record_id)
    ))
  } else {
    error_message <- response_content
    message("Error submitting to REDCap: ", error_message)
    return(list(
      success = FALSE,
      outcome_message = paste("Failed to submit data for record", record_id, ":", error_message)
    ))
  }
}

direct_redcap_import <- function(data, record_id, url, token) {
  # Ensure record_id is character
  record_id <- as.character(record_id)

  # Ensure data is a data frame
  if (!is.data.frame(data)) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }

  # Ensure record_id is in the data frame
  if (!"record_id" %in% names(data)) {
    data$record_id <- record_id
  }

  # Always use character types for all fields
  for (col in names(data)) {
    data[[col]] <- as.character(data[[col]])
  }

  # Move record_id to first position to ensure it's prominent in JSON
  data <- data[, c("record_id", setdiff(names(data), "record_id"))]

  # Ensure required repeating fields
  if (!"redcap_repeat_instrument" %in% names(data)) {
    stop("'redcap_repeat_instrument' must be in the data")
  }

  if (!"redcap_repeat_instance" %in% names(data)) {
    # Generate a timestamp-based instance
    timestamp <- as.numeric(format(Sys.time(), "%Y%m%d%H%M%S"))
    data$redcap_repeat_instance <- as.character(timestamp %% 10000) # Last 4 digits
  }

  # Convert to JSON - CRITICAL to wrap in a list
  json_data <- jsonlite::toJSON(list(data), auto_unbox = FALSE)

  # Log what we're sending
  message("Submitting to REDCap with record_id: ", record_id)
  message("JSON data (first 200 chars): ", substr(json_data, 1, 200))

  # Make REDCap API call
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
      data = json_data,
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

  if (status_code == 200) {
    return(list(
      success = TRUE,
      outcome_message = paste("Successfully submitted data for record", record_id)
    ))
  } else {
    return(list(
      success = FALSE,
      outcome_message = paste("Failed to submit data for record", record_id, ":", response_content)
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
handle_scholarship_submission <- function(record_id, values, token = NULL) {
  # Always use a hardcoded string URL to avoid closure issues
  redcap_url_string <- "https://redcapsurvey.slu.edu/api/"

  # Check if token is provided
  if (is.null(token)) {
    # Try to get token from parent environment
    if (exists("rdm_token", envir = parent.frame())) {
      token <- get("rdm_token", envir = parent.frame())
    } else {
      # Look for it in the global environment
      if (exists("rdm_token", envir = .GlobalEnv)) {
        token <- get("rdm_token", envir = .GlobalEnv)
      } else {
        stop("REDCap token not found. Please provide the token as a parameter.")
      }
    }
  }

  # Extract record_id if it's reactive
  actual_record_id <- if (is.reactive(record_id)) record_id() else record_id
  actual_record_id <- as.character(actual_record_id)

  message("Processing scholarship submission for record ID: ", actual_record_id,
          " with values: ", paste(names(values), collapse=", "))

  # Convert any Yes/No values to numeric
  yes_no_fields <- c("schol_pres", "schol_pub", "schol_ps", "schol_rca")
  for (field in yes_no_fields) {
    if (field %in% names(values)) {
      values[[field]] <- if (values[[field]] == "Yes") "1" else if (values[[field]] == "No") "0" else values[[field]]
    }
  }

  # Fix the field name issue
  if ("type" %in% names(values) && !("schol_type" %in% names(values))) {
    values[["schol_type"]] <- values[["type"]]
    values[["type"]] <- NULL
  }

  # Get the next instance number - SIMPLIFIED
  next_inst <- 1

  # Try direct REDCap API call to get existing instances
  tryCatch({
    body_params <- list(
      token = token,
      content = "record",
      action = "export",
      format = "json",
      type = "flat",
      records = actual_record_id,
      forms = "scholarship"
    )

    response <- httr::POST(
      url = redcap_url_string,
      body = body_params,
      encode = "form"
    )

    if (httr::status_code(response) == 200) {
      response_text <- httr::content(response, "text", encoding = "UTF-8")
      message("API Export Response Text (first 100 chars): ",
              substring(response_text, 1, 100))

      if (response_text != "" && response_text != "[]") {
        parsed_data <- jsonlite::fromJSON(response_text)
        if (is.data.frame(parsed_data) && nrow(parsed_data) > 0) {
          # Extract just records with redcap_repeat_instrument = "scholarship"
          scholarship_records <- parsed_data[
            parsed_data$redcap_repeat_instrument == "scholarship",
          ]

          if (nrow(scholarship_records) > 0 &&
              "redcap_repeat_instance" %in% colnames(scholarship_records)) {
            # Find max instance and increment
            instances <- as.numeric(scholarship_records$redcap_repeat_instance)
            instances <- instances[!is.na(instances)]
            if (length(instances) > 0) {
              next_inst <- max(instances) + 1
            }
          }
        }
      }
    } else {
      message("API error during export: ", httr::status_code(response),
              " - ", httr::content(response, "text"))
    }
  }, error = function(e) {
    message("Error during instance calculation: ", e$message)
  })

  message("Using instance number: ", next_inst)

  # Build a simple data structure with minimal fields
  submission_data <- list(
    record_id = actual_record_id,
    redcap_repeat_instrument = "scholarship",
    redcap_repeat_instance = as.character(next_inst)
  )

  # Add only the fields we need
  for (field in names(values)) {
    if (!is.null(values[[field]]) &&
        !is.na(values[[field]]) &&
        values[[field]] != "") {
      submission_data[[field]] <- as.character(values[[field]])
    }
  }

  # Convert to JSON for REDCap API - as an array of one object
  data_json <- jsonlite::toJSON(list(submission_data), auto_unbox = TRUE)

  message("Sending data to REDCap: ", data_json)

  # Submit to REDCap API with more detailed parameters
  response <- httr::POST(
    url = redcap_url_string,
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

  # Check the status and response
  status_code <- httr::status_code(response)
  response_content <- httr::content(response, "text", encoding = "UTF-8")

  message("REDCap API response status: ", status_code)
  message("REDCap API response: ", response_content)

  # Return result
  if (status_code == 200) {
    return(list(
      success = TRUE,
      outcome_message = paste("Successfully created scholarship entry (instance ",
                              next_inst, ")", sep = "")
    ))
  } else {
    return(list(
      success = FALSE,
      outcome_message = paste("Error: ", response_content, sep = "")
    ))
  }
}

# Get the next available instance for scholarship tracking
get_next_scholarship_instance <- function(record_id, redcap_url, token) {
  # Query REDCap for existing instances
  response <- httr::POST(
    url = redcap_url,
    body = list(
      token = token,
      content = "record",
      action = "export",
      format = "json",
      type = "flat",
      records = record_id,
      fields = "record_id",
      forms = "scholarship_tracking",
      returnFormat = "json"
    ),
    encode = "form"
  )

  # Process response
  if (httr::status_code(response) != 200) {
    message("Error querying REDCap for instances. Using instance 1.")
    return(1)
  }

  records <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

  # If no records found, use instance 1
  if (length(records) == 0 || nrow(records) == 0) {
    message("No existing scholarship instances found. Using instance 1.")
    return(1)
  }

  # Extract the instances and find the max
  if ("redcap_repeat_instance" %in% names(records)) {
    instances <- records$redcap_repeat_instance
    instances <- as.numeric(instances[!is.na(instances)])

    if (length(instances) > 0) {
      next_instance <- max(instances) + 1
      message("Found existing instances. Next instance will be ", next_instance)
      return(next_instance)
    }
  }

  # Default to instance 1 if no existing instances found
  message("No valid instances found. Using instance 1.")
  return(1)
}

# Improved helper function that creates a new instance every time for scholarship items
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
      # FIXED: Use redcap_uri parameter instead of global redcap_url
      data <- redcap_read(
        redcap_uri = redcap_uri,  # Use the parameter passed to the function
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
  # Use the standardized order of periods
  period_sequence <- c(
    "Entering Residency" = NA,  # No previous period
    "Mid Intern" = "Entering Residency",
    "End Intern" = "Mid Intern",
    "Mid PGY2" = "End Intern",
    "End PGY2" = "Mid PGY2",
    "Mid PGY3" = "End PGY2",
    "Graduating" = "Mid PGY3"
  )

  return(period_sequence[current_period])
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



# Get the next available instance for scholarship tracking
get_next_scholarship_instance <- function(record_id, redcap_url, token) {
  # Query REDCap for existing instances
  response <- httr::POST(
    url = redcap_url,
    body = list(
      token = token,
      content = "record",
      action = "export",
      format = "json",
      type = "flat",
      records = record_id,
      fields = "record_id",
      forms = "scholarship_tracking",
      returnFormat = "json"
    ),
    encode = "form"
  )

  # Process response
  if (httr::status_code(response) != 200) {
    message("Error querying REDCap for instances. Using instance 1.")
    return(1)
  }

  records <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

  # If no records found, use instance 1
  if (length(records) == 0 || nrow(records) == 0) {
    message("No existing scholarship instances found. Using instance 1.")
    return(1)
  }

  # Extract the instances and find the max
  if ("redcap_repeat_instance" %in% names(records)) {
    instances <- records$redcap_repeat_instance
    instances <- as.numeric(instances[!is.na(instances)])

    if (length(instances) > 0) {
      next_instance <- max(instances) + 1
      message("Found existing instances. Next instance will be ", next_instance)
      return(next_instance)
    }
  }

  # Default to instance 1 if no existing instances found
  message("No valid instances found. Using instance 1.")
  return(1)
}

# Get appropriate instance number for scholarship submissions
get_proper_scholarship_instance <- function(record_id, redcap_url, token) {
  # Debug message
  message("Finding appropriate instance number for record ID: ", record_id)

  # Query REDCap for existing scholarship instances for this record
  response <- httr::POST(
    url = redcap_url,
    body = list(
      token = token,
      content = "record",
      action = "export",
      format = "json",
      type = "flat",
      records = record_id,
      forms = "scholarship",  # Use your actual form name here
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      returnFormat = "json"
    ),
    encode = "form"
  )

  # Check if request was successful
  if (httr::status_code(response) != 200) {
    message("Error querying REDCap API. Using instance 1.")
    return(1)
  }

  # Parse the response
  result_text <- httr::content(response, "text", encoding = "UTF-8")
  records <- tryCatch({
    jsonlite::fromJSON(result_text)
  }, error = function(e) {
    message("Error parsing JSON response: ", e$message)
    return(NULL)
  })

  # If no records found or empty result, use instance 1
  if (is.null(records) || length(records) == 0 || nrow(records) == 0) {
    message("No existing scholarship records found. Using instance 1.")
    return(1)
  }

  # Filter for only scholarship records (in case other forms came back)
  records <- records[records$redcap_repeat_instrument == "scholarship", ]

  if (nrow(records) == 0) {
    message("No existing scholarship records found after filtering. Using instance 1.")
    return(1)
  }

  # Get the max instance number and add 1
  max_instance <- max(as.numeric(records$redcap_repeat_instance), na.rm = TRUE)
  next_instance <- max_instance + 1

  message("Found ", nrow(records), " existing scholarship records. Next instance will be ", next_instance)
  return(next_instance)
}
# Reliable submission function for scholarship data
reliable_scholarship_submission <- function(record_id, field_data, redcap_url, token) {
  # Ensure record_id is character
  record_id <- as.character(record_id)

  # Get proper instance number
  instance <- get_proper_scholarship_instance(record_id, redcap_url, token)
  instance <- as.character(instance)

  # Debug what's being submitted
  message("Scholarship submission data:")
  message("  record_id: ", record_id)
  message("  redcap_repeat_instrument: scholarship")
  message("  redcap_repeat_instance: ", instance)
  message("  scholarship_complete: 0")

  # Print all fields being submitted
  for (field_name in names(field_data)) {
    message("  ", field_name, ": ", field_data[[field_name]])
  }

  # Build the data payload
  data_list <- list(
    record_id = record_id,
    redcap_repeat_instrument = "scholarship",
    redcap_repeat_instance = instance,
    scholarship_complete = "0"
  )

  # Add all the field data
  for (field_name in names(field_data)) {
    if (!is.null(field_data[[field_name]]) && !is.na(field_data[[field_name]])) {
      data_list[[field_name]] <- field_data[[field_name]]
    }
  }

  # Convert to JSON
  data_json <- jsonlite::toJSON(list(data_list), auto_unbox = TRUE)

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
      data = data_json,
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

  if (status_code == 200) {
    return(list(
      success = TRUE,
      outcome_message = paste("Successfully submitted scholarship data for record", record_id),
      instance = instance
    ))
  } else {
    return(list(
      success = FALSE,
      outcome_message = paste("Failed to submit scholarship data for record", record_id, ":", response_content),
      instance = NULL
    ))
  }
}


# Function to filter and display scholarship data for a specific resident
display_filtered_scholarship_data <- function(schol_data, record_id) {
  # Debug
  message("Filtering scholarship data for record_id: ", record_id)

  # Ensure schol_data is a data frame
  if (!is.data.frame(schol_data)) {
    message("Error: schol_data is not a data frame")
    return(NULL)
  }

  # Check if record_id column exists
  if (!"record_id" %in% names(schol_data)) {
    message("Error: record_id column not found in scholarship data")
    return(NULL)
  }

  # Filter the data
  filtered_data <- schol_data[schol_data$record_id == record_id, ]

  message("After filtering, found ", nrow(filtered_data), " rows")

  # Return the filtered data
  return(filtered_data)
}

scholarship_module_ui <- function(id, rdm_dict){
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

    # Patient Safety Achievements moved here - right after intro
    uiOutput(ns("achievement_notifications")),

    # Horizontal rule to separate achievements from form
    hr(),

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
      DTOutput(ns("scholarship_table"))
      # Achievement notifications moved to top of page
    ),

    # Next button at the bottom
    div(
      class = "mt-4 text-center",
      actionButton(ns("next_btn"), "Next Section", class="btn-primary btn-lg")
    )
  )
}


scholarship_module_server <- function(id, rdm_dict, record_id, schol_data = NULL, token = NULL) {
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

    scholarship_results <- reactive({
      req(record_id())
      cat("DEBUG: Creating scholarship_results for record ID:", record_id(), "\n")

      # Get the actual record_id value
      actual_record_id <- if (is.reactive(record_id)) record_id() else record_id

      # Use the passed parameter instead of referencing a global variable
      if (is.null(schol_data)) {
        cat("DEBUG: schol_data is NULL\n")
        list(
          table_data = data.frame(
            Scholarship_Type = "No scholarship activities found",
            Description = "",
            stringsAsFactors = FALSE
          ),
          completed_ps = FALSE,
          completed_rca = FALSE
        )
      } else {
        cat("DEBUG: Processing scholarship data\n")

        # Handle schol_data being a reactive
        actual_data <- if (is.reactive(schol_data)) schol_data() else schol_data

        # Make sure we have valid data
        if (!is.data.frame(actual_data)) {
          cat("DEBUG: schol_data is not a data frame\n")
          return(list(
            table_data = data.frame(
              Scholarship_Type = "No scholarship activities found",
              Description = "",
              stringsAsFactors = FALSE
            ),
            completed_ps = FALSE,
            completed_rca = FALSE
          ))
        }

        result <- process_scholarship_data(actual_data, actual_record_id, rdm_dict)
        cat("DEBUG: Process complete, PS:", result$completed_ps, "RCA:", result$completed_rca, "\n")
        result
      }
    })

    # Updated process_scholarship_data function to handle actual data format
    process_scholarship_data <- function(data, record_id, rdm_dict) {
      cat("DEBUG: Processing scholarship data for record_id:", record_id, "\n")

      # Make sure record_id is a character
      record_id <- as.character(record_id)

      # Filter by record_id
      filtered_data <- data[data$record_id == record_id, ]

      cat("DEBUG: After filtering, found", nrow(filtered_data), "rows\n")

      # Skip processing if no data is found
      if (nrow(filtered_data) == 0) {
        return(list(
          table_data = data.frame(
            Scholarship_Type = character(0),
            Description = character(0),
            stringsAsFactors = FALSE
          ),
          completed_ps = FALSE,
          completed_rca = FALSE
        ))
      }

      # Check for Patient Safety and RCA completion
      completed_ps <- FALSE
      completed_rca <- FALSE

      # Check if columns exist first
      if ("schol_ps" %in% names(filtered_data)) {
        completed_ps <- any(filtered_data$schol_ps == "1" |
                              filtered_data$schol_ps == 1 |
                              filtered_data$schol_ps == "Yes",
                            na.rm = TRUE)
      }

      if ("schol_rca" %in% names(filtered_data)) {
        completed_rca <- any(filtered_data$schol_rca == "1" |
                               filtered_data$schol_rca == 1 |
                               filtered_data$schol_rca == "Yes",
                             na.rm = TRUE)
      }

      cat("DEBUG: PS flag =", completed_ps, ", RCA flag =", completed_rca, "\n")

      # Create a display table based on what's available
      if ("schol_type" %in% names(filtered_data)) {
        # Create a safe version that avoids using field names that might not exist
        table_data <- data.frame(
          Scholarship_Type = as.character(filtered_data$schol_type),
          Description = "",
          stringsAsFactors = FALSE
        )

        # Add description if available, checking each possible field
        for (i in 1:nrow(table_data)) {
          desc <- ""

          if ("schol_cit" %in% names(filtered_data) && !is.na(filtered_data$schol_cit[i])) {
            desc <- filtered_data$schol_cit[i]
          } else if ("schol_res" %in% names(filtered_data) && !is.na(filtered_data$schol_res[i])) {
            desc <- filtered_data$schol_res[i]
          } else if ("schol_qi" %in% names(filtered_data) && !is.na(filtered_data$schol_qi[i])) {
            desc <- filtered_data$schol_qi[i]
          } else if ("schol_pres_conf" %in% names(filtered_data) && !is.na(filtered_data$schol_pres_conf[i])) {
            desc <- filtered_data$schol_pres_conf[i]
          } else if ("schol_comm" %in% names(filtered_data) && !is.na(filtered_data$schol_comm[i])) {
            desc <- filtered_data$schol_comm[i]
          }

          table_data$Description[i] <- desc
        }
      } else {
        # No schol_type column - create empty table
        table_data <- data.frame(
          Scholarship_Type = character(0),
          Description = character(0),
          stringsAsFactors = FALSE
        )
      }

      # Return the results
      list(
        table_data = table_data,
        completed_ps = completed_ps,
        completed_rca = completed_rca
      )
    }

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

    # Replace the achievement_notifications renderUI with this version
    # Replace the achievement_notifications renderUI with this updated version
    output$achievement_notifications <- renderUI({
      # Get results
      results <- scholarship_results()
      if (is.null(results)) {
        return(NULL)
      }

      # Get the completion flags
      completed_ps <- results$completed_ps
      completed_rca <- results$completed_rca
      cat("DEBUG: PS flag =", completed_ps, ", RCA flag =", completed_rca, "\n")

      # Build notifications - ALWAYS show status for both PS and RCA
      notifications <- tagList()

      # Patient Safety Review status
      if (isTRUE(completed_ps)) {
        notifications <- tagAppendChild(
          notifications,
          tags$div(class = "alert alert-success mt-3",
                   tags$p(
                     tags$span(icon("check-circle"), class = "text-success me-2"),
                     tags$strong("Achievement: "),
                     "You have completed a Patient Safety Review"
                   )
          )
        )
      } else {
        notifications <- tagAppendChild(
          notifications,
          tags$div(class = "alert alert-danger mt-3",
                   tags$p(
                     tags$span(icon("exclamation-circle"), class = "text-danger me-2"),
                     tags$strong("Pending: "),
                     "You have not yet completed a Patient Safety Review"
                   )
          )
        )
      }

      # Root Cause Analysis status
      if (isTRUE(completed_rca)) {
        notifications <- tagAppendChild(
          notifications,
          tags$div(class = "alert alert-success mt-3",
                   tags$p(
                     tags$span(icon("check-circle"), class = "text-success me-2"),
                     tags$strong("Achievement: "),
                     "You have completed a Root Cause Analysis"
                   )
          )
        )
      } else {
        notifications <- tagAppendChild(
          notifications,
          tags$div(class = "alert alert-danger mt-3",
                   tags$p(
                     tags$span(icon("exclamation-circle"), class = "text-danger me-2"),
                     tags$strong("Pending: "),
                     "You have not yet completed a Root Cause Analysis"
                   )
          )
        )
      }

      # Return the notifications
      notifications
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

    # ---- IMPROVED FUNCTIONS FOR BETTER SUBMISSION ----

    # FIXED: Get appropriate instance number for scholarship submissions
    get_proper_scholarship_instance <- function(record_id, redcap_url, token) {
      # Debug message
      message("Finding appropriate instance number for record ID: ", record_id)

      # Query REDCap for existing scholarship instances for this record
      response <- httr::POST(
        url = redcap_url,
        body = list(
          token = token,
          content = "record",
          action = "export",
          format = "json",
          type = "flat",
          records = record_id,
          forms = "scholarship",  # Use your actual form name here
          rawOrLabel = "raw",
          rawOrLabelHeaders = "raw",
          exportCheckboxLabel = "false",
          returnFormat = "json"
        ),
        encode = "form"
      )

      # Check if request was successful
      if (httr::status_code(response) != 200) {
        message("Error querying REDCap API. Using timestamp instance.")
        # Use timestamp as fallback
        timestamp <- as.numeric(format(Sys.time(), "%Y%m%d%H%M%S"))
        return(timestamp %% 10000)
      }

      # Parse the response
      result_text <- httr::content(response, "text", encoding = "UTF-8")
      records <- tryCatch({
        jsonlite::fromJSON(result_text)
      }, error = function(e) {
        message("Error parsing JSON response: ", e$message)
        # Use timestamp as fallback
        timestamp <- as.numeric(format(Sys.time(), "%Y%m%d%H%M%S"))
        return(timestamp %% 10000)
      })

      # If no records found or empty result, use timestamp-based instance
      if (is.null(records) || length(records) == 0 || nrow(records) == 0) {
        message("No existing scholarship records found. Using timestamp instance.")
        timestamp <- as.numeric(format(Sys.time(), "%Y%m%d%H%M%S"))
        return(timestamp %% 10000)
      }

      # Filter for only scholarship records (in case other forms came back)
      records <- records[records$redcap_repeat_instrument == "scholarship", ]

      if (nrow(records) == 0) {
        message("No existing scholarship records found after filtering. Using timestamp instance.")
        timestamp <- as.numeric(format(Sys.time(), "%Y%m%d%H%M%S"))
        return(timestamp %% 10000)
      }

      # Get the max instance number and add 1
      if ("redcap_repeat_instance" %in% names(records)) {
        instances <- as.numeric(records$redcap_repeat_instance)
        if (length(instances) > 0 && !all(is.na(instances))) {
          max_instance <- max(instances, na.rm = TRUE)
          next_instance <- max_instance + 1
          message("Found ", nrow(records), " existing scholarship records. Next instance will be ", next_instance)
          return(next_instance)
        }
      }

      # Fallback to timestamp if something went wrong
      message("Could not determine next instance. Using timestamp.")
      timestamp <- as.numeric(format(Sys.time(), "%Y%m%d%H%M%S"))
      return(timestamp %% 10000)
    }

    # FIXED: Reliable submission function for scholarship data
    reliable_scholarship_submission <- function(record_id, field_data, redcap_url, token) {
      # Ensure record_id is character
      record_id <- as.character(record_id)

      # Get proper instance number - ALWAYS GET A NEW ONE
      timestamp <- as.numeric(format(Sys.time(), "%Y%m%d%H%M%S"))
      instance <- as.character(timestamp %% 10000)
      message("Using timestamp-based instance number: ", instance, " for scholarship")

      # Debug what's being submitted
      message("Scholarship submission data:")
      message("  record_id: ", record_id)
      message("  redcap_repeat_instrument: scholarship")
      message("  redcap_repeat_instance: ", instance)
      message("  scholarship_complete: 0")

      # Print all fields being submitted
      for (field_name in names(field_data)) {
        message("  ", field_name, ": ", field_data[[field_name]])
      }

      # Build the data payload
      data_list <- list(
        record_id = record_id,
        redcap_repeat_instrument = "scholarship",
        redcap_repeat_instance = instance,
        scholarship_complete = "0"
      )

      # Add all the field data
      for (field_name in names(field_data)) {
        if (!is.null(field_data[[field_name]]) && !is.na(field_data[[field_name]])) {
          data_list[[field_name]] <- field_data[[field_name]]
        }
      }

      # Convert to JSON
      data_json <- jsonlite::toJSON(list(data_list), auto_unbox = TRUE)

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
          data = data_json,
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

      if (status_code == 200) {
        return(list(
          success = TRUE,
          outcome_message = paste("Successfully submitted scholarship data for record", record_id),
          instance = instance
        ))
      } else {
        return(list(
          success = FALSE,
          outcome_message = paste("Failed to submit scholarship data for record", record_id, ":", response_content),
          instance = NULL
        ))
      }
    }

    # Handle scholarship submission - wrapper function that uses the reliable submission
    handle_scholarship_submission <- function(record_id, values, token = NULL) {
      # Use hardcoded URL
      redcap_url_string <- "https://redcapsurvey.slu.edu/api/"

      # Get token if not provided
      if (is.null(token)) {
        if (exists("rdm_token", envir = parent.frame())) {
          token <- get("rdm_token", envir = parent.frame())
        } else if (exists("rdm_token", envir = .GlobalEnv)) {
          token <- get("rdm_token", envir = .GlobalEnv)
        } else {
          stop("REDCap token not found")
        }
      }

      # Get record_id
      actual_record_id <- if (is.reactive(record_id)) record_id() else record_id

      # Add debugging for all values
      message("SUBMITTING SCHOLARSHIP VALUES:")
      for (k in names(values)) {
        message("  ", k, " = ", values[[k]])
      }

      # Process Yes/No fields
      yes_no_fields <- c("schol_ps", "schol_rca", "schol_pres", "schol_pub")
      for (field in names(values)) {
        if (field %in% yes_no_fields) {
          if (values[[field]] == "Yes" || values[[field]] == 1) {
            values[[field]] <- "1"
          } else if (values[[field]] == "No" || values[[field]] == 0) {
            values[[field]] <- "0"
          }
        }
      }

      # Use the reliable submission function
      return(reliable_scholarship_submission(
        record_id = actual_record_id,
        field_data = values,
        redcap_url = redcap_url_string,
        token = token
      ))
    }

    # Save a presentation using the reliable submission function
    observeEvent(input$save_pres, {
      removeModal()

      message("Saving presentation with citation: ", input$p_cit)

      handle_scholarship_submission(
        record_id = record_id,
        values = list(
          schol_type = input$type,
          schol_pres_type = input$p_type,
          schol_pres_conf = input$p_conf,
          schol_cit = input$p_cit,
          schol_pres = "Yes"  # Explicitly set this to Yes
        ),
        token = token  # Pass the token parameter
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

    # Save a publication using the reliable submission function
    observeEvent(input$save_pub, {
      removeModal()

      message("Saving publication with citation: ", input$pub_cit)

      handle_scholarship_submission(
        record_id = record_id,
        values = list(
          schol_type = input$type,
          schol_cit = input$pub_cit,  # This is the citation field that must be saved
          schol_pub = "Yes"  # Explicitly set this to Yes
        ),
        token = token  # Pass the token parameter
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
        "schol_pub",
        "schol_cit",          # Include citation field
        "schol_pres_type",
        "schol_pres_conf",
        "schol_comm",
        "schol_comm_type",
        "schol_comm_other"
      )

      # Filter only the keys that actually exist in vals
      keep <- keep[keep %in% names(vals)]

      # Get only the values that exist and ensure they're all vectors
      project_data <- list()
      for (k in keep) {
        if (!is.null(vals[[k]]) && vals[[k]] != "") {
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
        ## now submit using the reliable submission function
        tryCatch({
          result <- handle_scholarship_submission(
            record_id = record_id,
            values = project_data,
            token = token  # Pass the token parameter
          )

          if (result$success) {
            showNotification("Project saved", type = "message")
          } else {
            showNotification(paste("Error saving project:", result$outcome_message), type = "error")
          }
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

# Simple helper function
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

#' Safely escape a string for JSON inclusion
#'
#' @param str String to escape
#' @return Escaped string safe for JSON
#' @export
# Function to safely escape JSON strings
escape_json_string <- function(str) {
  if (is.null(str)) return("")
  if (!is.character(str)) str <- as.character(str)

  # Replace any backslashes first
  str <- gsub('\\', '\\\\', str, fixed = TRUE)

  # Then replace double quotes
  str <- gsub('"', '\\"', str, fixed = TRUE)

  # Handle other special characters if needed
  str <- gsub('\n', '\\n', str, fixed = TRUE)
  str <- gsub('\r', '\\r', str, fixed = TRUE)
  str <- gsub('\t', '\\t', str, fixed = TRUE)

  return(str)
}

reliable_scholarship_submission <- function(record_id, field_data, redcap_url, token) {
  # Ensure record_id is character
  record_id <- as.character(record_id)

  # Get proper instance number
  instance <- get_proper_scholarship_instance(record_id, redcap_url, token)
  instance <- as.character(instance)

  # Debug what's being submitted
  message("Scholarship submission data:")
  message("  record_id: ", record_id)
  message("  redcap_repeat_instrument: scholarship")
  message("  redcap_repeat_instance: ", instance)
  message("  scholarship_complete: 0")

  # Print all fields being submitted (with sanitized values for logging)
  for (field_name in names(field_data)) {
    safe_value <- if (is.null(field_data[[field_name]]) || is.na(field_data[[field_name]])) {
      "NULL or NA"
    } else {
      # Limit length of logged values for clarity
      val <- as.character(field_data[[field_name]])
      if (nchar(val) > 30) {
        paste0(substr(val, 1, 30), "...")
      } else {
        val
      }
    }
    message("  ", field_name, ": ", safe_value)
  }

  # Try-catch block to handle any errors
  tryCatch({
    # Use manual JSON construction with the improved escape_json_string function
    data_str <- sprintf('[{"record_id":"%s","redcap_repeat_instrument":"scholarship","redcap_repeat_instance":"%s","scholarship_complete":"0"',
                        escape_json_string(record_id), escape_json_string(instance))

    # Add all fields with proper escaping
    for (field_name in names(field_data)) {
      if (!is.null(field_data[[field_name]]) && !is.na(field_data[[field_name]])) {
        value <- escape_json_string(field_data[[field_name]])
        data_str <- paste0(data_str, sprintf(',"%s":"%s"', field_name, value))
      }
    }

    # Close JSON structure
    data_str <- paste0(data_str, "}]")

    message("JSON data (first 100 chars): ", substr(data_str, 1, 100))

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

    if (status_code == 200) {
      return(list(
        success = TRUE,
        outcome_message = paste("Successfully submitted scholarship data for record", record_id),
        instance = instance
      ))
    } else {
      return(list(
        success = FALSE,
        outcome_message = paste("Failed to submit data for record", record_id, ":", response_content),
        instance = NULL
      ))
    }
  }, error = function(e) {
    # Capture any errors in JSON processing or API call
    message("ERROR in scholarship submission: ", e$message)
    return(list(
      success = FALSE,
      outcome_message = paste("Error in submission process:", e$message),
      instance = NULL
    ))
  })
}

reliable_s_eval_submission <- function(record_id, period, processed_inputs, redcap_url, token) {
  # Ensure record_id is character
  record_id <- as.character(record_id)

  # Map text period to numeric code
  period_mapping <- c(
    "Entering Residency" = 7,
    "Mid Intern" = 1,
    "End Intern" = 2,
    "Mid PGY2" = 3,
    "End PGY2" = 4,
    "Mid PGY3" = 5,
    "Graduating" = 6
  )

  # Get period code - this will also be our instance number
  if (is.character(period) && period %in% names(period_mapping)) {
    period_code <- as.character(period_mapping[period])
    instance_number <- period_code
  } else if (is.numeric(period)) {
    period_code <- as.character(period)
    instance_number <- period_code
  } else {
    period_code <- "1"
    instance_number <- "1"
  }

  message("Using period-based instance number: ", instance_number, " for s_eval")

  # Build data with robust character escaping
  tryCatch({
    # Start the JSON structure
    data_str <- sprintf('[{"record_id":"%s","redcap_repeat_instrument":"s_eval","redcap_repeat_instance":"%s","s_e_date":"%s","s_e_period":"%s","s_eval_complete":"0"',
                        escape_json_string(record_id),
                        escape_json_string(instance_number),
                        escape_json_string(format(Sys.Date(), "%Y-%m-%d")),
                        escape_json_string(period_code)
    )

    # Add all fields with proper escaping
    for (field in names(processed_inputs)) {
      if (!is.null(processed_inputs[[field]]) && !is.na(processed_inputs[[field]])) {
        value <- escape_json_string(processed_inputs[[field]])
        data_str <- paste0(data_str, sprintf(',"%s":"%s"', field, value))
      }
    }

    # Close the object and array
    data_str <- paste0(data_str, "}]")

    message("Submitting JSON data (first 100 chars): ", substr(data_str, 1, 100))

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

    if (status_code == 200) {
      return(list(
        success = TRUE,
        outcome_message = paste("Successfully submitted data for record", record_id)
      ))
    } else {
      return(list(
        success = FALSE,
        outcome_message = paste("Failed to submit data for record", record_id, ":", response_content)
      ))
    }
  }, error = function(e) {
    # Handle any JSON processing errors
    message("Error processing JSON: ", e$message)
    return(list(
      success = FALSE,
      outcome_message = paste("JSON processing error:", e$message)
    ))
  })
}

# Generic reliable submission function for any instrument
reliable_redcap_submission <- function(record_id, instrument, instance, fields, redcap_url, token) {
  # Ensure record_id is character
  record_id <- as.character(record_id)

  # Ensure instance is character
  instance <- as.character(instance)

  message("Using instance number: ", instance, " for ", instrument)

  # Build data manually in the exact format REDCap expects
  data_str <- sprintf(
    '[{"record_id":"%s","redcap_repeat_instrument":"%s","redcap_repeat_instance":"%s"',
    record_id, instrument, instance
  )

  # Add completion field
  complete_field <- paste0(instrument, "_complete")
  data_str <- paste0(data_str, sprintf(',"%s":"0"', complete_field))

  # Add all other fields
  for (field in names(fields)) {
    if (!is.null(fields[[field]]) && !is.na(fields[[field]])) {
      # Escape quotes in values
      value <- gsub('"', '\\"', as.character(fields[[field]]))
      data_str <- paste0(data_str, sprintf(',"%s":"%s"', field, value))
    }
  }

  # Close the object and array
  data_str <- paste0(data_str, "}]")

  message("Submitting manual JSON data (first 100 chars): ", substr(data_str, 1, 100))

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

  if (status_code == 200) {
    return(list(
      success = TRUE,
      outcome_message = paste("Successfully submitted data for record", record_id)
    ))
  } else {
    return(list(
      success = FALSE,
      outcome_message = paste("Failed to submit data for record", record_id, ":", response_content)
    ))
  }
}
