ui <- fluidPage(
  theme = bs_theme(version = 5),
  useShinyjs(),
  tags$style(HTML("
  /* Imres colors */
    :root {
      --imres-bg: #FFFFFF;
      --imres-text: #333333;
      --imres-primary: #0072B2;
      --imres-secondary: #56B4E9;
      --imres-success: #009E73;
      --imres-warning: #E69F00;
      --imres-danger: #D55E00;
    }

    /* General spacing and styling */
    .section-container {
      background-color: var(--imres-bg);
      padding: 2rem;
      margin-bottom: 2rem;
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }

    .section-header {
      color: var(--imres-primary);
      margin-bottom: 1.5rem;
      padding-bottom: 1rem;
      border-bottom: 2px solid var(--imres-secondary);
    }

    .question-group {
      background-color: #f8f9fa;
      padding: 1.5rem;
      margin-bottom: 1.5rem;
      border-radius: 6px;
      border-left: 4px solid var(--imres-secondary);
    }

    .question-header {
      color: var(--imres-primary);
      margin-bottom: 1rem;
    }

    .question-description {
      color: #666;
      margin-bottom: 1rem;
      font-style: italic;
    }

    /* Progress indicator styling */
    .progress-indicator {
      display: flex;
      justify-content: center;
      margin-bottom: 2rem;
    }

    .progress-step {
      width: 30px;
      height: 30px;
      border-radius: 50%;
      background-color: #ddd;
      color: white;
      display: flex;
      align-items: center;
      justify-content: center;
      margin: 0 10px;
    }

    .progress-step.active {
      background-color: var(--imres-primary);
    }

    .progress-step.completed {
      background-color: var(--imres-success);
    }
  .hidden-card { display: none; }
  .center-screen {
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    text-align: center;
    width: 80%;
    max-width: 500px;
  }
  .access-code-container {
    padding: 2rem;
    border-radius: 8px;
    background: white;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  /* Hide the validation input */
  #is_valid_code { display: none; }
")),

  # Add this somewhere in your UI, probably near the top
  # Initial access code screen
  div(
    id = "access_code_screen",
    class = "center-screen",
    style = "max-width: 1000px; width: 90%;", # Twice as wide
    div(
      class = "access-code-container",
      h1("IMSLU Resident Self-Assessment Form", class = "mb-4 text-center",
         style = "color: #0072B2; font-weight: bold;"),

      div(class = "p-4 bg-light rounded mb-4",
          h4("The following is a semi-annual self-assessment for residents in the SSM-SLUH Internal Medicine Program.",
             class = "mb-3"),
          p("This assessment will help in developing your own 'Individualized Learning Program (ILP)'.
        The data collected will be anonymized and used by our Program Evaluation Committee (PEC)
        to improve the program.", class = "mb-3", style = "font-size: 1.1rem;"),
          p("Please dedicate about 20-30 minutes to complete this assessment.
        The more effort you put in, the better we can coach you.",
            class = "mb-3", style = "font-size: 1.1rem;"),
          p("If you encounter any issues with this application, please contact Dr. Buckhold.
        While much of this was developed with the assistance of AI, he will do his best to resolve any problems.",
            style = "font-style: italic; font-size: 1rem;")
      ),

      div(
        class = "mb-4 p-4 border rounded bg-white",
        h4("Enter Your Access Code", class = "mb-3 text-center"),
        div(
          class = "mb-3",
          textInput("access_code_input", NULL, placeholder = "Enter your access code (e.g., ABC123)",
                    width = "100%")
        ),
        div(
          class = "text-center",
          actionButton("validate_code", "Submit",
                       class = "btn-primary btn-lg",
                       style = "min-width: 150px;")
        ),
        conditionalPanel(
          condition = "input.access_code_input && input.is_valid_code === false",
          div(
            class = "mt-3 p-2 bg-danger text-white rounded text-center",
            icon("exclamation-triangle"),
            " Invalid access code, please try again"
          )
        )
      )
    )
  ),

  div(
    class = "hidden-card",
    textInput("is_valid_code", label = NULL, value = FALSE)
  ),

  div(
    id = "main_app_content",
    style = "display: none;",
    div(
      class = "container mt-4",
      # Header: resident & coach
      div(
        class = "resident-header mb-4",
        style = "background: linear-gradient(135deg, #0072B2 20%, #56B4E9 80%); color: white; padding: 15px; border-radius: 8px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          h3(textOutput("resident_name"), style = "margin: 0; font-weight: bold;"),
          h4(textOutput("coach_name"), style = "margin: 0; font-weight: normal;")
        )
      ),

      # Add this period selection card
      div(
        id = "period_selection_card",
        card(
          card_header("Select Assessment Period"),
          card_body(
            mod_miles_select_ui("period_select"),  # This is your period selection module
            div(
              style = "display:flex;justify-content:space-between;",
              actionButton("period_next", "Begin Assessment", class = "btn-primary")
            )
          )
        )
      ),

      # Period selection help text
      div(
        id = "period_help_text",
        class = "period-help text-center mb-3",
        p(
          "To complete this self-assessment, please select the period to review. ",
          "This should reflect your current year, and the time - if doing in the Fall/Winter, pick 'mid', ",
          "if in the Spring pick 'end' or 'graduating'. Incoming interns - do 'Entering Residency'. ",
          "You're smart, you can figure it out."
        )
      ),

      # Plus/Delta Card
      div(
        id = "section1_card", class = "hidden-card",
        conditionalPanel(
          condition = "input.period_select !== 'Entering Residency'",
          card(
            card_header(
              tagList(
                "1. Plus/Delta Review",
                actionButton("open_modal", label = NULL, icon = icon("table"), class = "btn btn-link", title = "Review Plus/Delta Assessments")
              )
            ),
            card_body(
              fluidRow(column(12, h3("Please review your Plus/Delta comments, especially over the last six months. You can click the small icon above or the link below."))),
              br(),
              fluidRow(column(12, h3("What are you doing well?"))),
              textAreaInput("plus", label = NULL, rows = 3, width = "100%"),
              br(),
              fluidRow(column(12, h3("Where do you need to improve?"))),
              textAreaInput("delta", label = NULL, rows = 3, width = "100%"),
              br(),
              fluidRow(column(12, actionButton("section1_next", "Next Section", class = "btn-primary"))),
              br(),
              fluidRow(column(12, actionButton("reopen_modal", "Review Plus/Delta Assessments", icon = icon("table"), class = "btn-secondary")))
            )
          )
        )
      ),

      # Card 2: Skills & Extra Reflections
      # First, modify the card2 UI in your main UI file:
      div(
        id = "section2_card",
        class = "hidden-card",
        # Add the intro text section
        div(
          class = "mb-4 p-4 bg-light rounded",
          h3("Learning and Career Planning", class = "mb-3"),
          p("The following questions will be asking a few questions about your comfort with topics of medicine,
      how you like to learn, your board prep, and some questions about your career planning.
      We use this data both to help guiding your learning as well as to inform our curriculum for the Program")
        ),
        # Container for the dynamic content
        uiOutput("card2UI"),
        # Navigation buttons container
        div(
          class = "d-flex justify-content-between mt-4",
          actionButton("section2_prev", "Previous", class = "btn-secondary"),
          actionButton("section2_next", "Next", class = "btn-primary")
        )
      ),

      # Card 3: Review of Program
      div(
        id = "section3_card", class = "hidden-card",
        card(
          card_header("3. Review of Program"),
          card_body(
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
            ),

            tags$div(
              style = "margin-top: 30px;",
              actionButton("section3_next", "Next Section", class = "btn-primary")
            )
          )
        )
      ),

      # ─── Card 4: Scholarship ────────────────────────────────────────────────────────
      div(
        id = "section4_card",
        class = "hidden-card",
        card(
          card_header("4. Scholarship"),
          card_body(
            uiOutput("scholarship_module_ui")
          )
        )
      ),

      # _____ Card 5: Milestone self_assessment
      div(
        id = "section5_card",
        style = "display: none;",
        card(
          card_header("Milestone Self-Assessment"),
          card_body(
            # Previous milestone visualization section
            div(
              id = "previous_milestones_section",
              conditionalPanel(
                condition = "input.show_prev_milestones",
                h3("Previous Milestone Assessments", class = "mb-3"),
                p("These charts show your previous self-assessment (left) and the Clinical Competency Committee's ratings (right) from your previous evaluation period, if available."),
                fluidRow(
                  column(
                    width = 6,
                    div(
                      class = "self-assessment",
                      h4("Your Previous Self-Assessment"),
                      plotOutput("previous_self_plot", height = "400px")
                    )
                  ),
                  column(
                    width = 6,
                    div(
                      class = "program-assessment",
                      h4("Previous Program Assessment"),
                      plotOutput("previous_program_plot", height = "400px")
                    )
                  )
                )
              ),
              checkboxInput("show_prev_milestones", "Show Previous Milestone Assessments", value = TRUE)
            ),

            hr(),

            # Current milestone assessment module
            h3("Current Milestone Self-Assessment", class = "mb-3"),
            uiOutput("milestone_module_ui")
          )
        )
      ),

      # _____ Card 6: Milestone-based Goal Setting and Review
      div(
        id = "section6_card",
        class = "hidden-card",
        card(
          card_header("Milestone-based Goal Setting and Review"),
          card_body(
            # Introductory text
            div(
              class = "intro-text mb-4",
              p("As part of the development of your Individualized Learning Plan (\"ILP\"), we ask that you make three goals based on the Milestones you just self-reflected on. A spider plot is displayed to your right, compared to the median of what other residents at your level of training have self-rated in the past. Take a look at places were you perhaps lower than average, or a place you would like to improve."),

              br(),

              p("The purpose of this is to identify a milestone you think you can reach in the next six months. They are in 3 groupings - Patient Care / Medical Knowledge, Systems Based Practice / Practice-Based Learning, and Professionalism / Interpersonal Communication Skills. As you work through this, you will see the milestones presented."),

              br(),

              p("The last points: 1) Milestones often have rows - the selector below will ask for different rows and spell out the actual milestones you need to reach; 2) On selection, you may see that the first milestone doesn't display. Just click another one and come back. 3) As this is new, you may be asked if you reached your last Milestone goal, which you may not have set yet. Just say no, and explain why. Dr. B had a lot of trouble getting this to work..."),

              hr()
            ),

            # Goal Setting Module UI
            goalSettingUI("goals")


            )
          )
        ),
      # Card 7: Completion
      div(
        id    = "completion_card",
        class = "hidden-card",
        div(
          class = "center-screen",
          h2("🎉 All done!"),
          p("Thank you—your self‑assessment and ILP goals have been submitted."),
          actionButton("finish_back_to_top", "Back to Top", class = "btn-primary")
        )
      )# End of completion card
    )  # End of container mt-4
  )  # End of main_app_content
)  # End of fluidPage

