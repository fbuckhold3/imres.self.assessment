ui <- fluidPage(
  theme = bs_theme(version = 5),
  useShinyjs(),
  tags$style(HTML("
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
  div(
    class = "hidden-card",
    textInput("is_valid_code", label = NULL, value = FALSE)
  ),

  # Initial access code screen
  div(
    id = "access_code_screen",
    class = "center-screen",
    div(
      class = "access-code-container",
      h1("IMSLU Resident Self-Assessment Form", class = "mb-4"),
      div(
        class = "mb-3",
        textInput("access_code_input", "Access Code:", placeholder = "ABC123")
      ),
      div(
        class = "mb-3",
        actionButton("validate_code", "Submit", class = "btn-primary")
      ),
      conditionalPanel(
        condition = "input.access_code_input && !input.is_valid_code",
        p("❌ Invalid access code, try again", style = "color:red;")
      )
    )
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


      # Intro Card
      div(
        id    = "intro_card",
        class = "hidden-card",
        card(
          card_header("Welcome & Intern Intro"),
          card_body(
            p("Welcome to the IMSLU Residency Program! The following serves as a way to introduce yourself to the program and for us to get an understanding of your strengths and opportunities for improvement. Early in your intern year, we will meet to review this and discuss your transition to resident."),
            p("The name shown here is used for data collection. If incorrect, email Dr. Buckhold at fred.buckhold@slucare.ssmhealth.com."),
            p("The following three questions are part of a grant‑funded data collection for Missouri residency positions."),

            radioButtons("hs_mo",     "Did you get any of your high school education in Missouri?", choices = c("Yes","No"), inline = TRUE),
            radioButtons("college_mo","Did you have any college education in Missouri?",              choices = c("Yes","No"), inline = TRUE),
            radioButtons("med_mo",    "Did you go to medical school in Missouri?",                   choices = c("Yes","No"), inline = TRUE),

            actionButton("intro_next", "Next Section", class = "btn-primary")
          )
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
      div(
        id    = "section2_card",
        class = "hidden-card",
        card(
          card_header("2. Reflection on knowledge"),
          card_body(
            uiOutput("card2UI"),                         # ← this is the placeholder
            actionButton("section2_next",
                         "Next Section",
                         class = "btn-primary")
          )
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
        id    = "section4_card", class = "hidden-card",
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

      # Card 6: Goals & submission
      div(
        id = "section6_card",
        class = "hidden-card",
        card(
          card_header("Review and Implement New Goals"),
          card_body(
            fluidRow(
              # Left column (4/12) - Milestone Visualization
              column(
                width = 4,
                div(
                  class = "milestone-visualization p-2",
                  h4("Your Current Milestone Assessment", class = "mb-3"),
                  plotOutput("current_milestone_plot", height = "380px")
                )
              ),

              # Right column (8/12) - Goal Setting Placeholder
              column(
                width = 8,
                div(
                  class = "goal-setting p-2",
                  h4("Set Development Goals", class = "mb-3"),
                  p("Based on your milestone assessment, set three goals to focus on for the upcoming period."),

                  # Goal 1
                  div(
                    class = "mb-3",
                    textAreaInput("goal1", "Goal 1:", rows = 2, width = "100%"),
                    dateInput("goal1_deadline", "Deadline for Goal 1:", width = "50%")
                  ),

                  # Goal 2
                  div(
                    class = "mb-3",
                    textAreaInput("goal2", "Goal 2:", rows = 2, width = "100%"),
                    dateInput("goal2_deadline", "Deadline for Goal 2:", width = "50%")
                  ),

                  # Goal 3
                  div(
                    class = "mb-3",
                    textAreaInput("goal3", "Goal 3:", rows = 2, width = "100%"),
                    dateInput("goal3_deadline", "Deadline for Goal 3:", width = "50%")
                  ),

                  # Submit button
                  div(
                    class = "text-center mt-4",
                    actionButton("submit", "Submit Self-Assessment", class = "btn-success btn-lg")
                  )
                )
              )
            )
          )
        )
      ),

      # Completion
      div(
        id = "completion_card", class = "hidden-card",
        card(
          card_header("Self-Assessment Completed"),
          card_body(
            p("Thank you for completing your self-assessment!"),
            p("Your responses have been recorded.")
          )
        )
      )
    )
  )
)
