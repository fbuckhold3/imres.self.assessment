ui <- fluidPage(
  theme = bs_theme(version = 5),
  useShinyjs(),
  tags$style(HTML(
    ".hidden-card { display: none; }"
  )),

  # always show this
  div(
    class = "initial-screen text-center mt-5",
    h1("IMSLU Resident Self-Assessment Form"),
    p("Please enter your unique access code to start"),
    textInput("access_code_input","Access Code:",placeholder="ABC123")
  ),

  # show an error message *once* they type something invalid
  conditionalPanel(
    condition = "input.access_code_input && !output.valid_code",
    p("❌ Invalid access code, try again", style="color:red;")
  ),

  # everything else only shows when code is valid
  conditionalPanel(
    condition = "output.valid_code",
    div(
      class = "container mt-4",
      # Header: resident & coach
      div(
        class = "resident-header mb-4",
        style = "background: linear-gradient(135deg, #0072B2 20%, #56B4E9 80%); color: white; padding: 15px; border-radius: 8px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          h3(textOutput("resident_name"), style = "margin: 0; font-weight: bold;"),
          h4(textOutput("coach_name"),    style = "margin: 0; font-weight: normal;")
        )
      ),

      # Explanatory text for period selection (disappears after selection)
      conditionalPanel(
        condition = "!input.period_select",
        div(
          class = "period-help text-center mb-3",
          p(
            "To complete this self-assessment, please select the period to review. ",
            "This should reflect your current year, and the time - if doing in the Fall/Winter, pick 'mid', ",
            "if in the Spring pick 'end' or 'graduating'. Incoming interns - do 'Entering Residency'. ",
            "You're smart, you can figure it out."
          )
        )
      ),

      # Period Selection Card
      div(
        id = "period_selection_card",
        card(
          card_header("Select Assessment Period"),
          card_body(
            mod_miles_select_ui("period_select"),
            div(
              style = "display:flex;justify-content:space-between;",
              actionButton("period_next",        "Begin Assessment", class = "btn-primary"),
              div(
                id    = "custom_period_next_container",
                style = "display:none;",
                actionButton("custom_period_next","Begin Assessment", class = "btn-primary")
              )
            )
          )
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

      div(
        id = "section5_card", style = "display: none;",
          card(
            card_header("Milestone Self-Assessment"),
            card_body(
              uiOutput("milestone_module_ui")
            )
          )
      ),

      # Card 6: Goals & submission
      div(
        id = "section6_card", class = "hidden-card",
        card(
          card_header("6. Review and Implement New Goals"),
          card_body(
            textAreaInput("goal1", "Goal 1:", rows = 2), dateInput("goal1_deadline", "Deadline for Goal 1:"),
            textAreaInput("goal2", "Goal 2:", rows = 2), dateInput("goal2_deadline", "Deadline for Goal 2:"),
            textAreaInput("goal3", "Goal 3:", rows = 2), dateInput("goal3_deadline", "Deadline for Goal 3:"),
            actionButton("submit", "Submit Self-Assessment", class = "btn-success")
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
