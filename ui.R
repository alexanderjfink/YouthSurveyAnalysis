shinyUI(navbarPage("SPYC Data",
  tabPanel("Summary",
    fluidRow(
      column(6,
          h3("Race"),
          verbatimTextOutput("raceTable")
      ),
      column(6,
        h3("Gender"),
        verbatimTextOutput("genderTable")
      )
    ),
    fluidRow(
      column(6,
          h3("City"),
          verbatimTextOutput("city")
      ),
      column(6,
        h3("School"),
        verbatimTextOutput("school")

      )
    ),
    fluidRow(
      column(6,
        h3("Race / School Crosstab"),
        verbatimTextOutput("raceSchoolCross")
      ),
      column(6,
        h3("Gender / Race Crosstab"),
        verbatimTextOutput("raceGenderCross")
      )
    ),
    fluidRow(
      column(6,
        h3("Gender / School Crosstab"),
        verbatimTextOutput("genderSchoolCross")
      ),
      column(6,
        h3("Neighborhood"),
        verbatimTextOutput("neighborhood")
      )
    )
  ),

  tabPanel("Bar Plots",
      mainPanel(
        plotOutput("genderPlot"),
        plotOutput("racePlot"),
        plotOutput("schoolPlot")
      )
  ),

  tabPanel("Cleaned Data",
      mainPanel(
          dataTableOutput(outputId="dataTable")
      )
  ),

  tabPanel("Word Clouds",
    sidebarLayout(
      # Sidebar with a slider and selection inputs
      sidebarPanel(
        selectInput("query",
                    "Unfairness / Changes Devoted To",
                    choices = list("Unfairness (Uncoded)" = "Unfairness",
                                    "Unfairness (Coded)" = "CodedUnfairness",
                                    "Changes Devoted To (Uncoded)" = "ChangesDevotedTo",
                                    "Changes Devoted To (Coded)" = "CodedChangesDevotedTo")),

        hr(),

        selectInput("selection", "Choose a grouping model:",
                    choices = groups),

        conditionalPanel(
                condition = "input.selection == 'Gender'",
                selectInput("gender", "Gender",
                            choices = gender)),

        conditionalPanel(
                condition = "input.selection == 'Race'",
                selectInput("race", "Race",
                  choices = race)),

        conditionalPanel(
                condition = "input.selection == 'School'",
                selectInput("school", "School",
                  choices = school)),

        conditionalPanel(
                condition = "input.selection == 'City'",
                selectInput("city", "City",
                  choices = city)),

        actionButton("update", "Change"),
        hr(),

        textInput("toRemove",
                    label = "Remove These Words:",
                    value = "Separate words by commas"),

        helpText("Separate words by commas"),

        hr(),

        sliderInput("freq",
                    "Minimum Frequency:",
                    min = 1,  max = 50, value = 2),
        sliderInput("max",
                    "Maximum Number of Words:",
                    min = 1,  max = 300,  value = 100)
      ),

      # Show Word Cloud
      mainPanel(
        plotOutput("plot")
      )
    )
  )
))
