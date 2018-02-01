library(shiny)
library(shinyFiles)

shinyUI(fluidPage(

  titlePanel("Train model"),

  sidebarLayout(
    sidebarPanel(
      width = 2,
      shinyDirButton(
        "path",
        label = "database",
        title = "path"
      ),
      actionButton("step_backward", label = "<<<"),
      actionButton("step_forward", label = ">>>"),
      sliderInput(
        "starttime",
        label = "start time (ms)",
        value = 0,
        min = 0,
        max = 10000,
        animate = animationOptions(interval = 1000)
      ),
      sliderInput(
        "frequency",
        label = "frequency (kHz)",
        value = c(0, 150),
        min = 0,
        max = 150
      ),
      sliderInput(
        "timeinterval",
        label = "interval (ms)",
        value = 200,
        step = 50,
        min = 50,
        max = 1000
      ),
      sliderInput(
        "amplitude",
        label = "amplitude (dB)",
        value = c(0, 50),
        min = -50,
        max = 100
      ),
      selectInput(
        "aspect",
        label = "aspect ratio",
        choices = c("1/4" = 0.25, "1/2" = 0.5, "1" = 1, "2" = 2),
        selected = 1
      ),
      selectInput(
        "contour_species",
        label = "species",
        choices = c(choose = "")
      ),
      selectInput(
        "contour_activity",
        label = "activity",
        choices = c(choose = "")
      ),
      numericInput("contour_animal", label = "animal", value = NA),
      actionButton("next_bad", label = "bad"),
      actionButton("next_good", label = "good"),
      actionButton("save", label = "save labels"),
      actionButton("other", label = "next recording")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Sonogram",
          plotOutput("sonogram", height = "850px", click = "plot_click")
        ),
        tabPanel(
          "Species list",
          tableOutput("species_list"),
          actionButton("new_species", "New species")
        ),
        tabPanel(
          "Activity list",
          tableOutput("activity_list"),
          actionButton("new_activity", "New activity")
        )
      )
    )
  )
))
