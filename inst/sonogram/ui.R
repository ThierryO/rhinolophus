library(shiny)
library(shinyFiles)

shinyUI(fluidPage(

  titlePanel("Sonogram"),

  sidebarLayout(
    sidebarPanel(
      width = 2,
      shinyDirButton(
        "path",
        label = "select directory",
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
      actionButton("move_file", label = "move file"),
      checkboxInput("check", label = "to check", value = FALSE),
      uiOutput("species"),
      textInput("new_species", label = "new species"),
      actionButton("add_species", label = "add new species"),
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
        "channel",
        label = "channel + TE factor",
        choices = c("left, TE = 1", "right, TE = 10")
      ),
      selectInput(
        "aspect",
        label = "aspect ratio",
        choices = c("1/4" = 0.25, "1/2" = 0.5, "1" = 1, "2" = 2),
        selected = 1
      )
    ),

    mainPanel(
       plotOutput("sonogram", height = "900px")
    )
  )
))
