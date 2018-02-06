library(shiny)
library(shinyFiles)

shinyUI(fluidPage(

  titlePanel("Train sounds"),

  sidebarLayout(
    sidebarPanel(
      width = 2,
      shinyDirButton(
        "path",
        label = "path",
        title = "path"
      ),
      actionButton("save", label = "save", icon = icon("save")),
      actionButton("new_spectrogram", label = "next", icon = icon("file")),
      selectInput(
        "class",
        label = "class",
        choices = "unlabeled"
      ),
      actionButton("step_backward", label = "<<<"),
      actionButton("step_forward", label = ">>>"),
      textInput("new_class", label = "new class"),
      actionButton("add_class", label = "add class"),
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
      )
    ),

    mainPanel(
      plotOutput("sonogram", height = "850px")
    )
  )
))
