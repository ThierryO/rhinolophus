library(rhinolophus)
library(shiny)
library(shinyFiles)
library(dplyr)
shinyServer(function(input, output, session) {
  data <- reactiveValues(
    connection = character(0),
    contour = integer(0),
    specieslist = data.frame(
      id = integer(0),
      parent = integer(0),
      description = character(0),
      abbrevation = character(0)
    ),
    activitylist = data.frame(
      id = integer(0),
      description = character(0)
    )
  )

  roots <- c(home = "~", root = "/")
  shinyDirChoose(
    input,
    "path",
    roots = roots,
    filetypes = "sqlite"
  )

  observeEvent(
    input$path,
    {
      if (length(data$connection) > 0) {
        dbDisconnect(data$connection)
      }
      data$connection <- parseDirPath(roots, input$path) %>%
        connect_db()
      data$specieslist <- get_species_list(data$connection)
      output$species_list <- renderTable(data$specieslist)
      updateSelectInput(
        session,
        "contour_species",
        choices = c(choose = "", data$specieslist$abbreviation)
      )
      updateSelectInput(
        session,
        "contour_activity",
        choices = c(choose = "", data$activitylist$description)
      )
    }
  )

  observeEvent(
    input$step_backward,
    updateSliderInput(
      session,
      "starttime",
      value = input$starttime - input$timeinterval
    )
  )

  observeEvent(
    input$step_forward,
    updateSliderInput(
      session,
      "starttime",
      value = input$starttime + input$timeinterval
    )
  )

  filename <- reactive({
    if (length(data$connection) == 0) {
      return(NULL)
    }
    check <- label_unsupervised(data$connection)
    if (file_test("-f", check$filename)) {
      return(
        list(
          id = check$recording,
          wav = check$filename,
          t_e_factor = check$t_e_factor,
          channel = ifelse(check$left_channel == 1, "left", "right")
        )
      )
    } else {
      return(
        list(
          id = check$recording,
          wav = character(0),
          t_e_factor = 1,
          channel = "left"
        )
      )
    }
  })

  sonor <- reactive({
    if (length(filename()$wav) == 0) {
      return(NULL)
    }
    removeTmpFiles(h = 1/60)
    sonogram <- read_wav(
      filename()$wav,
      channel = filename()$channel,
      te.factor = filename()$t_e_factor
    ) %>%
      wav2spectrogram()
    sonogram@SpecGram$f <- sonogram@SpecGram$f / 1000
    sonogram@SpecGram$t <- sonogram@SpecGram$t * 1000
    updateSliderInput(
      session,
      "starttime",
      value = 0,
      max = input$timeinterval * (max(sonogram@SpecGram$t) %/% input$timeinterval),
      step = input$timeinterval
    )
    amplitude_range <- pretty(range(sonogram@SpecGram$S), 10)
    updateSliderInput(
      session,
      "amplitude",
      value = c(0, max(amplitude_range)),
      min = min(amplitude_range),
      max = max(amplitude_range)
    )
    frequency_range <- pretty(range(sonogram@SpecGram$f), 10)
    updateSliderInput(
      session,
      "frequency",
      value = c(0, pmin(140, max(frequency_range))),
      min = min(frequency_range),
      max = max(frequency_range)
    )
    raster(
      sonogram@SpecGram$S[rev(seq_along(sonogram@SpecGram$f)), ],
      xmn = min(sonogram@SpecGram$t),
      xmx = max(sonogram@SpecGram$t),
      ymn = min(sonogram@SpecGram$f),
      ymx = max(sonogram@SpecGram$f)
    )
  })

  contours <- reactive({
    if (is.null(filename())) {
      return(NULL)
    }
    cont <- db2sp(connection = data$connection, recording = filename()$id)
    data$contour <- cont$contour[order(cont$peak_time)[1]]
    cont
  })


  output$sonogram <- renderPlot({
    if (is.null(sonor())) {
      return(NULL)
    }
    breaks <- pretty(input$amplitude[1]:input$amplitude[2], 20)
    plot(
      clamp(sonor(), lower = input$amplitude[1], upper = input$amplitude[2]),
      asp = input$aspect,
      breaks = breaks,
      col = topo.colors(length(breaks)),
      xlim = input$starttime + c(0, input$timeinterval),
      ylim = input$frequency,
      xlab = "time (ms)",
      ylab = "frequency (kHz)"
    )
    abline(
      h = c(20, 30, 40, 50, 60, 80, 90, 110),
      lty = 2,
      col = "white",
      lwd = 2
    )
    abline(h = c(18, 21, 27, 35), lty = 3, col = "white", lwd = 2)
    plot(contours(), lwd = 2, border = contours()$class, add = TRUE)
    points(
      contours()$peak_time,
      contours()$peak_frequency,
      col = contours()$class,
      pch = 13,
      cex = 2
    )
  })

  observeEvent(
    input$plot_click,
    {
      data$contour <- contours()$contour[
        which.min(
          (contours()$peak_time - input$plot_click$x) ^ 2 +
          (contours()$peak_frequency - input$plot_click$y) ^ 2
        )
      ]
      # updateSliderInput(
      #   session,
      #   "starttime",
      #   value = pmax(input$plot_click$x - input$timeinterval / 2, 0)
      # )
    }
  )

  speciesModal <- function(name = FALSE, abbr = FALSE) {
    if (length(data$connection) == 0) {
      return(modalDialog("Please connect to a database first"))
    }
    modalDialog(
      textInput("new_species_name", "Species name"),
      textInput("new_species_abbr", "Species abbrevitation"),
      selectInput(
        "new_species_parent",
        "Parent",
        choices = c("No parent" = NA, data$specieslist$description)
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("new_species_ok", "OK")
      )
    )
  }

  observeEvent(
    input$new_species,
    showModal(speciesModal())
  )

  observeEvent(
    input$new_species_ok,
    {
      dbWriteTable(
        data$connection,
        "species",
        data.frame(
          id = ifelse(
            nrow(data$specieslist) == 0,
            1,
            max(data$specieslist$id) + 1
          ),
          parent = ifelse(
            is.na(input$new_species_parent),
            NA,
            data$specieslist$id[
              data$specieslist$description == input$new_species_parent
            ]
          ),
          description = input$new_species_name,
          abbreviation = input$new_species_abbr,
          stringsAsFactors = FALSE
        ),
        append = TRUE
      )
      removeModal()
      data$specieslist <- get_species_list(data$connection)
      output$species_list <- renderTable(data$specieslist)
      updateSelectInput(
        session,
        "contour_species",
        choices = c(choose = "", data$specieslist$abbreviation)
      )
    }
  )

  activityModal <- function(name = FALSE) {
    if (length(data$connection) == 0) {
      return(modalDialog("Please connect to a database first"))
    }
    modalDialog(
      textInput("new_activity_name", "Activity name"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("new_activity_ok", "OK")
      )
    )
  }

  observeEvent(
    input$new_activity,
    showModal(activityModal())
  )

  observeEvent(
    input$new_activity_ok,
    {
      dbWriteTable(
        data$connection,
        "activity",
        data.frame(
          id = ifelse(
            nrow(data$activitylist) == 0,
            1,
            max(data$activitylist$id) + 1
          ),
          description = input$new_activity_name,
          stringsAsFactors = FALSE
        ),
        append = TRUE
      )
      removeModal()
      data$activitylist <- get_activity_list(data$connection)
      output$activity_list <- renderTable(data$activitylist)
      updateSelectInput(
        session,
        "contour_activity",
        choices = c(choose = "", data$activititylist$description)
      )
    }
  )

  reactive(on.exit(dbDisconnect(data$connection)))
})
