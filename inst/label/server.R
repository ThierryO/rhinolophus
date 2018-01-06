library(rhinolophus)
library(shiny)
library(shinyFiles)
library(dplyr)
shinyServer(function(input, output, session) {
  data <- reactiveValues(
    connection = character(0),
    recording = NULL,
    wav = character(0),
    t_e_factor = 1,
    channel = "left",
    contours = NULL,
    current = integer(0),
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

  observeEvent(
    data$connection,
    {
      if (length(data$connection) == 0) {
        return(NULL)
      }
      data$specieslist <- get_species_list(data$connection)
      output$species_list <- renderTable(data$specieslist)
      tmp <- data$specieslist$id
      names(tmp) <- data$specieslist$abbreviation
      tmp <- tmp[
        order(-data$specieslist$times_used, data$specieslist$abbreviation)
      ]
      updateSelectInput(
        session,
        "contour_species",
        choices = c(choose = "", tmp)
      )
      data$activitylist <- get_activity_list(data$connection)
      output$activity_list <- renderTable(data$activitylist)
      tmp <- data$activitylist$id
      names(tmp) <- data$activitylist$description
      tmp <- tmp[
        order(-data$activitylist$times_used, data$activitylist$description)
      ]
      updateSelectInput(
        session,
        "contour_activity",
        choices = c(choose = "", tmp)
      )
      output$status_unsupervised <- renderTable(
        status_unsupervised(data$connection)
      )
      check <- label_unsupervised(data$connection)
      data$recording = check$recording
      if (file_test("-f", check$filename)) {
        data$wav <- check$filename
        data$t_e_factor <- check$t_e_factor
        data$channel <- ifelse(check$left_channel == 1, "left", "right")
      } else {
        data$wav <- character(0)
        data$t_e_factor <- 1
        data$channel <- "left"
      }
    }
  )

  observeEvent(
    input$other,
    {
      if (length(data$connection) == 0) {
        return(NULL)
      }
      data$specieslist <- get_species_list(data$connection)
      output$species_list <- renderTable(data$specieslist)
      tmp <- data$specieslist$id
      names(tmp) <- data$specieslist$abbreviation
      tmp <- tmp[
        order(-data$specieslist$times_used, data$specieslist$abbreviation)
      ]
      updateSelectInput(
        session,
        "contour_species",
        choices = c(choose = "", tmp)
      )
      data$activitylist <- get_activity_list(data$connection)
      output$activity_list <- renderTable(data$activitylist)
      tmp <- data$activitylist$id
      names(tmp) <- data$activitylist$description
      tmp <- tmp[
        order(-data$activitylist$times_used, data$activitylist$description)
      ]
      updateSelectInput(
        session,
        "contour_activity",
        choices = c(choose = "", tmp)
      )
      output$status_unsupervised <- renderTable(
        status_unsupervised(data$connection)
      )
      check <- label_unsupervised(data$connection)
      data$recording = check$recording
      if (file_test("-f", check$filename)) {
        data$wav <- check$filename
        data$t_e_factor <- check$t_e_factor
        data$channel <- ifelse(check$left_channel == 1, "left", "right")
      } else {
        data$wav <- character(0)
        data$t_e_factor <- 1
        data$channel <- "left"
      }
    }
  )

  sonor <- reactive({
    if (length(data$wav) == 0) {
      return(NULL)
    }
    removeTmpFiles(h = 1/60)
    sonogram <- read_wav(
      data$wav,
      channel = data$channel,
      te.factor = data$t_e_factor
    ) %>%
      wav2spectrogram()
    sonogram@SpecGram$f <- sonogram@SpecGram$f / 1000
    sonogram@SpecGram$t <- sonogram@SpecGram$t * 1000
    updateSliderInput(
      session,
      "starttime",
      value = pmax(
        data$contours$peak_time[data$contours$contour == data$current] -
          input$timeinterval / 2,
        0
      ),
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

  observeEvent(
    data$recording,
    {
      if (is.null(data$recording)) {
        return(NULL)
      }
      data$contours <- db2sp(connection = data$connection, recording = data$recording)
      data$current <- data$contours@data %>%
        filter(is.na(species)) %>%
        arrange(peak_time) %>%
        slice(1) %>%
        pull(contour)
    }
  )

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
      main = data$wav,
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
    if (any(!is.na(data$contours$animal))) {
      plot(
        data$contours[!is.na(data$contours$animal), ],
        col = data$contours$animal[!is.na(data$contours$animal)] + 1,
        add = TRUE
      )
    }
    if (any(is.na(data$contours$animal))) {
      plot(
        data$contours[is.na(data$contours$species), ],
        add = TRUE
      )
    }
    points(
      data$contours$peak_time,
      data$contours$peak_frequency,
      pch = 13,
      cex = 2
    )
    if (length(data$current)) {
      plot(
        data$contours[data$contours$contour == data$current, ],
        lwd = 6,
        border = "red",
        add = TRUE
      )
    }
  })

  observeEvent(
    input$plot_click,
    {
      data$current <- data$contours$contour[
        which.min(
          (data$contours$peak_time - input$plot_click$x) ^ 2 +
          (data$contours$peak_frequency - input$plot_click$y) ^ 2
        )
      ]
      updateSelectInput(
        session,
        "contour_species",
        selected = ifelse(
          is.na(data$contours$species[data$contours$contour == data$current]),
          "",
          as.character(
            data$contours$species[data$contours$contour == data$current]
          )
        )
      )
      updateSelectInput(
        session,
        "contour_activity",
        selected = ifelse(
          is.na(data$contours$activity[data$contours$contour == data$current]),
          "",
          as.character(
            data$contours$activity[data$contours$contour == data$current]
          )
        )
      )
      updateNumericInput(
        session,
        "contour_animal",
        value = data$contours$animal[data$contours$contour == data$current]
      )
      updateSliderInput(
        session,
        "starttime",
        value = pmax(
          data$contours$peak_time[data$contours$contour == data$current] -
            input$timeinterval / 2,
          0
        )
      )
    }
  )

  observeEvent(
    input$contour_species,
    {
      if (input$contour_species == "") {
        data$contours$species[data$contours$contour == data$current] <- NA
      } else {
        data$contours$species[data$contours$contour == data$current] <-
          as.integer(input$contour_species)
      }
    }
  )

  observeEvent(
    input$contour_activity,
    {
      if (input$contour_activity == "") {
        data$contours$activity[data$contours$contour == data$current] <- NA
      } else {
        data$contours$activity[data$contours$contour == data$current] <-
          as.integer(input$contour_activity)
      }
    }
  )

  observeEvent(
    input$contour_animal,
    {
      data$contours$animal[data$contours$contour == data$current] <-
        as.integer(input$contour_animal)
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
      store_manual(data$connection, data$contours@data)
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
      tmp <- data$specieslist$id
      names(tmp) <- data$specieslist$abbreviation
      tmp <- tmp[
        order(-data$specieslist$times_used, data$specieslist$abbreviation)
      ]
      updateSelectInput(
        session,
        "contour_species",
        choices = c(choose = "", tmp)
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
      store_manual(data$connection, data$contours@data)
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
      tmp <- data$activitylist$id
      names(tmp) <- data$activitylist$description
      tmp <- tmp[
        order(-data$activitylist$times_used, data$activitylist$description)
      ]
      updateSelectInput(
        session,
        "contour_activity",
        choices = c(choose = "", tmp)
      )
    }
  )

  observeEvent(
    input$save,
    {
      if (!inherits(data$connection, "DBIConnection")) {
        return(NULL)
      }
      store_manual(data$connection, data$contours@data)
      output$status_unsupervised <- renderTable(
        status_unsupervised(data$connection)
      )
    }
  )


  reactive(on.exit(dbDisconnect(data$connection)))
})
