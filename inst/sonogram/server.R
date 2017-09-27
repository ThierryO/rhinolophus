library(shiny)
library(shinyFiles)
library(dplyr)
library(rhinolophus)
library(raster)

shinyServer(function(input, output, session) {
  data <- reactiveValues(
    n_todo = integer(0),
    filename = character(0),
    species = character(0)
  )

  roots <- c(root = "/")
  shinyDirChoose(
    input,
    "path",
    roots = roots,
    filetypes = c("wav", "WAV")
  )

  observeEvent(
    input$path,
    {
      data$species <- parseDirPath(roots, input$path) %>%
        list.files(
          pattern = ".*µ.*\\.wav",
          ignore.case = TRUE,
          recursive = TRUE
        ) %>%
        basename() %>%
        gsub(pattern = "µ.*$", replacement = "") %>%
        strsplit("_") %>%
        unlist() %>%
        gsub(pattern = "-[[:digit:]]*", replacement = "") %>%
        c(data$species) %>%
        unique() %>%
        sort()
      updateSelectInput(session, "aspect", selected = 1)
      updateCheckboxInput(session, "check", value = FALSE)
      todo <- list.files(
        path = parseDirPath(roots, input$path),
        pattern = "\\.wav$",
        ignore.case = TRUE,
        full.names = TRUE
      )
      data$n_todo <- length(todo)
      if (data$n_todo > 0) {
        data$filename <- sample(todo, 1)
      } else {
        data$filename <- character(0)
      }
    }
  )

  sonor <- reactive({
    if (length(data$filename) == 0) {
      return(NULL)
    }
    removeTmpFiles(h = 1/60)
    sonogram <- read_wav(
      data$filename,
      channel = gsub(",.*$", "", input$channel),
      te.factor = as.integer(gsub("^.*, TE = ", "", input$channel))
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
      ylab = "frequency (kHz)",
      main = sprintf("%s\nremaining: %i files", data$filename, data$n_todo - 1)
    )
    abline(
      h = c(20, 30, 40, 50, 60, 80, 90, 110),
      lty = 2,
      col = "white",
      lwd = 2
    )
    abline(h = c(18, 21, 27, 35), lty = 3, col = "white", lwd = 2)
  })

  observeEvent(
    input$aspect,
    {
      updateSliderInput(session, "timeinterval", value = 200 * as.numeric(input$aspect))
    }
  )

  observeEvent(
    input$add_species,
    {
      data$species <- sort(unique(c(data$species, input$new_species)))
    }
  )

  output$species <- renderUI({
    lapply(
      data$species,
      function(species_name){
        numericInput(
          species_name,
          label = species_name,
          value = 0,
          min = 0,
          max = 9,
          step = 1,
        )
      }
    ) %>%
      tagList()
  })

  observeEvent(
    input$move_file,
    {
      if (length(data$species) == 0) {
        return(NULL)
      }
      species_names <- sapply(
        data$species,
        function(species_name){
          if (input[[species_name]] > 0) {
            updateNumericInput(session, species_name, value = 0)
            sprintf("%s-%i", species_name, input[[species_name]])
          } else {
            ""
          }
        }
      )
      species_names <- paste(species_names[species_names != ""], collapse = "_")
      if (nchar(species_names) == 0) {
        return(NULL)
      }
      if (input$check) {
        subdir <- sprintf("%s/check", dirname(data$filename))
      } else {
        subdir <- sprintf("%s/done", dirname(data$filename))
      }
      if (!file_test("-d", subdir)) {
        dir.create(subdir)
      }
      file.rename(
        data$filename,
        sprintf(
          "%s/%sµ%s%iµ%s",
          subdir,
          species_names,
          gsub(",.*$", "", input$channel) %>%
            strtrim(1),
          as.integer(gsub("^.*, TE = ", "", input$channel)),
          basename(data$filename)
        )
      )
      updateSelectInput(session, "aspect", selected = 1)
      todo <- list.files(
        path = parseDirPath(roots, input$path),
        pattern = "\\.wav$",
        ignore.case = TRUE,
        full.names = TRUE
      )
      data$n_todo <- length(todo)
      if (data$n_todo > 0) {
        data$filename <- sample(todo, 1)
      } else {
        data$filename <- character(0)
      }
      updateCheckboxInput(session, "check", value = FALSE)
    }
  )
})
