library(shiny)
library(shinyFiles)
library(rhinolophus)
library(dplyr)
library(png)
library(raster)
shinyServer(function(input, output, session) {
  roots <- c(home = "~", root = "/")

  data <- reactiveValues(
    path = NULL,
    spectrogram = NULL,
    png = NULL,
    current = NULL,
    sonogram = NULL,
    this_pulse = NULL,
    pulse_lim = NULL,
    clamped = NULL,
    classes = c("unlabeled", "unknown")
  )

  shinyDirChoose(
    input,
    "path",
    roots = roots,
    filetypes = "png"
  )

  observeEvent(
    input$path,
    {
      data$path <- parseDirPath(roots, input$path) %>%
        normalizePath()
    }
  )

  observeEvent(
    data$path,
    {
      if (is.null(data$path)) {
        return(NULL)
      }
      data$spectrogram <- data_frame(
        spectrogram = list.files(
          data$path,
          pattern = "[[:xdigit:]]{40}\\.rds",
          full.names = TRUE
        )
      ) %>%
        mutate(
          fingerprint = basename(spectrogram) %>%
            gsub(pattern = "\\.rds$", replacement = "")
        ) %>%
        rowwise() %>%
        mutate(
          meta = readRDS(spectrogram) %>%
            list()
        )
      data$png <- data_frame(
        png = list.files(
          data$path,
          pattern = "[[:xdigit:]]{40}.*\\.png",
          recursive = TRUE
        )
      ) %>%
        mutate(
          species = gsub(
            "(.*?)/+([[:xdigit:]]{40})_(.*)_(.*)\\.png",
            "\\1",
            .data$png
          ),
          spectrogram = gsub(
            "(.*?)/+([[:xdigit:]]{40})_(.*)_(.*)\\.png",
            "\\2",
            .data$png
          ),
          location = gsub(
            "(.*?)/+([[:xdigit:]]{40})_(.*)_(.*)\\.png",
            "\\3",
            .data$png
          ) %>%
            as.integer(),
          soundbite = gsub(
            "(.*?)/+([[:xdigit:]]{40})_(.*)_(.*)\\.png",
            "\\4",
            .data$png
          ) %>%
            as.integer(),
          updated = FALSE,
          full = paste(data$path, png, sep = "/")
        )
      data$classes <- names(sort(table(data$png$species), decreasing = TRUE))
      data$png %>%
        filter(species == "unlabeled") %>%
        pull(spectrogram) %>%
        sample(1) -> data$current
      data$png %>%
        filter(
          spectrogram == data$current,
          species == "unlabeled"
        ) %>%
        arrange(location) %>%
        slice(1) %>%
        pull("full") ->
        data$this_pulse
    }
  )

  observeEvent(
    input$new_spectrogram,
    {
      data$png %>%
        filter(species == "unlabeled") %>%
        pull(spectrogram) %>%
        sample(1) ->
        data$current
      data$png %>%
        filter(
          spectrogram == data$current,
          species == "unlabeled"
        ) %>%
        arrange(location) %>%
        slice(1) %>%
        pull("full") ->
        data$this_pulse
    }
  )

  observeEvent(
    data$classes,
    {
      updateSelectInput(
        session = session,
        inputId = "class",
        choices = data$classes
      )
    }
  )

  observeEvent(
    data$this_pulse,
    {
      if (is.null(data$this_pulse)) {
        return(NULL)
      }
      meta <- data$this_pulse %>%
        readPNG(info = TRUE) %>%
        attr("info") %>%
        "[["("text")
      updateSliderInput(
        session,
        "starttime",
        value = 1e3 * mean(as.numeric(meta[c("time_start", "time_end")])) -
          input$timeinterval / 2
      )
      updateSelectInput(
        session = session,
        "class",
        selected = data$png %>%
          filter(full == data$this_pulse) %>%
          pull("species")
      )
      data$pulse_lim <- 1e3 * as.numeric(meta[c("time_start", "time_end")])
    }
  )

  observeEvent(
    input$class,
    {
      if (is.null(data$png)) {
        return(NULL)
      }
      data$png$species[data$png$full == data$this_pulse] <- input$class
      data$png$updated[data$png$full == data$this_pulse] <- TRUE
    }
  )

  observeEvent(
    input$step_backward,
    {
      if (is.null(data$this_pulse)) {
        return(NULL)
      }
      i <- which(data$png$full == data$this_pulse) - 1
      if (i >= 1) {
        data$this_pulse <- data$png$full[i]
      }
    }
  )

  observeEvent(
    input$step_forward,
    {
      if (is.null(data$this_pulse)) {
        return(NULL)
      }
      i <- which(data$png$full == data$this_pulse) + 1
      if (i <= nrow(data$png)) {
        data$this_pulse <- data$png$full[i]
      }
    }
  )

  observeEvent(
    data$current,
    {
      if (is.null(data$current)) {
        return(NULL)
      }
      meta <- data$spectrogram %>%
        filter(fingerprint == data$current) %>%
        pull(meta) %>%
        "[["(1)
      read_wav(
        meta@Recording$Filename,
        channel = ifelse(meta@Recording$LeftChannel, "left", "right"),
        te.factor = meta@Recording$TEFactor
      ) %>%
        wav2spectrogram(
          window.ms = meta@Spectrogram$WindowMS,
          overlap = meta@Spectrogram$Overlap
        ) ->
        sonogram
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
      frequency_range <- pretty(range(sonogram@SpecGram$f), 10)
      updateSliderInput(
        session,
        "frequency",
        value = c(0, pmin(140, max(frequency_range))),
        min = min(frequency_range),
        max = max(frequency_range)
      )
      data$sonogram <- raster(
        sonogram@SpecGram$S[rev(seq_along(sonogram@SpecGram$f)), ],
        xmn = min(sonogram@SpecGram$t),
        xmx = max(sonogram@SpecGram$t),
        ymn = min(sonogram@SpecGram$f),
        ymx = max(sonogram@SpecGram$f)
      )
    }
  )

  observeEvent(
    data$sonogram,
    {
      if (is.null(data$sonogram)) {
        return(NULL)
      }
      amplitude_range <- pretty(cellStats(data$sonogram, "range"), 10)
      updateSliderInput(
        session,
        "amplitude",
        value = c(0, max(amplitude_range)),
        min = min(amplitude_range),
        max = max(amplitude_range)
      )
    }
  )

  observeEvent(
    input$amplitude,
    {
      if (is.null(data$sonogram)) {
        data$clamped <- NULL
      } else {
        data$clamped <- clamp(
          data$sonogram,
          lower = input$amplitude[1],
          upper = input$amplitude[2]
        )
      }
    }
  )

  output$sonogram <- renderPlot({
    if (is.null(data$clamped)) {
      return(NULL)
    }
    breaks <- pretty(input$amplitude[1]:input$amplitude[2], 20)
    plot(
      data$clamped,
      breaks = breaks,
      col = topo.colors(length(breaks)),
      xlim = input$starttime + c(0, input$timeinterval),
      ylim = input$frequency,
      xlab = "time (ms)",
      ylab = "frequency (kHz)",
      asp = input$aspect
    )
    abline(
      h = c(20, 30, 40, 50, 60, 80, 90, 110),
      lty = 2,
      col = "white",
      lwd = 2
    )
    abline(h = c(18, 21, 27, 35), lty = 3, col = "white", lwd = 2)
    abline(v = data$pulse_lim, lty = 2, col = "red")
  })

  observeEvent(
    input$add_class,
    {
      if (input$new_class %in% c("", data$classes)) {
        return(NULL)
      }
      data$classes <- c(data$classes, input$new_class)
      sdir <- paste(data$path, input$new_class, sep = "/")
      if (!dir.exists(sdir)) {
        dir.create(sdir)
      }
    }
  )

  observeEvent(
    input$save,
    {
      if (is.null(data$png)) {
        return(NULL)
      }
      data$png %>%
        filter(updated) %>%
        mutate(
          new = sprintf(
            fmt = "%s/%s/%s",
            data$path,
            species,
            basename(png)
          )
        ) -> todo
      file.rename(todo$full, todo$new)
      data$classes <- names(sort(table(data$png$species), decreasing = TRUE))
    }
  )

})
