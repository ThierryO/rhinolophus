#' Extract pulses and store them in an rds file
#' @export
#' @param path the path of the files
#' @param overwrite overwrite existing rds files?. Default to FALSE
#' @inheritParams read_wav
#' @inheritParams wav2spectrogram
#' @inheritParams extract_pulse
#' @importFrom dplyr %>%
wav2rds <- function(
  path,
  te.factor = 1,
  channel = c("left", "right"),
  max.length = 5,
  window.ms = 1,
  overlap = 0.9,
  min.peak = 30,
  contour.step = 10,
  contour.n = 3,
  overwrite = FALSE
) {
  channel <- match.arg(channel)
  available <- list.files(
    path,
    pattern = "WAV$",
    ignore.case = TRUE,
    recursive = TRUE,
    full.names = TRUE
  )
  sapply(
    sample(available),
    function(filename) {
      message("\n", filename)
      target <- gsub("\\.(wav|WAV)$", ".rds", filename)
      if (file.exists(target) & !overwrite) {
        return("exists")
      }
      test <- try(
        read_wav(
          filename,
          te.factor = te.factor,
          channel = channel,
          max.length = max.length
        ) %>%
          wav2spectrogram(window.ms = window.ms, overlap = overlap) %>%
          extract_pulse(
            min.peak = min.peak,
            contour.step = contour.step,
            contour.n = contour.n
          ) %>%
          pulse_parameter() %>%
          saveRDS(file = target)
      )
      if (inherits(test, "try-error")) {
        rm(test)
        gc()
        return(paste("error in", filename))
      } else {
        rm(test)
        gc()
        return("parameters stored")
      }
    }
  )
}
