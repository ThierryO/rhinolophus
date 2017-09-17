#' Extract pulses and store them in an rds file
#' @export
#' @param path the path of the files
#' @inheritParams read_wav
#' @inheritParams wav2spectrogram
#' @inheritParams extract_pulse
#' @importFrom dplyr %>%
wav2rds <- function(
  path,
  te.factor = 1,
  channel = c("left", "right"),
  window.ms = 1,
  overlap = 0.9,
  min.peak = 30,
  contour.step = 10,
  contour.n = 3
) {
  channel <- match.arg(channel)
  available <- list.files(
    path,
    pattern = "WAV$",
    ignore.case = TRUE,
    recursive = TRUE,
    full.names = TRUE
  )
  for (filename in available) {
    message(filename)
    read_wav(filename, te.factor = te.factor, channel = channel) %>%
      wav2spectrogram(window.ms = window.ms, overlap = overlap) %>%
      extract_pulse(
        min.peak = min.peak,
        contour.step = contour.step,
        contour.n = contour.n
      ) %>%
      saveRDS(file = gsub("\\.(wav|WAV)$", ".rds", filename))
  }
  return(invisible(NULL))
}
