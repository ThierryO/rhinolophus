#' Find the pulses in a bunch of wav files and save them to rda files
#' @export
#' @param path The path of the wav files. The function will reach recursive in
#'    the subdirectories of the path.
#' @param te.factor The time-expansion factor used in the wav files. Defaults
#'    to 10.
#' @param channel Which channel will be read. Defaults to "right".
#' @param window.ms The size of the window for fourier transformation of the
#'    sound in milliseconds. The time-expansion factor is taken into account.
#'    Defaults to 1.
#' @param min.contour The minimum amplitude to mark the start of a pulse.
#'    Expressed in dB above the median amplitude of the wav file.
#' @param min.peak The required maximum amplitude of a pulse. Pulses with a
#'    maximum amplitude below min.peak will be ignored.
#' @param n.fourier The number of parameters of the Fourier Transformation to
#'    store per dimension. Defaults to 30.
#' @importFrom assertthat assert_that is.string
wav2rda <- function(
  path,
  te.factor = 10,
  channel = c("right", "left"),
  window.ms = 2,
  min.contour = 10,
  min.peak = 20,
  n.fourier = 30
){
  assert_that(is.string(path))
  channel <- match.arg(channel)

  filenames <- list.files(
    path = path,
    pattern = "\\.wav$",
    recursive = TRUE,
    ignore.case = TRUE,
    full.names = TRUE
  )
  for (filename in filenames) {
    message(filename)
    wav <- read_wav(
      filename = filename,
      te.factor = te.factor,
      channel = channel
    )
    if (length(wav$values) == 0) {
      warning(filename, " contains no information in the ", channel, " channel")
      next
    }
    spectrogram <- wav2spectrogram(
      wav = wav,
      window.ms = window.ms
    )
    rm(wav)
    pulses <- find_pulses(
      spectrogram = spectrogram,
      min.contour = min.contour,
      min.peak = min.peak
    )
    pulses.fft <- fft_pulse(
      pulses = pulses,
      spectrogram = spectrogram,
      n.fourier = n.fourier
    )
    rm(pulses)
    save(
      pulses.fft, te.factor, channel, window.ms, min.contour, min.peak,
      n.fourier,
      file = gsub("(WAV|wav)$", "rda", filename)
    )
    rm(pulses.fft)
    gc()
  }
}
