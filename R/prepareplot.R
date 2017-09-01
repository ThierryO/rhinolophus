#' Prepare the plot a single pattern
#' @export
#' @param pattern A single pattern
prepareplot <- function(pattern){
  ### Fooling R CMD check
  te.factor <- NULL
  rm(te.factor)
  channel <- NULL
  rm(channel)
  window.ms <- NULL
  rm(window.ms)
  ### Fooling R CMD check

  filename <- levels(pattern$filename)[pattern$filename]
  load(gsub("\\.WAV$", ".rda", filename))
  wav <- read_wav(
    filename = filename,
    te.factor = te.factor,
    channel = channel
  )
  spectrogram <- wav2spectrogram(wav = wav, window.ms = window.ms)
  spectrogram$S[spectrogram$S < 1e-10] <- 1e-10
  list(
    spectrogram = spectrogram,
    time = c(0, (pattern$StartTime + c(0, pattern$PulsDuration)) * 1e-3, tail(spectrogram$t, 1)),
    frequency = c(0, c(pattern$FrequencyMin, pattern$FrequencyMax) * 1e3, tail(spectrogram$f, 1)),
    amplitude = c(0, pattern$AmplitudeMin, pattern$AmplitudeMax, max(spectrogram$S))
  )
}
