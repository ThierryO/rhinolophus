#' Prepare the plot a single pattern
#' @export
#' @param pattern A single pattern
prepareplot <- function(pattern){
  filename <- levels(pattern$filename)[pattern$filename]
  load(gsub("\\.WAV$", ".rda", filename))
  wav <- read.wav(
    filename = filename,
    time.expansion.factor = time.expansion.factor,
    channel = channel
  )
  spectrogram <- wav.2.spectrogram(wav = wav, window.ms = window.ms)
  spectrogram$S[spectrogram$S < 1e-10] <- 1e-10
  list(
    spectrogram = spectrogram,
    time = c(0, (pattern$start.time + c(0, pattern$puls.duration)) * 1e-3, tail(spectrogram$t, 1)),
    frequency = c(0, c(pattern$frequency.min, pattern$frequency.max) * 1e3, tail(spectrogram$f, 1)),
    amplitude = c(0, pattern$amplitude.min, pattern$amplitude.max, max(spectrogram$S))
  )
}