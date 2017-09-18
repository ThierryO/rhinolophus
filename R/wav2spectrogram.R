#' Convert the output of read_wav to a spectrogram
#' @param wav An object as created by \code{\link{read_wav}}
#' @param window.ms The size of the window in microseconds. Default to 1.
#' @param overlap The overlap of two windows. Defaults to 0.9.
#' @importFrom assertthat assert_that has_name is.number
#' @importFrom signal specgram
#' @importFrom stats median
#' @importFrom methods validObject
#' @export
#' @examples
#'  wav2spectrogram(
#'    wav = read_wav(
#'      system.file("demo_wav/leislers.wav", package = "rhinolophus")
#'    )
#'  )
wav2spectrogram <- function(wav, window.ms = 1, overlap = 0.9){
  assert_that(inherits(wav, "batWav"))
  validObject(wav)
  assert_that(is.number(window.ms))
  assert_that(window.ms > 0)
  assert_that(is.number(overlap))
  assert_that(overlap > 0)
  assert_that(overlap < 1)

  window.n <- next_power_2(wav@Recording$SampleRate * window.ms / 1000)
  spectrogram <- specgram(
    x = wav@Values,
    n = window.n,
    Fs = wav@Recording$SampleRate,
    overlap = ceiling(overlap * window.n)
  )
  spectrogram$S <- 20 * log10(abs(spectrogram$S))
  spectrogram$S <- spectrogram$S - median(spectrogram$S)
  fingerprint <- sha1(
    list(
      Recording = wav@Recording$Fingerprint,
      WindowMS = window.ms,
      WindowN = window.n,
      Overlap = overlap
    )
  )
  spectrogram.meta <- data.frame(
    ID = 1,
    Fingerprint = fingerprint,
    WindowMS = window.ms,
    WindowN = window.n,
    Overlap = overlap,
    Recording = wav@Recording$ID
  )
  new(
    "batSpectrogram",
    Recording = wav@Recording,
    Spectrogram = spectrogram.meta,
    SpecGram = spectrogram
  )
}
