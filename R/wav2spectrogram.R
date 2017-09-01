#' Convert the output of read_wav to a spectrogram
#' @param wav An object as created by \code{\link{read_wav}}
#' @param window.ms The size of the window in microseconds. Default to 1.
#' @importFrom assertthat assert_that has_name is.number
#' @importFrom signal specgram
#' @export
#' @examples
#'  wav2spectrogram(
#'    wav = read_wav(
#'      system.file("demo_wav/leislers.wav", package = "rhinolophus")
#'    )
#'  )
wav2spectrogram <- function(wav, window.ms = 1){
  assert_that(is.list(wav))
  assert_that(has_name(wav, "sample.rate"))
  assert_that(has_name(wav, "values"))
  assert_that(is.number(wav$sample.rate))
  assert_that(is.numeric(wav$values))
  assert_that(length(wav$values) > 0)
  assert_that(is.number(window.ms))
  assert_that(window.ms > 0)

  window.n <- next_power_2(wav$sample.rate * window.ms / 1000)
  spectrogram <- specgram(
    x = wav$values,
    n = window.n,
    Fs = wav$sample.rate,
    overlap = ceiling(0.9 * window.n)
  )
  spectrogram$S <- 20 * log10(abs(spectrogram$S))
  spectrogram$S <- spectrogram$S - median(spectrogram$S)
  return(spectrogram)
}
