#' The batSpectrogramMeta class
#'
#' It holds metadata on bat recordings
#' @section Slots:
#'   \describe{
#'    \item{\code{Recording}}{A data.frame with ID, Fingerprint, Filename, Timestamp, SampleRate, SampleRate, TEFactor, LeftChannel}
#'    \item{\code{Spectrogram}}{A data.frame with ID, Fingerprint, WindowMS, WindowN, Overlap, Recording}
#'   }
#' @name batSpectrogramMeta-class
#' @rdname batSpectrogramMeta-class
#' @exportClass batSpectrogramMeta
#' @aliases batSpectrogramMeta-class
#' @importFrom methods setClass
#' @docType class
#' @include batWavMeta-class.R
setClass(
  "batSpectrogramMeta",
  representation = representation(
    Spectrogram = "data.frame"
  ),
  contains = "batWavMeta"
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name
setValidity(
  "batSpectrogramMeta",
  function(object){
    assert_that(has_name(object@Spectrogram, "ID"))
    assert_that(has_name(object@Spectrogram, "Fingerprint"))
    assert_that(has_name(object@Spectrogram, "WindowMS"))
    assert_that(has_name(object@Spectrogram, "WindowN"))
    assert_that(has_name(object@Spectrogram, "Overlap"))
    assert_that(has_name(object@Spectrogram, "Recording"))

    return(TRUE)
  }
)
