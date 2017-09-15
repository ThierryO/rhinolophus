#' The batPulseMeta class
#'
#' It holds metadata on bat recordings
#' @section Slots:
#'   \describe{
#'    \item{\code{Recording}}{A data.frame with ID, Fingerprint, Filename, Timestamp, SampleRate, SampleRate, TEFactor, LeftChannel}
#'    \item{\code{Spectrogram}}{A data.frame with ID, Fingerprint, WindowMS, WindowN, Overlap, Recording}
#'    \item{\code{Pulse}}{A data.frame with ID, Fingerprint, WindowMS, WindowN, Overlap, Recording}
#'   }
#' @name batPulseMeta-class
#' @rdname batPulseMeta-class
#' @exportClass batPulseMeta
#' @aliases batPulseMeta-class
#' @importFrom methods setClass
#' @docType class
#' @include batSpectrogramMeta-class.R
setClass(
  "batPulseMeta",
  representation = representation(
    Pulse = "data.frame"
  ),
  contains = "batSpectrogramMeta"
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name
setValidity(
  "batPulseMeta",
  function(object){
    assert_that(has_name(object@Pulse, "ID"))
    assert_that(has_name(object@Pulse, "Fingerprint"))
    assert_that(has_name(object@Pulse, "Spectrogram"))
    assert_that(has_name(object@Pulse, "PeakX"))
    assert_that(has_name(object@Pulse, "PeakY"))
    assert_that(has_name(object@Pulse, "PeakAmplitude"))
    assert_that(has_name(object@Pulse, "ContourAmplitude"))
    assert_that(has_name(object@Pulse, "ContourStep"))
    assert_that(has_name(object@Pulse, "ContourMax"))

    return(TRUE)
  }
)
