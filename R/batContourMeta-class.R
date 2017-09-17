#' The batContourMeta class
#'
#' It holds metadata on bat recordings
#' @section Slots:
#'   \describe{
#'    \item{\code{Recording}}{A data.frame with ID, Fingerprint, Filename, Timestamp, SampleRate, SampleRate, TEFactor, LeftChannel}
#'    \item{\code{Spectrogram}}{A data.frame with ID, Fingerprint, WindowMS, WindowN, Overlap, Recording}
#'    \item{\code{Pulse}}{A data.frame with ID, Fingerprint, Spectrogram, PeakX, PeakY, PeakAmplitude}
#'    \item{\code{Contour}}{A data.frame with ID, Fingerprint, Pulse, ContourAmplitude, ContourStep, ContourMax}
#'   }
#' @name batContourMeta-class
#' @rdname batContourMeta-class
#' @exportClass batContourMeta
#' @aliases batContourMeta-class
#' @importFrom methods setClass
#' @docType class
#' @include batPulseMeta-class.R
setClass(
  "batContourMeta",
  representation = representation(
    Contour = "data.frame"
  ),
  contains = "batPulseMeta"
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name
setValidity(
  "batContourMeta",
  function(object){
    assert_that(has_name(object@Contour, "ID"))
    assert_that(has_name(object@Contour, "Fingerprint"))
    assert_that(has_name(object@Contour, "Pulse"))
    assert_that(has_name(object@Contour, "ContourMax"))
    assert_that(has_name(object@Contour, "ContourStep"))
    assert_that(has_name(object@Contour, "ContourAmplitude"))

    return(TRUE)
  }
)
