#' The batPulseParameter class
#'
#' Holds the relevant content of wav files
#' @section Slots:
#'   \describe{
#'    \item{\code{Recording}}{The metadata on the recording}
#'    \item{\code{SpecGram}}{The actual spectrogram}
#'   }
#' @name batPulseParameter-class
#' @rdname batPulseParameter-class
#' @exportClass batPulseParameter
#' @aliases batPulseParameter-class
#' @importFrom methods setClass
#' @docType class
#' @include batContourMeta-class.R
setClass(
  "batPulseParameter",
  representation = representation(
    Parameter = "data.frame"
  ),
  contains = "batContourMeta"
)

#' @importFrom methods setValidity
setValidity(
  "batPulseParameter",
  function(object){
    return(TRUE)
  }
)
