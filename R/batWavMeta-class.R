#' The batWavMeta class
#'
#' It holds metadata on bat recordings
#' @section Slots:
#'   \describe{
#'    \item{\code{Recording}}{A data.frame with ID, Fingerprint, Filename, Timestamp, SampleRate, SampleRate, TEFactor, LeftChannel}
#'   }
#' @name batWavMeta-class
#' @rdname batWavMeta-class
#' @exportClass batWavMeta
#' @aliases batWavMeta-class
#' @importFrom methods setClass
#' @docType class
setClass(
  "batWavMeta",
  representation = representation(
    Recording = "data.frame"
  )
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name
setValidity(
  "batWavMeta",
  function(object){
    assert_that(has_name(object@Recording, "ID"))
    assert_that(has_name(object@Recording, "Fingerprint"))
    assert_that(has_name(object@Recording, "Filename"))
    assert_that(has_name(object@Recording, "Timestamp"))
    assert_that(has_name(object@Recording, "SampleRate"))
    assert_that(has_name(object@Recording, "TEFactor"))
    assert_that(has_name(object@Recording, "LeftChannel"))

    return(TRUE)
  }
)
