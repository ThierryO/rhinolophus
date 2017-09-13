#' The batWav class
#'
#' Holds the relevant content of wav files
#' @section Slots:
#'   \describe{
#'    \item{\code{SampleRate}}{The realtime sample rate (i.e. recording sample rate x time expantion factor)}
#'    \item{\code{TEFactor}}{Time expantion factor}
#'    \item{\code{Value}}{The actual recorded values}
#'    \item{\code{Fingerprint}}{The fingerprint of the object}
#'   }
#' @name batWav-class
#' @rdname batWav-class
#' @exportClass batWav
#' @aliases batWav-class
#' @importFrom methods setClass
#' @docType class
setClass(
  "batWav",
  representation = representation(
    Filename = "character",
    LeftChannel = "logical",
    TEFactor = "integer",
    SampleRate = "integer",
    Values = "integer",
    Fingerprint = "character"
  )
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that is.count is.string is.flag noNA
#' @importFrom digest sha1
setValidity(
  "batWav",
  function(object){
    assert_that(is.count(object@SampleRate))
    assert_that(is.count(object@TEFactor))
    assert_that(is.string(object@Fingerprint))
    assert_that(is.string(object@Filename))
    assert_that(is.flag(object@LeftChannel))
    assert_that(noNA(object@LeftChannel))
    assert_that(is.vector(Values))

    fingerprint <- sha1(
      list(
        SampleRate = object@SampleRate,
        TEFactor = object@TEFactor,
        Values = object@Values
      )
    )
    if (fingerprint != object@Fingerprint) {
      stop("Fingerprint doesn't match")
    }
    return(TRUE)
  }
)
