#' The batWav class
#'
#' Holds the relevant content of wav files
#' @section Slots:
#'   \describe{
#'    \item{\code{Recording}}{The metadata on the recording}
#'    \item{\code{Values}}{The actual recorded values}
#'   }
#' @name batWav-class
#' @rdname batWav-class
#' @exportClass batWav
#' @aliases batWav-class
#' @importFrom methods setClass
#' @docType class
#' @include batWavMeta-class.R
setClass(
  "batWav",
  representation = representation(
    Values = "integer"
  ),
  contains = "batWavMeta"
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that is.count is.string is.flag noNA
#' @importFrom digest sha1
setValidity(
  "batWav",
  function(object){
    assert_that(is.vector(object@Values))
    return(TRUE)
  }
)
