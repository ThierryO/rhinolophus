#' The batPulse class
#'
#' Holds the relevant content of wav files
#' @section Slots:
#'   \describe{
#'    \item{\code{Plot}}{A SpatialPolygonsDataFrame with the contours}
#'   }
#' @name batPulse-class
#' @rdname batPulse-class
#' @exportClass batPulse
#' @aliases batPulse-class
#' @importFrom methods setClass
#' @importClassesFrom sp SpatialPolygonsDataFrame
#' @docType class
#' @include batContourMeta-class.R
setClass(
  "batPulse",
  representation = representation(
    Plot = "SpatialPolygonsDataFrame"
  ),
  contains = "batContourMeta"
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that is.count is.string is.flag noNA
#' @importFrom digest sha1
setValidity(
  "batPulse",
  function(object){
    return(TRUE)
  }
)
