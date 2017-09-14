#' The batSpectrogram class
#'
#' Holds the relevant content of wav files
#' @section Slots:
#'   \describe{
#'    \item{\code{Recording}}{The metadata on the recording}
#'    \item{\code{SpecGram}}{The actual spectrogram}
#'   }
#' @name batSpectrogram-class
#' @rdname batSpectrogram-class
#' @exportClass batSpectrogram
#' @aliases batSpectrogram-class
#' @importFrom methods setClass
#' @docType class
#' @include batMeta-class.R
#' @include import_S3_classes.R
setClass(
  "batSpectrogram",
  representation = representation(
    SpecGram = "specgram"
  ),
  contains = "batMeta"
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that is.count is.string is.flag noNA
#' @importFrom digest sha1
setValidity(
  "batSpectrogram",
  function(object){
    return(TRUE)
  }
)
