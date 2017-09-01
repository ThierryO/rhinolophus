#' Cluster contours into pulses
#' @export
#' @param contours A SpatialPolygonsDataFrame with contours. E.g. the output of \code{\link{spectrogram_contour}}
#' @param min.peak the minimum height of the highest contour of a peak
#' @importFrom rgeos gContains gWithin
#' @importFrom assertthat assert_that is.number
#' @examples
#' \dontrun{
#' wav <- read_wav(
#'   system.file("demo_wav/leislers.wav", package = "rhinolophus")
#' )
#' spectrogram <- wav2spectrogram(wav)
#' contours <- spectrogram_contour(spectrogram)
#' pulses <- contour2pulse(contours)
#' plot(pulses[[1]], asp = 0.25)
#' axis(1)
#' axis(2)
#' }
contour2pulse <- function(contours, min.peak = 20){
  assert_that(inherits(contours, "SpatialPolygonsDataFrame"))
  assert_that(is.number(min.peak))

  pulses <- list()
  current.max <- which.max(contours$level)
  while (contours$level[current.max] >= min.peak & length(contours) > 1) {
    covers <- gContains(
      contours[-current.max, ],
      contours[current.max, ],
      byid = TRUE
    )
    if (sum(covers) == 0) {
      contours <- contours[-current.max, ]
      current.max <- which.max(contours$level)
      next
    }
    selection.topdown <- which(
      rownames(contours@data) %in% colnames(covers)[covers]
    )
    lowest <- selection.topdown[which.min(contours$level[selection.topdown])]
    covered <- gWithin(contours[-lowest, ], contours[lowest, ], byid = TRUE)
    selection.bottumup <- c(
      lowest,
      which(rownames(contours@data) %in% colnames(covered)[covered])
    )
    pulses <- c(pulses, contours[selection.bottumup, ])
    contours <- contours[-selection.bottumup, ]
    current.max <- which.max(contours$level)
  }
  return(pulses)
}
