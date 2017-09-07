#' Extract pulses from a spectrogram
#' @export
#' @importFrom raster raster xres focal cellStats maxValue xyFromCell xmin xmax ymin ymax extent crop mask which.max
#' @importFrom dplyr %>%
#' @importFrom utils tail
#' @importFrom sp coordinates
#' @importFrom rgeos gContains
#' @param spectrogram a spectrogram
#' @param min.peak minimum amplitude of the peak. Defaults to 30 dB.
#' @param contour.step step size of the contour. Defaults to 10 dB.
extract_pulse <- function(spectrogram, min.peak = 30, contour.step = 10){
  spectrogram_raster <- raster(
    spectrogram$S[rev(seq_len(nrow(spectrogram$S))), ],
    xmn = min(spectrogram$t) * 1000,
    xmx = max(spectrogram$t) * 1000,
    ymn = min(spectrogram$f) / 1000,
    ymx = max(spectrogram$f) / 1000
  )
  names(spectrogram_raster) <- "dB"
  pulse_zone <- ceiling(20 / xres(spectrogram_raster))

  local_max <- focal(spectrogram_raster, matrix(1, 3, 3), max)
  candidate <- abs(spectrogram_raster - local_max) < 1e-8 &
    spectrogram_raster >= min.peak
  contours <- vector("list", cellStats(candidate, sum))
  candidate[candidate == 0] <- NA
  candidate <- candidate * spectrogram_raster
  current_max <- maxValue(candidate)
  while (!is.na(maxValue(candidate)) && maxValue(candidate) >= min.peak) {
    # get location of the strongest remaining peak
    peak_location <- xyFromCell(spectrogram_raster, which.max(candidate)) %>%
      as.data.frame()
    # subset the spectrogram to speed things up
    peak_range <- xmin(spectrogram_raster) %>%
      pmax(peak_location$x + c(-20, 20)) %>%
      pmin(xmax(spectrogram_raster))
    peak <- extent(
      peak_range[1],
      peak_range[2],
      ymin(spectrogram_raster),
      ymax(spectrogram_raster)
    ) %>%
      crop(x = spectrogram_raster) %>%
      "-"(current_max)
    # calculate the contours
    current_contours <- seq(
      0,
      by = -contour.step,
      length = 4
    ) %>%
      tail(-1) %>%
      get_contours(x = peak)
    # select only the contours which contain the peak
    coordinates(peak_location) <- ~x + y
    relevant <- gContains(current_contours, peak_location, byid = TRUE)[1, ]
    if (any(relevant)) {
      current_contours <- current_contours[relevant, ]
      current_contours$X <- peak_location$x
      current_contours$Y <- peak_location$y
      current_contours$PeakAmplitude <- current_max
      # store contours
      contours[[which.max(sapply(contours, is.null))]] <- current_contours
      # ignore candidates within current contour
      candidate <- mask(candidate, current_contours, inverse = TRUE)
    } else {
      candidate[candidate == maxValue(candidate)] <- NA
    }
    current_max <- maxValue(candidate)
  }
  contours <- contours[!sapply(contours, is.null)] %>%
    do.call(what = rbind)
}
