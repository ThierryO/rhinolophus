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
#' @param contour.n the maximum number of contours per pulse. Defaults to 3.
extract_pulse <- function(spectrogram, min.peak = 30, contour.step = 10, contour.n = 3){
  contour.n <- as.integer(contour.n)
  spectrogram_raster <- raster(
    spectrogram@SpecGram$S[rev(seq_len(nrow(spectrogram@SpecGram$S))), ],
    xmn = min(spectrogram@SpecGram$t) * 1000,
    xmx = max(spectrogram@SpecGram$t) * 1000,
    ymn = min(spectrogram@SpecGram$f) / 1000,
    ymx = max(spectrogram@SpecGram$f) / 1000
  )
  names(spectrogram_raster) <- "dB"

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
      -contour.step,
      by = -contour.step,
      length = contour.n
    ) %>%
      get_contours(x = peak)
    # select only the contours which contain the peak
    coordinates(peak_location) <- ~x + y
    relevant <- gContains(current_contours, peak_location, byid = TRUE)[1, ]
    if (any(relevant)) {
      current_contours <- current_contours[relevant, ]
      current_contours$PeakX <- peak_location$x
      current_contours$PeakY <- peak_location$y
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
  contours$ContourStep <- contour.step
  contours$ContourMax <- contour.n
  contours$Spectrogram <- spectrogram@Spectrogram$Fingerprint
  contours$Fingerprint <- sapply(
    seq_along(contours@polygons),
    function(i){
      contours@data %>%
        slice_(~i) %>%
        select_(
          ~Spectrogram, ~PeakX, ~PeakY, ~PeakAmplitude,
          ~ContourAmplitude, ~ContourStep, ~ContourMax
        ) %>%
        mutate_(
          Contour = ~list(contours@polygons[[i]]@Polygons[[1]]@coords)
        ) %>%
        sha1()
    }
  )
  contours$ID <- seq_along(contours$ID)
  pulse.meta <- spectrogram@Spectrogram %>%
    select_(Spectrogram = ~ID, SpecFinger = ~Fingerprint) %>%
    inner_join(
      contours@data,
      by = c("SpecFinger" = "Spectrogram")
    ) %>%
    select_(~-SpecFinger)
  new(
    "batPulse",
    Recording = spectrogram@Recording,
    Spectrogram = spectrogram@Spectrogram,
    Pulse = pulse.meta,
    Contour = contours["ID"]
  )
}
