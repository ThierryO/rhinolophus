#' Store sounds from the spectrogram as png
#' @param sp a batSpectgram object
#' @param min.amp the minimal amplitude of the strongest part of the sound in dB
#' @param soundbite the maximal length of the sound in seconds
#' @export
#' @importFrom assertthat assert_that is.number
#' @importFrom zoo rollmean
#' @importFrom raster raster extend resample as.matrix
#' @importFrom png writePNG
#' @importFrom dplyr %>%
sound_png <- function(sp, min.amp = 30, soundbite = 10e-3, verbose = TRUE) {
  assert_that(inherits(sp, "batSpectrogram"))
  assert_that(is.number(min.amp))
  assert_that(is.number(soundbite))
  assert_that(soundbite > 0)

  if (verbose) {
    message(sp@Recording$Filename)
  }
  metafile <- sprintf(
    "%s/%s.rds",
    dirname(sp@Recording$Filename),
    sp@Spectrogram$Fingerprint
  )
  if (file.exists(metafile)) {
    return(invisible(NULL))
  }

  relevant <- sum(sp@SpecGram$f < 10e3):(sum(sp@SpecGram$f <= 120e3) + 1)
  sp@SpecGram$S <- sp@SpecGram$S[relevant, ]
  sp@SpecGram$f <- sp@SpecGram$f[relevant]
  delta <- ceiling(2 + soundbite / diff(sp@SpecGram$t[2:3]))
  delta2 <- floor(delta / 2)
  max_amp <- apply(sp@SpecGram$S, 2, max)
  n <- length(max_amp)
  max_amp[seq_len(delta)] <- 0
  max_amp[seq(n, by = -1, length = delta)] <- 0
  rmean <- rollmean(colSums(sp@SpecGram$S), k = delta, na.pad = TRUE)
  rev_id <- rev(seq_len(length(sp@SpecGram$f)))
  xd <- diff(sp@SpecGram$t[1:2]) * 1000
  ymn <- min(sp@SpecGram$f) / 1000
  ymx <- max(sp@SpecGram$f) / 1000
  target <- extent(c(-soundbite / 2e-3, soundbite / 2e-3, 10, 120)) %>%
    raster(ncols = 50, nrows = 110)
  sp@SpecGram$S[sp@SpecGram$S < 0] <- 0

  location <- which.max(max_amp)
  while (max_amp[location] > min.amp) {
    center <- which.max(rmean[-delta2:delta2 + location])
    z <- (-delta2:delta2 + location)[center]
    set <- raster(
      sp@SpecGram$S[rev_id, -delta2:delta2 + z],
      xmn = -xd * delta2, xmx = xd * delta2, ymn = ymn, ymx = ymx
    ) %>%
      raster::resample(target) %>%
      raster::as.matrix()
    file <- sprintf(
      "%s/%s_%05.0f_%02.0f.png",
      dirname(sp@Recording$Filename),
      sp@Spectrogram$Fingerprint,
      round(sp@SpecGram$t[location], 3) * 1e3,
      soundbite * 1e3
    )
    writePNG(
      set / max(set),
      target = file,
      text = c(
        spectrogram = sp@Spectrogram$Fingerprint,
        location = sprintf("%0.3f", sp@SpecGram$t[location]),
        soundbite = sprintf("%0.3f", soundbite)
      )
    )
    done <- seq(
      max(1, z - delta),
      min(n, z + delta)
    )
    max_amp[done] <- 0
    location <- which.max(max_amp)
  }

  saveRDS(
    new(
      "batSpectrogramMeta",
      Spectrogram = sp@Spectrogram,
      Recording = sp@Recording
    ),
    file = metafile
  )
  return(invisible(NULL))
}
