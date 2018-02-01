#' Store sounds from the spectrogram as png
#' @param sp a batSpectgram object
#' @param min.amp the minimal amplitude of the strongest part of the sound in dB
#' @param soundbite the maximal length of the sound in seconds
#' @param path the path to store the png and metadata files
#' @param verbose display the name of the current wav file
#' @export
#' @importFrom assertthat assert_that is.number
#' @importFrom zoo rollsum
#' @importFrom raster raster extend resample as.matrix
#' @importFrom png writePNG
#' @importFrom dplyr %>%
sound_png <- function(
  sp,
  min.amp = 30,
  soundbite = 10e-3,
  path,
  verbose = TRUE
) {
  assert_that(inherits(sp, "batSpectrogram"))
  assert_that(is.number(min.amp))
  assert_that(is.number(soundbite))
  assert_that(soundbite > 0)

  frequency.range <- c(10e3, 120e3)
  if (verbose) {
    message(sp@Recording$Filename)
  }
  if (missing(path)) {
    path <- dirname(sp@Recording$Filename)
  }
  path <- normalizePath(path, mustWork = TRUE)
  if (!dir.exists(paste(path, "unlabeled", sep = "/"))) {
    dir.create(path = paste(path, "unlabeled", sep = "/"), recursive = TRUE)
  }
  metafile <- sprintf(
    "%s/%s.rds",
    path,
    sp@Spectrogram$Fingerprint
  ) %>%
    normalizePath(mustWork = FALSE)
  if (file.exists(metafile)) {
    return(invisible(NULL))
  }

  relevant <- sum(
    sp@SpecGram$f < frequency.range[1]
  ):(
    sum(sp@SpecGram$f <= frequency.range[2]) + 1
  )
  sp@SpecGram$S <- sp@SpecGram$S[relevant, ]
  sp@SpecGram$f <- sp@SpecGram$f[relevant]
  delta <- ceiling(soundbite / diff(sp@SpecGram$t[2:3]))
  delta2 <- ceiling(delta / 2)
  max_amp <- apply(sp@SpecGram$S, 2, max)
  n <- length(max_amp)
  max_amp[seq_len(delta)] <- 0
  max_amp[seq(n, by = -1, length = delta)] <- 0
  rev_id <- rev(seq_len(length(sp@SpecGram$f)))
  x_res <- soundbite / 50
  local_weight <- (delta + 1 - seq_len(delta)) / delta
  local_weight <- c(rev(local_weight), 1, local_weight)

  location <- which.max(max_amp)
  while (max_amp[location] > min.amp) {
    local <- max_amp[-delta:delta + location] * local_weight
    center <- which.max(rollsum(local, k = delta, na.pad = TRUE)) +
      location - delta - 1

    local <- raster(
      sp@SpecGram$S[rev_id, -delta:delta + center],
      xmn = sp@SpecGram$t[center - delta],
      xmx = sp@SpecGram$t[center + delta],
      ymn = sp@SpecGram$f[1],
      ymx = tail(sp@SpecGram$f, 1)
    )
    lmin <- cellStats(local, "min")
    lmax <- cellStats(local, "max")
    local <- (local - lmin) / (lmax - lmin)

    mean(c(local@extent@xmin, local@extent@xmax)) %>%
      "/"(x_res) %>%
      round() %>%
      "*"(x_res) ->
      target_center

    extent(
      target_center - x_res * 25,
      target_center + x_res * 25,
      frequency.range[1],
      frequency.range[2]
    ) %>%
      raster(ncols = 50, nrows = 110) ->
      target
    local %>%
      raster::resample(target) %>%
      raster::as.matrix() ->
      set
    sprintf(
      "%s/unlabeled/%s_%05.0f_%02.0f.png",
      path,
      sp@Spectrogram$Fingerprint,
      target_center * 1e3,
      soundbite * 1e3
    ) %>%
      normalizePath(mustWork = FALSE) ->
      file
    writePNG(
      set,
      target = file,
      text = c(
        spectrogram = sp@Spectrogram$Fingerprint,
        time_peak = sp@SpecGram$t[location],
        time_start = target@extent@xmin,
        time_end = target@extent@xmax,
        frequency_start = frequency.range[1],
        frequency_end = frequency.range[2],
        amplitude_start = lmin,
        amplitude_end = lmax,
        soundbite = soundbite
      )
    )
    done <- seq(
      max(1, center - delta),
      min(n, center + delta)
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
