#' Find the location of possible pulses in a spectrogram
#' @export
#' @importFrom raster raster Which rowColFromCell zonal clump subs
#' @importFrom assertthat assert_that is.number
#' @importFrom dplyr %>% filter_ rename_ mutate_ summarize_ select_ inner_join
#' @param spectrogram The spectrogram
#' @param min.contour The minimum amplitude of the pulse
#' @param min.peak Take only contour into account with a maximum amplitude of at least min.peak
#' @examples
#'  wav <- read_wav(
#'    system.file("demo_wav/leislers.wav", package = "rhinolophus")
#'  )
#'  spectrogram <- wav2spectrogram(wav)
#'  find_pulses(spectrogram)
find_pulses <- function(spectrogram, min.contour = 10, min.peak = 20){
  assert_that(is.number(min.contour))
  assert_that(is.number(min.peak))
  assert_that(min.peak > min.contour)
  assert_that(inherits(spectrogram, "specgram"))

  spectrogram.raster <- raster(
    spectrogram$S[rev(seq_len(nrow(spectrogram$S))), ],
    xmn = min(spectrogram$t) * 1000,
    xmx = max(spectrogram$t) * 1000,
    ymn = min(spectrogram$f) / 1000,
    ymx = max(spectrogram$f) / 1000
  )
  names(spectrogram.raster) <- "dB"

  minimum.contour <- clump(spectrogram.raster >= min.contour, directions = 4)
  local.max <- zonal(spectrogram.raster, minimum.contour, fun = max)
  recode <- local.max %>%
    as.data.frame() %>%
    filter_(~ value >= min.peak) %>%
    rename_(AmplitudeMax = ~value) %>%
    mutate_(Pulse = ~seq_along(zone))
  selected.pulses <- subs(minimum.contour, recode, by = "zone", which = "Pulse")
  pulses <- do.call(
    rbind,
    lapply(
      recode$Pulse,
      function(i){
        xy <- rowColFromCell(
          selected.pulses,
          Which(selected.pulses == i, cells = TRUE)
        )
        xy %>%
          as.data.frame() %>%
          summarize_(
            Pulse = i,
            Xmin = ~min(col),
            Xmax = ~max(col),
            Ymin = ~min(row),
            Ymax = ~max(row),
            Ratio = ~n() / (diff(range(col)) * diff(range(row))),
            AmplitudeMin = min.contour
          )
      }
    )
  )
  pulses <- recode %>%
    select_(~-zone) %>%
    inner_join(x = pulses, by = "Pulse")
  return(pulses)
}
