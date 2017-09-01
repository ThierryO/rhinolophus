#' Calculate the fast fourier transformation for each pulse
#' @export
#' @importFrom sp bbox
#' @importFrom assertthat assert_that is.count has_name
#' @param pulses A list of pulses. E.g. the output of \code{\link{find_pulses}}
#' @param spectrogram The spectrogram. E.g. the output of \code{\link{wav2spectrogram}}
#' @param n.fourier The number of required Fourier components in each direction.
#' @examples
#' wav <- read_wav(
#'   system.file("demo_wav/leislers.wav", package = "rhinolophus")
#' )
#' spectrogram <- wav2spectrogram(wav)
#' pulses <- find_pulses(spectrogram = spectrogram)
#' pulse.fft <- fft_pulse(pulses = pulses, spectrogram = spectrogram)
fft_pulse <- function(pulses, spectrogram, n.fourier = 30){
  assert_that(is.data.frame(pulses))
  assert_that(inherits(spectrogram, what = "specgram"))
  assert_that(is.count(n.fourier))
  assert_that(has_name(pulses, "Xmin"))
  assert_that(has_name(pulses, "Xmax"))
  assert_that(has_name(pulses, "Ymin"))
  assert_that(has_name(pulses, "Ymax"))

  apply(
    pulses,
    1,
    function(pulse){
      select.t <- pulse["Xmin"]:pulse["Xmax"]
      select.f <- 1 + length(spectrogram$f) - pulse["Ymax"]:pulse["Ymin"]
      selection <- spectrogram$S[select.f, select.t, drop = FALSE]
      pulse.fft <- fft(selection)
      full <- matrix(0, ncol = n.fourier, nrow = n.fourier)
      select.dim <- pmin(n.fourier, dim(pulse.fft))
      full[seq_len(select.dim[1]), seq_len(select.dim[2])] <- pulse.fft[
        seq_len(select.dim[1]),
        seq_len(select.dim[2])
      ]
      parameters <- c(
        pulse["Pulse"],
        spectrogram$t[pulse["Xmin"]] * 1e3,
        (1 + pulse["Xmax"] - pulse["Xmin"]) * diff(spectrogram$t[1:2]) * 1e3,
        diff(spectrogram$t[1:2]) * 1e3,
        range(spectrogram$f[select.f]) * 1e-3,
        diff(spectrogram$f[1:2]) * 1e-3,
        pulse[c("Ratio", "AmplitudeMin", "AmplitudeMax")]
      )
      names(parameters) <- c(
        "Pulse", "StartTime", "PulsDuration", "TimeDelta", "FrequencyMin",
        "FrequencyMax", "FrequencyDelta", "Ratio", "AmplitudeMin",
        "AmplitudeMax"
      )
      list(
        parameters = parameters,
        fft.dim = dim(selection),
        fft = full
      )
    }
  )
}
