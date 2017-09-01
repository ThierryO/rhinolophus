context("find_pulses")
wav <- read_wav(
  system.file("demo_wav/leislers.wav", package = "rhinolophus")
)
spectrogram <- wav2spectrogram(wav)
expect_is(pulses <- find_pulses(spectrogram), "data.frame")
expect_identical(
  colnames(pulses),
  c("Pulse", "Xmin", "Xmax", "Ymin", "Ymax", "Ratio", "AmplitudeMin", "AmplitudeMax")
)
expect_error(
  find_pulses(spectrogram, min.contour = "a"),
  "min\\.contour is not a number \\(a length one numeric vector\\)\\."
)
expect_error(
  find_pulses(spectrogram, min.peak = "a"),
  "min\\.peak is not a number \\(a length one numeric vector\\)\\."
)
expect_error(
  find_pulses(spectrogram, min.contour = 10, min.peak = 0),
  "min\\.peak not greater than min\\.contour"
)
expect_error(
  find_pulses("a"),
  "spectrogram does not inherit from class specgram"
)
