context("contour2pulse")
wav <- read_wav(
  system.file("demo_wav/leislers.wav", package = "rhinolophus")
)
spectrogram <- wav2spectrogram(wav)
contours <- spectrogram_contour(spectrogram)
pulses <- contour2pulse(contours)
expect_is(pulses, "list")
expect_error(
  contour2pulse(contours = "abc"),
  "contours does not inherit from class SpatialPolygonsDataFrame"
)
expect_error(
  contour2pulse(contours = contours, min.peak = "abc"),
  "min\\.peak is not a number \\(a length one numeric vector\\)\\."
)
expect_identical(
  length(contour2pulse(contours = contours, min.peak = 1000)),
  0L
)
