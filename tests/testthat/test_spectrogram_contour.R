context("spectrogram_contour")
wav <- read_wav(
  system.file("demo_wav/leislers.wav", package = "rhinolophus")
)
spectrogram <- wav2spectrogram(wav)
expect_error(
  spectrogram_contour("a"),
  "spectrogram does not inherit from class specgram"
)
expect_error(
  spectrogram_contour(spectrogram, contour.level = "a"),
  "contour\\.level is not a numeric or integer vector"
)
expect_error(
  spectrogram_contour(spectrogram, contour.level = 0),
  "contour\\.level not greater than 0"
)
expect_error(
  spectrogram_contour(spectrogram, contour.level = -1),
  "contour\\.level not greater than 0"
)

expect_is(
  spectrogram_contour(spectrogram),
  "SpatialPolygonsDataFrame"
)
expect_is(
  spectrogram_contour(spectrogram, contour.level = 2),
  "SpatialPolygonsDataFrame"
)
this.contour.level <- c(-2, 2)
contours <- spectrogram_contour(spectrogram, contour.level = this.contour.level)
expect_true(
  all(contours@data$level %in% this.contour.level)
)
