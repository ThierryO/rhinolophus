context("wav2spectrogram")
leislers <- read_wav(
  filename = system.file("demo_wav/leislers.wav", package = "rhinolophus")
)
test_that("wav2spectrogram handles execptions", {
  expect_error(wav2spectrogram("a"), "wav does not inherit from class batWav")
  expect_error(
    wav2spectrogram(leislers, window.ms = "a"),
    "window.ms is not a number \\(a length one numeric vector\\)"
  )
  expect_error(
    wav2spectrogram(leislers, window.ms = 0),
    "window.ms not greater than 0"
  )
})

test_that("wav2spectrogram works as expected", {
  expect_is(
    wav2spectrogram(leislers),
    "batSpectrogram"
  )
})
