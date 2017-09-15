context("wav2spectrogram")
expect_error(wav2spectrogram("a"), "wav does not inherit from class batWav")
leislers <- read_wav(
  filename = system.file("demo_wav/leislers.wav", package = "rhinolophus")
)
expect_error(
  wav2spectrogram(leislers, window.ms = "a"),
  "window.ms is not a number \\(a length one numeric vector\\)"
)
expect_error(
  wav2spectrogram(leislers, window.ms = 0),
  "window.ms not greater than 0"
)

expect_is(
  wav2spectrogram(leislers),
  "batSpectrogram"
)
