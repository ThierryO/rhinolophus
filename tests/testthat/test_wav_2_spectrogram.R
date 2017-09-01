context("wav2spectrogram")
expect_error(wav2spectrogram("a"), "wav is not a list")
expect_error(
  wav2spectrogram(list(values = 1)),
  "wav does not have name sample.rate"
)
expect_error(
  wav2spectrogram(list(sample.rate = 1)),
  "wav does not have name values"
)
expect_error(
  wav2spectrogram(list(sample.rate = "a", values = 1)),
  "wav\\$sample.rate is not a number \\(a length one numeric vector\\)"
)
expect_error(
  wav2spectrogram(list(sample.rate = 1, values = "a")),
  "wav\\$values is not a numeric or integer vector"
)
expect_error(
  wav2spectrogram(list(sample.rate = 1, values = 1), window.ms = "a"),
  "window.ms is not a number \\(a length one numeric vector\\)"
)
expect_error(
  wav2spectrogram(list(sample.rate = 1, values = 1), window.ms = 0),
  "window.ms not greater than 0"
)

expect_is(
  wav2spectrogram(
    wav = read_wav(
      system.file("demo_wav/leislers.wav", package = "rhinolophus")
    )
  ),
  "specgram"
)
expect_error(
  wav2spectrogram(
    wav = read_wav(
      system.file("demo_wav/leislers.wav", package = "rhinolophus"),
      channel = "right"
    )
  ),
  "length\\(wav\\$values\\) not greater than 0"
)
