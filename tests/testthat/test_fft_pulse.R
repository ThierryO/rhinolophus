context("fft_pulse")
wav <- read_wav(
  system.file("demo_wav/leislers.wav", package = "rhinolophus")
)
spectrogram <- wav2spectrogram(wav)
pulses <- find_pulses(spectrogram = spectrogram)
n.fourier <- 20L
pulse.fft <- fft_pulse(
  pulses = pulses,
  spectrogram = spectrogram,
  n.fourier = n.fourier
)

expect_is(pulse.fft, "list")
expect_is(pulse.fft[[1]], "list")
expect_identical(names(pulse.fft[[1]]), c("parameters", "fft.dim", "fft"))

expect_is(pulse.fft[[1]]$parameters, "numeric")
expect_identical(
  names(pulse.fft[[1]]$parameters),
  c(
    "Pulse", "StartTime", "PulsDuration", "TimeDelta", "FrequencyMin",
    "FrequencyMax", "FrequencyDelta", "Ratio", "AmplitudeMin", "AmplitudeMax"
  )
)

expect_is(pulse.fft[[1]]$fft.dim, "integer")
expect_identical(length(pulse.fft[[1]]$fft.dim), 2L)

expect_is(pulse.fft[[1]]$fft, "matrix")
expect_identical(dim(pulse.fft[[1]]$fft), rep(n.fourier, 2))
expect_is(pulse.fft[[1]]$fft[1, 1], "complex")
