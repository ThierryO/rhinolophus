context("rda2pattern")
target.path <- tempdir()
source.path <- system.file("demo_wav", package = "rhinolophus")
to.copy <- list.files(source.path, pattern = "\\.wav$", full.names = TRUE)
file.copy(from = to.copy, to = target.path)
wav2rda(target.path, te.factor = 10, channel = "left")
n.part <- 10
patterns <- rda2pattern(target.path, n.part = n.part)
expect_is(patterns, "data.frame")

this.colnames <- c(
  "Filename", "Pulse", "StartTime", "PulsDuration", "TimeDelta", "FrequencyMin",
  "FrequencyMax", "FrequencyDelta", "Ratio", "AmplitudeMin", "AmplitudeMax",
  sprintf(
    "fft.%02i.%02i",
    rep(seq_len(n.part), each = n.part),
    rep(seq_len(n.part), n.part)
  )
)
expect_identical(sort(colnames(patterns)), sort(this.colnames))

file.remove(
  list.files(target.path, pattern = "\\.(wav|rda)$", full.names = TRUE)
)
