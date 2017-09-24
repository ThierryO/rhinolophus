context("extract_pulse")
test_that("wav2rds works", {
  expect_is(
    system.file("demo_wav/leislers.wav", package = "rhinolophus") %>%
      read_wav(te.factor = 10) %>%
      wav2spectrogram() %>%
      extract_pulse(),
    "batPulse"
  )
})
