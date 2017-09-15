context("read_wav")
test_that("read_wav handles execptions", {
  dir <- tempdir()
  expect_error(
    read_wav(filename = dir),
    paste(dir, "does not exist")
  )
  expect_error(
    read_wav(
      filename = system.file("demo_wav/leislers.wav", package = "rhinolophus"),
      channel = "right"
    ),
    "No data in selected channel"
  )
})
test_that("read_wav works as expected", {
  expect_is(
    leislers <- read_wav(
      filename = system.file("demo_wav/leislers.wav", package = "rhinolophus")
    ),
    "batWav"
  )
  expect_identical(leislers@Recording$SampleRate, 441000)
})
