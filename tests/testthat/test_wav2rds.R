context("wav2rds")
test_that("wav2rds works", {
  tmp <- tempdir()
  system.file("demo_wav", package = "rhinolophus") %>%
    list.files(full.names = TRUE) %>%
    file.copy(tmp)
  expect_is(
    wav2rds(tmp),
    "character"
  )
})
