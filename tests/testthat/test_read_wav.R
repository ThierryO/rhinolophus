context("read_wav")
dir <- tempdir()
expect_error(
  read_wav(filename = dir),
  paste(dir, "does not exist")
)
expect_is(
  leislers <- read_wav(
    filename = system.file("demo_wav/leislers.wav", package = "rhinolophus")
  ),
  "list"
)
expect_identical(leislers$sample.rate, 441000)
expect_identical(leislers$sample, 5065L)
expect_identical(leislers$sample, length(leislers$values))

expect_is(
  leislers.right <- read_wav(
    filename = system.file("demo_wav/leislers.wav", package = "rhinolophus"),
    channel = "right"
  ),
  "list"
)
expect_identical(length(leislers.right$values), 0L)
