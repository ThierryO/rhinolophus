#' read a WAV file
#' @param filename The name of the file
#' @param channel Select the left or the right channel
#' @param max.length Maximum length of the recording to use in seconds. If the
#' recording is longer, the last part is ignored.
#' @param te.factor The factor to which the original sound was slowed down prior
#'    to recording
#' @export
#' @importFrom tuneR readWave
#' @importFrom assertthat assert_that is.string is.number
#' @importFrom utils file_test
#' @examples
#'  wav = read_wav(
#'    system.file("demo_wav/leislers.wav", package = "rhinolophus")
#'  )
read_wav <- function(
  filename,
  channel = c("left", "right"),
  max.length = 5,
  te.factor = 10
){
  channel <- match.arg(channel)
  assert_that(is.string(filename))
  assert_that(is.number(te.factor))
  if (!file_test("-f", filename)) {
    stop(filename, " does not exist")
  }


  header <- readWave(filename, header = TRUE)
  raw.data <- readWave(
    filename = filename,
    from = 1,
    to = pmin(header$samples, header$sample.rate * te.factor * max.length),
    units = "samples"
  )
  if (channel == "left") {
    selected.channel <- raw.data@left
  } else {
    selected.channel <- raw.data@right
  }
  if (length(selected.channel) == 0) {
    stop("No data in selected channel")
  }
  bat_wav(
    filename = filename,
    timestamp = file.info(filename)$mtime,
    channel = channel,
    te.factor = te.factor,
    sample.rate = header$sample.rate * te.factor,
    values = selected.channel
  )
}
