#' read a WAV file
#' @param filename The name of the file
#' @param channel Select the left or the right channel
#' @param te.factor The factor to which the original sound was slowed down prior
#'    to recording
#' @export
#' @importFrom tuneR readWave
#' @importFrom assertthat assert_that is.string is.number
#' @examples
#'  wav = read_wav(
#'    system.file("demo_wav/leislers.wav", package = "rhinolophus")
#'  )
read_wav <- function(
  filename,
  channel = c("left", "right"),
  te.factor = 10
){
  channel <- match.arg(channel)
  assert_that(is.string(filename))
  assert_that(is.number(te.factor))
  if (!file_test("-f", filename)) {
    stop(filename, " does not exist")
  }


  header <- readWave(filename, header = TRUE)
  raw.data <- readWave(filename)
  if (channel == "left") {
    selected.channel <- raw.data@left
  } else {
    selected.channel <- raw.data@right
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
