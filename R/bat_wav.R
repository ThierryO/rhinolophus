#' create a batWav object
#' @inheritParams read_wav
#' @param timestamp the timestamp of the file
#' @param sample.rate the realtime sample rate
#' @param values the recorded values
#' @export
#' @importFrom digest sha1
#' @importFrom methods new
bat_wav <- function(
  filename,
  timestamp,
  channel = c("left", "right"),
  te.factor = 1,
  sample.rate,
  values
) {
  channel <- match.arg(channel)
  fingerprint <- sha1(
    list(
      SampleRate = sample.rate,
      TEFactor = te.factor,
      Values = values
    )
  )
  recording <- data.frame(
    ID = 1,
    Fingerprint = fingerprint,
    Filename = filename,
    Timestamp = timestamp,
    SampleRate = sample.rate,
    TEFactor = te.factor,
    LeftChannel = channel == "left",
    stringsAsFactors = FALSE
  )
  new(
    "batWav",
    Recording = recording,
    Values = values
  )
}
