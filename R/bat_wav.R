#' create a batWav object
#' @inheritParams read_wav
#' @param sample.rate the realtime sample rate
#' @param values the recorded values
#' @export
#' @importFrom digest sha1
#' @importFrom methods new
bat_wav <- function(
  filename,
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
  new(
    "batWav",
    Filename = filename,
    LeftChannel = channel == "left",
    TEFactor = te.factor,
    SampleRate = sample.rate,
    Values = values,
    Fingerprint = fingerprint
  )
}
