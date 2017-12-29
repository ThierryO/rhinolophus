#' Unscale the machine learning features
#' @param x the scaled features
#' @export
unscale <- function(x) {
  x$L1 <- x$L1 * attr(x, "maxL1")
  x$d_time <- x$d_time * attr(x, "d_time")
  x$d_frequency <- x$d_frequency * attr(x, "d_frequency")
  x$peak_frequency <- x$peak_frequency * attr(x, "peak_frequency")
  x$peak_amplitude <- x$peak_amplitude * attr(x, "peak_amplitude")
  sincos <- grep("^h[[:digit:]]+(sin|cos)_(frequency|time)", colnames(x))
  x[, sincos] <- x[, sincos] * x$L1
  return(x)
}
