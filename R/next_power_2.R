#' Calculate the next power of 2
#' @param x the number of which to calculate the next power of 2
#' @export
#' @examples
#' next_power_2(3)
#' next_power_2(12345)
#' next_power_2(c(7, 9))
next_power_2 <- function(x){
  if (!inherits(x, c("numeric", "integer"))) {
    stop("x is not numeric")
  }
  if (any(x <= 1)) {
    stop("x must be > 1")
  }
  2 ^ ceiling(log2(x))
}
