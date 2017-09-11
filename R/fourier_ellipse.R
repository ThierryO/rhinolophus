#' calculate the ellipse parameters from a Fourier parameters
#' @param fourier_param a 2x2 matrix with Fourier parameters
#' @return a vector with 3 values: the mayor semi-axis, the minor semi-axis and the rotation (in radians)
#' @export
#' @importFrom dplyr %>%
#' @importFrom RConics conicThrough5Points conicMatrixToEllipse
fourier_ellipse <- function(fourier_param) {
  theta <- seq(0, by = 2 * pi / 5, length.out = 5)
  pq <- cbind(sin(theta), cos(theta)) %*% fourier_param
  ellipse <- apply(cbind(pq, 1), 1, list) %>%
    unlist(recursive = FALSE) %>%
    do.call(what = conicThrough5Points) %>%
    conicMatrixToEllipse()
  c(ellipse$saxes, ellipse$theta)
}
