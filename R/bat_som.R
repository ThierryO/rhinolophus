#' Calculate a self organizing map on bat pulse features
#' @param x the output of \code{db2ml()}
#' @param dims dimensions of the grid
#' @param topo hexagonal or rectangular topology
#' @export
#' @importFrom kohonen som somgrid
bat_som <- function(
  x,
  dims = rep(floor(log(nrow(x))), 2),
  topo = c("hexagonal", "rectangular")
) {
  topo <- match.arg(topo)
  x_som <- som(
    as.matrix(x[, colnames(x) != "pulse"]),
    grid = somgrid(xdim = dims[1], ydim = dims[2], topo = topo)
  )
  attr(x_som$codes[[1]], "maxL1") <- attr(x, "maxL1")
  attr(x_som$codes[[1]], "peak_frequency") <- attr(x, "peak_frequency")
  attr(x_som$codes[[1]], "peak_amplitude") <- attr(x, "peak_amplitude")
  attr(x_som$codes[[1]], "d_frequency") <- attr(x, "d_frequency")
  attr(x_som$codes[[1]], "d_time") <- attr(x, "d_time")
  return(x_som)
}
