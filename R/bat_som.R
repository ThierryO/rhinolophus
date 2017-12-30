#' Calculate a self organizing map on bat pulse features
#' @param x the output of \code{\link{db2ml}}
#' @param dims dimensions of the grid
#' @param topo rectangular or hexagonal topology
#' @param ... arguments past to \code{\link[kohonen]{som}}
#' @export
#' @importFrom kohonen som somgrid
bat_som <- function(
  x,
  dims = rep(floor(log(nrow(x))), 2),
  topo = c("rectangular", "hexagonal"),
  ...
) {
  topo <- match.arg(topo)
  xm <- as.matrix(x[, colnames(x) != "pulse"])
  rownames(xm) <- x[, "pulse", drop = TRUE]
  x_som <- som(
    xm,
    grid = somgrid(xdim = dims[1], ydim = dims[2], topo = topo),
    ...
  )
  attr(x_som$codes[[1]], "maxL1") <- attr(x, "maxL1")
  attr(x_som$codes[[1]], "peak_frequency") <- attr(x, "peak_frequency")
  attr(x_som$codes[[1]], "peak_amplitude") <- attr(x, "peak_amplitude")
  attr(x_som$codes[[1]], "d_frequency") <- attr(x, "d_frequency")
  attr(x_som$codes[[1]], "d_time") <- attr(x, "d_time")
  return(x_som)
}
