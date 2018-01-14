#' Calculate a self organizing map on bat pulse features
#' @param x the output of \code{\link{db2ml}}
#' @param dims dimensions of the grid
#' @param topo rectangular or hexagonal topology
#' @param ... arguments past to \code{\link[kohonen]{som}}
#' @export
#' @importFrom kohonen som somgrid
#' @importFrom stats prcomp
bat_som <- function(
  x,
  dims = rep(floor(log(nrow(x))), 2),
  topo = c("rectangular", "hexagonal"),
  ...
) {
  topo <- match.arg(topo)
  xm <- as.matrix(x[, colnames(x) != "pulse"])
  rownames(xm) <- x[, "pulse", drop = TRUE]
  pca <- prcomp(xm)
  split <- function(y, z) {
    my <- min(y)
    dy <- diff(range(y)) + 1e-6
    (seq_len(z) - 0.5) * dy / z + my
  }
  center <- expand.grid(
    split(pca$x[, 1], dims[1]),
    split(pca$x[, 2], dims[2])
  )
  center <- cbind(
    as.matrix(center),
    matrix(0, nrow = nrow(center), ncol = ncol(pca$rotation) - 2)
  )
  center <- tcrossprod(center, pca$rotation)
  x_som <- som(
    xm,
    grid = somgrid(xdim = dims[1], ydim = dims[2], topo = topo),
    init = center,
    ...
  )
  attr(x_som$codes[[1]], "maxL1") <- attr(x, "maxL1")
  attr(x_som$codes[[1]], "peak_frequency") <- attr(x, "peak_frequency")
  attr(x_som$codes[[1]], "peak_amplitude") <- attr(x, "peak_amplitude")
  attr(x_som$codes[[1]], "d_frequency") <- attr(x, "d_frequency")
  attr(x_som$codes[[1]], "d_time") <- attr(x, "d_time")
  return(x_som)
}
