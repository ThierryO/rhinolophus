#' calculate the ellipse parameters from a Fourier parameters
#' @param fourier_param a 2x2 matrix with Fourier parameters
#' @return a vector with 3 values: the mayor semi-axis, the minor semi-axis and the rotation (in radians)
#' @export
#' @importFrom stats rnorm
fourier_ellipse <- function(fourier_param) {
  theta <- seq(0, by = 2 * pi / 5, length.out = 5)
  pq <- cbind(sin(theta), cos(theta)) %*% fourier_param

  repeat {
    pq <- pq + rnorm(length(pq), sd = .Machine$double.eps)
    D1 <- cbind(pq[, 1] ^ 2, pq[, 1] * pq[, 2], pq[, 2] ^ 2)
    D2 <- cbind(pq, 1)
    S1 <- crossprod(D1)
    S2 <- crossprod(D1, D2)
    S3 <- crossprod(D2)
    T <- -tcrossprod(solve(S3), S2)
    M <- S1 + S2 %*% T
    M <- rbind(M[3,] / 2, -M[2,], M[1,] / 2)
    evec <- eigen(M)$vec
    if (!is.complex(evec)) {
      break
    }
  }
  cond <- 4 * evec[1,] * evec[3,] - evec[2,]^2
  a1 <- evec[, which(cond > 0)]
  f <- c(a1, T %*% a1)
  names(f) <- letters[1:6]

  A <- matrix(c(2 * f[1], f[2], f[2], 2 * f[3]), nrow = 2, byrow = TRUE)
  b <- matrix(c(-f[4], -f[5]), nrow = 2, byrow = TRUE)
  soln <- solve(A) %*% b

  b2 <- f[2] ^ 2 / 4

  num  <- 2 * (
    f[1] * f[5] ^ 2 / 4 +
    f[3] * f[4] ^ 2 / 4 +
    f[6] * b2 - f[2] * f[4] * f[5] / 4 -
    f[1] * f[3] * f[6]
  )
  den1 <- (b2 - f[1] * f[3])
  den2 <- sqrt((f[1] - f[3]) ^ 2 + 4 * b2)
  den3 <- f[1] + f[3]

  semi.axes <- sqrt(
    c(num / (den1 * (den2 - den3)),  num / (den1 * (-den2 - den3)))
  )
  names(semi.axes) <- c("major", "minor")
  # calculate the angle of rotation
  angle <- atan2(f[2], f[1] - f[3]) / 2
  names(angle) <- "rotation"
  c(semi.axes, angle)
}
