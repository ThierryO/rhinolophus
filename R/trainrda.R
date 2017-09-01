#' Train a Kohonen network
#' @export
#' @import kohonen
#' @importFrom kohonen somgrid
#' @param pattern A matrix with patterns
#' @param truth A data.frame with the truth
trainrda <- function(pattern, truth){
  truth$combinations <- paste(truth$type, truth$species, sep = ":")
  types <- table(truth$combinations)
  truth$combinations[truth$combinations %in% names(types)[types < 10]] <- "other:"
  Y <- model.matrix(~ 0 + combinations, data = truth)
  Y <- Y[, colnames(Y) != "combinationsother:"]
  colnames(Y) <- names(types)[types >= 10]
  n.dim <- floor(log(nrow(truth)))
  list(
    model = xyf(
      pattern,
      Y,
      somgrid(n.dim, n.dim, "hexagonal")
    ),
    weights = 1 / table(truth$combinations)
  )
}
