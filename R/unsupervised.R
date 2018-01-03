#' Create a new unsupervised classification
#' @inheritParams db2ml
#' @inheritParams bat_som
#' @export
#' @importFrom RSQLite dbSendQuery dbClearResult dbWriteTable dbDisconnect
unsupervised <- function(
  db.path,
  n.harmonic = 10,
  contour.amplitude = -25,
  dims = c(4, 4),
  topo = c("rectangular","hexagonal"),
  ...
) {
  x <- db2ml(
    db.path = db.path,
    n.harmonic = n.harmonic,
    contour.amplitude = contour.amplitude
  )
  x_som <- bat_som(x, dims = dims, topo = topo, ...)
  connection <- connect_db(path)
  res <- dbSendQuery(connection, "DELETE FROM unsupervised")
  dbClearResult(res)
  dbWriteTable(
    conn = connection,
    name = "unsupervised",
    value = data.frame(
      pulse = as.integer(rownames(x_som$data[[1]])),
      class = x_som$unit.classif,
      distance = x_som$distances
    ),
    append = TRUE
  )
  dbDisconnect(connection)

}
