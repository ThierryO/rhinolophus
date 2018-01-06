#' Create a new unsupervised classification
#' @inheritParams db2ml
#' @inheritParams bat_som
#' @export
#' @importFrom RSQLite dbSendQuery dbClearResult dbWriteTable dbDisconnect
unsupervised <- function(
  path,
  n.harmonic = 10,
  contour.amplitude = -25,
  class = NULL,
  dims = c(4, 4),
  topo = c("rectangular","hexagonal"),
  ...
) {
  x <- db2ml(
    path = path,
    class = class,
    n.harmonic = n.harmonic,
    contour.amplitude = contour.amplitude
  )
  x_som <- bat_som(x, dims = dims, topo = topo, ...)
  connection <- connect_db(path)
  value <- data.frame(
    pulse = as.integer(rownames(x_som$data[[1]])),
    class = x_som$unit.classif,
    distance = x_som$distances
  )
  if (missing(class)) {
    res <- dbSendQuery(connection, "DELETE FROM unsupervised")
    dbClearResult(res)
    dbWriteTable(
      conn = connection,
      name = "unsupervised",
      value = value,
      append = TRUE
    )
  } else {
    value$class <- value$class + 16 * class
    hash <- paste0("unsupervised_", sha1(value))
    dbWriteTable(connection, hash, value)
    res <- dbSendQuery(
      conn = connection,
      sprintf("
        INSERT OR REPLACE INTO
          unsupervised (pulse, class, distance)
        SELECT
          h.pulse, h.class, h.distance
        FROM
          %s AS h
        ",
        hash
      )
    )
    dbClearResult(res)
    dbRemoveTable(connection, hash)
  }
  dbDisconnect(connection)
}
