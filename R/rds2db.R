#' Store all information from individual rds files into a database
#' @param path the path to the rds files
#' @param db_path the path to the database
#' @export
#' @importFrom dplyr %>%
#' @importFrom RSQLite dbDisconnect
rds2db <- function(path, db_path = path){
  db_path <- normalizePath(db_path)
  connection <- connect_db(db_path)
  stored <- list.files(
    path,
    pattern = "rds$",
    recursive = TRUE,
    full.names = TRUE
  ) %>%
    sample() %>%
    sapply(
      function(file, connection){
        message(file)
        rds <- try(readRDS(file))
        if (inherits(rds, "try-error")) {
          file.remove(file)
          return("rds error")
        }
        stored <- try(batpulse2db(x = rds, connection = connection))
        if (inherits(stored, "try-error")) {
          return("db error")
        } else {
          return("stored")
        }
      },
      connection = connection
    )
  dbDisconnect(connection)
  return(list(db_path = db_path, stored = stored))
}
