#' Store all information from individual rds files into a database
#' @param path the path to the rds files
#' @param db_path the path to the database
#' @export
#' @importFrom dplyr %>%
#' @importFrom RSQLite dbDisconnect
rds2db <- function(path, db_path = path){
  connection <- connect_db(db_path)
  list.files(path, pattern = "rds$", recursive = TRUE, full.names = TRUE) %>%
    sapply(
      function(file, connection){
        message(file)
        readRDS(file) %>%
          batpulse2db(connection)
      },
      connection = connection
    )
  dbDisconnect(connection)
  return(invisible(NULL))
}
