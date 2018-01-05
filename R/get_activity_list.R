#' get the acitivity list from the database
#' @inherit db2sp
#' @export
#' @importFrom RSQLite dbGetQuery
get_activity_list <- function(connection) {
  dbGetQuery(
    conn = connection, "
    SELECT
      id, description
    FROM
      activity"
  )
}
