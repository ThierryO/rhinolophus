#' get the acitivity list from the database
#' @inherit db2sp
#' @export
#' @importFrom RSQLite dbGetQuery
get_activity_list <- function(connection) {
  dbGetQuery(
    conn = connection, "
    SELECT
      a.id, a.description, COUNT(m.contour) AS times_used
    FROM
      activity AS a
    LEFT JOIN
      manual AS m
    ON
      a.id = m.activity
    GROUP BY
      a.id, a.description
    ORDER BY
      times_used DESC, description
    "
  )
}
