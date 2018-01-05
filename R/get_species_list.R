#' get the species list from the database
#' @inherit db2sp
#' @export
#' @importFrom RSQLite dbGetQuery
get_species_list <- function(connection) {
  dbGetQuery(
    conn = connection, "
    WITH cte_parent AS (
        SELECT id, parent, 0 AS level, abbreviation AS rank
        FROM species
        WHERE parent IS NULL
      UNION
        SELECT
          s.id, s.parent, c.level + 1 AS level, c.rank || s.abbreviation AS rank
        FROM species AS s INNER JOIN cte_parent AS c ON s.parent = c.id
    ),
    cte_used AS (
      SELECT s.id, count(m.contour) AS times_used
      FROM species AS s LEFT JOIN manual AS m ON s.id = m.species
      GROUP BY s.id
    )

    SELECT
      s.id, s.parent, s.description, s.abbreviation, c.level, u.times_used
    FROM
        cte_parent AS c
      INNER JOIN
        species AS s
      ON
        c.id = s.id
    INNER JOIN
      cte_used AS u
    ON
      s.id = u.id
    ORDER BY c.rank
    "
  )
}
