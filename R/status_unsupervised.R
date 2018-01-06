#' Create an overview of the labels and unsupervised classes
#' @inheritParams db2sp
#' @export
#' @importFrom RSQLite dbGetQuery
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
status_unsupervised <- function(connection) {
  dbGetQuery(
    conn = connection, "
    WITH cte_status AS (
      SELECT u.class, m.species, COUNT(u.pulse) AS n
      FROM
        unsupervised AS u INNER JOIN contour AS c ON u.pulse = c.pulse
      LEFT JOIN manual AS m
        ON c.id = m.contour
      GROUP BY u.class, m.species
    ),
    cte_total AS (
      SELECT class, SUM(n) AS total
      FROM cte_status
      GROUP BY class
    ),
    cte_done AS (
      SELECT class, SUM(n) AS done, MAX(n) AS max, COUNT(n) AS species
      FROM cte_status
      WHERE species IS NOT NULL
      GROUP BY class
    )

    SELECT
      t.class, t.total, d.done, d.max, d.done - d.max AS alternative, d.species
    FROM cte_total AS t LEFT JOIN cte_done AS d ON t.class = d.class
    ORDER BY
      t.total < 160, d.done - d.max < 10, species DESC, d.done - d.max DESC
  ") %>%
    mutate(split = .data$total > 160 & .data$alternative >= 10)

}
