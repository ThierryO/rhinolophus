#' Extract a machine learning dataset from the database
#' @param path the path of the rhinolophus database
#' @param n.harmonic the number of harmonics to extract
#' @param contour.amplitude the amplitude of the contour to extract
#' @export
#' @importFrom assertthat assert_that is.count is.number
#' @importFrom RSQLite dbConnect SQLite dbDisconnect
#' @importFrom dplyr %>% select inner_join group_by summarise mutate
#' @importFrom rlang .data
#' @importFrom tidyr spread
db2ml <- function(path, n.harmonic = 30, contour.amplitude = -25) {
  assert_that(is.count(n.harmonic))
  assert_that(is.number(contour.amplitude))

  connection <- connect_db(path)

  ellipse <- outer(
    c("sin_time", "cos_time", "sin_frequency", "cos_frequency"),
    seq_len(n.harmonic),
    function(...){sprintf(fmt = "h%2$02i_%1$s", ...)}
  ) %>%
    as.vector()

  l1 <- sprintf("
    cte_L1 AS (
      SELECT
        contour,
        %s AS L1
      FROM
        parameter AS pa
    )",
    paste(sprintf("abs(ifnull(%s, 0))", ellipse), collapse = "+")
  )

  max.L1 <- sprintf("WITH %s SELECT max(L1) AS maxL1 FROM cte_l1", l1) %>%
    dbGetQuery(conn = connection) %>%
    pull("maxL1")

  z <- sprintf("
    WITH %s

    SELECT
      c.pulse AS pulse,
      p.peak_y / 100 AS peak_frequency,
      p.peak_amplitude / 100 AS peak_amplitude,
      pa.d_time / 10 AS d_time,
      pa.d_frequency / 100 AS d_frequency,
      l1.L1 / %f AS L1,
      %s
    FROM
      contour AS c
    INNER JOIN
      pulse AS p
    ON
      c.pulse = p.id
    INNER JOIN
      parameter AS pa
    ON
      c.id = pa.contour
    INNER JOIN
      cte_l1 AS l1
    ON
      c.id = l1.contour
    WHERE
      contour_amplitude = %i",
    l1,
    max.L1,
    paste(
      sprintf("ifnull(%1$s, 0)*2/l1.L1 AS %1$s", ellipse),
      collapse = ",\n"
    ),
    contour.amplitude
  ) %>%
    dbGetQuery(conn = connection)

  attr(z, "maxL1") <- max.L1
  attr(z, "peak_frequency") <- 100
  attr(z, "peak_amplitude") <- 100
  attr(z, "d_frequency") <- 100
  attr(z, "d_time") <- 10
  dbDisconnect(connection)
  return(z)
}
