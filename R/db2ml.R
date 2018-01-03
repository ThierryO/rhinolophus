#' Extract a machine learning dataset from the database
#' @param db.path the path of the rhinolophus database
#' @param n.harmonic the number of harmonics to extract
#' @param contour.amplitude the amplitude of the contour to extract
#' @export
#' @importFrom assertthat assert_that is.count is.number
#' @importFrom RSQLite dbConnect SQLite dbDisconnect
#' @importFrom dplyr %>% select inner_join group_by summarise mutate
#' @importFrom rlang .data
#' @importFrom tidyr spread
db2ml <- function(db.path, n.harmonic = 20, contour.amplitude = -25) {
  assert_that(is.count(n.harmonic))
  assert_that(is.number(contour.amplitude))

  connection <- connect_db(db.path)

  relevant <- sprintf("cte_relevant AS (
    SELECT
      contour.id,
      pulse,
      peak_y / 100 AS peak_frequency,
      peak_amplitude / 100 AS peak_amplitude
    FROM
      contour
    INNER JOIN
      pulse
    ON
      contour.pulse = pulse.id
    WHERE
      contour_amplitude = %i)",
    contour.amplitude
  )

  params <- sprintf("cte_param AS (
    SELECT
      contour, harmonic, description AS parameter_type, value
    FROM
      parameter
    INNER JOIN
      parameter_type
    ON
      parameter.parameter_type = parameter_type.id
    WHERE
      harmonic > 0 AND
      harmonic <= %i)",
    n.harmonic
  )

  fourier <- sprintf("WITH %s, %s
    SELECT
      contour,
      'h' || substr('00' || harmonic, -2, 2) || parameter_type AS harmonic,
      value
    FROM
      cte_relevant
    INNER JOIN
      cte_param
    ON
      cte_relevant.id = cte_param.contour",
    relevant,
    params
  ) %>%
    dbGetQuery(conn = connection)
  fourier <- fourier %>%
    group_by(.data$contour) %>%
    summarise(L1 = sum(abs(.data$value))) %>%
    inner_join(fourier, by = "contour") %>%
    mutate(value = .data$value * 2 / .data$L1) %>%
    spread("harmonic", "value", fill = 0)
  max.L1 <- max(fourier$L1)
  fourier$L1 <- fourier$L1 / max.L1

  params <- "cte_param AS (
    SELECT
      contour, harmonic, description AS parameter_type, value
    FROM
      parameter
    INNER JOIN
      parameter_type
    ON
      parameter.parameter_type = parameter_type.id
    WHERE
      harmonic = 0)"

  center <- sprintf("WITH %s, %s
    SELECT
      contour, parameter_type, value
    FROM
      cte_relevant
    INNER JOIN
      cte_param
    ON
      cte_relevant.id = cte_param.contour",
    relevant,
    params
  ) %>%
    dbGetQuery(conn = connection) %>%
    spread("parameter_type", "value", fill = 0) %>%
    mutate(
      d_frequency = .data$d_frequency / 100,
      d_time = .data$d_time / 10
    )

  z <- sprintf("WITH %s
    SELECT
      id, pulse, peak_frequency, peak_amplitude
    FROM
      cte_relevant",
    relevant
  ) %>%
    dbGetQuery(conn = connection) %>%
    inner_join(center, by = c("id" = "contour")) %>%
    inner_join(fourier, by = c("id" = "contour")) %>%
    select(-.data$id)
  attr(z, "maxL1") <- max.L1
  attr(z, "peak_frequency") <- 100
  attr(z, "peak_amplitude") <- 100
  attr(z, "d_frequency") <- 100
  attr(z, "d_time") <- 10
  dbDisconnect(connection)
  return(z)
}
