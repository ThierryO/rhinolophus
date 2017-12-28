#' Extract a machine learning dataset from the database
#' @param db.path the path of the rhinolophus database
#' @param n.harmonic the number of harmonics to extract
#' @param contour.amplitude the amplitude of the contour to extract
#' @export
#' @importFrom assertthat assert_that is.count is.number
#' @importFrom RSQLite dbConnect SQLite
#' @importFrom dplyr %>% tbl filter_ select inner_join transmute semi_join collect group_by summarise
#' @importFrom dbplyr src_dbi
#' @importFrom rlang .data
#' @importFrom tidyr spread
db2ml <- function(db.path, n.harmonic = 20, contour.amplitude = -25) {
  assert_that(is.count(n.harmonic))
  assert_that(is.number(contour.amplitude))

  connection <- paste0(db.path, "/rhinolophus.sqlite") %>%
    normalizePath() %>%
    dbConnect(drv = SQLite())
  src <- src_dbi(connection)

  relevant <- tbl(src, "contour") %>%
    filter_(~contour.amplitude == contour_amplitude) %>%
    select("id", "pulse") %>%
    inner_join(
      tbl(src, "pulse") %>%
        select("id", peak_frequency = "peak_y", "peak_amplitude"),
      by = c("pulse" = "id")
    )

  fourier <- tbl(src, "parameter") %>%
    semi_join(relevant, by = c("contour" = "id")) %>%
    filter_(~harmonic <= n.harmonic) %>%
    inner_join(
      tbl(src, "parameter_type") %>%
        filter_(~description != "d_time", ~description != "d_frequency"),
      by = c("parameter_type" = "id")
    ) %>%
    collect() %>%
    transmute(
      .data$contour,
      harmonic = sprintf("%s%02i", .data$description, .data$harmonic),
      .data$value
    )
  fourier <- fourier %>%
    group_by(.data$contour) %>%
    summarise(L1 = sum(abs(.data$value))) %>%
    inner_join(fourier, by = "contour") %>%
    spread("harmonic", "value", fill = 0)

  center <- tbl(src, "parameter") %>%
    semi_join(relevant, by = c("contour" = "id")) %>%
    filter_(~harmonic <= n.harmonic) %>%
    inner_join(
      tbl(src, "parameter_type") %>%
        filter_(~description == "d_time" | description == "d_frequency"),
      by = c("parameter_type" = "id")
    ) %>%
    select("contour", "description", "value") %>%
    collect() %>%
    spread("description", "value", fill = 0)

  relevant %>%
    collect() %>%
    inner_join(center, by = c("id" = "contour")) %>%
    inner_join(fourier, by = c("id" = "contour")) %>%
    select(-.data$id)
}
