#' convert the contours of a given recording in the database into a SpatialPolygonsDataFrame
#' @param connection the connection to the database
#' @param recording the id of the recording
#' @export
#' @importFrom RSQLite dbGetQuery
#' @importFrom dplyr %>% group_by do pull select mutate
#' @importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @importFrom tibble column_to_rownames
db2sp <- function(connection, recording) {
  ellipse <- outer(
    c("sin_time", "cos_time", "sin_frequency", "cos_frequency"),
    1:30,
    function(...){sprintf(fmt = "h%2$02i_%1$s", ...)}
  ) %>%
    as.vector()

  params <- sprintf("
    SELECT
      pa.contour,
      p.peak_x AS peak_time,
      p.peak_y AS peak_frequency,
      p.peak_x + pa.d_time AS d_time,
      pa.d_frequency,
      %s,
      m.species,
      m.activity,
      m.animal
    FROM
            spectrogram AS s
          INNER JOIN
            pulse AS p
          ON
            s.id = p.spectrogram
        INNER JOIN
          contour AS c
        ON
          p.id = c.pulse
      INNER JOIN
        parameter AS pa
      ON
        c.id = pa.contour
    LEFT JOIN
      manual AS m
    ON
      c.id = m.contour
    WHERE
      recording = %i
    ",
    paste(sprintf("ifnull(pa.%1$s, 0) AS %1$s", ellipse), collapse = ",\n"),
    recording
  ) %>%
    dbGetQuery(conn = connection)
  reconstruct(params) %>%
    group_by(.data$pulse) %>%
    do(
      poly = cbind(.data$time, .data$frequency) %>%
        Polygon() %>%
        list() %>%
        Polygons(ID = .data$pulse[1])
    ) %>%
    pull(.data$poly) %>%
    SpatialPolygons() %>%
    SpatialPolygonsDataFrame(
      params %>%
        select(
          "contour", "peak_time", "peak_frequency", "species", "activity",
          "animal"
        ) %>%
        mutate(rowname = .data$contour) %>%
        column_to_rownames()
    )
}
