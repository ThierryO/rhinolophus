#' convert the contours of a given recording in the database into a SpatialPolygonsDataFrame
#' @param connection the connection to the database
#' @param recording the id of the recording
#' @param dl get the naive deep learning classification
#' @export
#' @importFrom RSQLite dbGetQuery
#' @importFrom dplyr %>% group_by do pull select mutate arrange desc summarise left_join
#' @importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @importFrom tibble column_to_rownames
db2sp <- function(connection, recording, dl = FALSE) {
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
      u.class,
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
      INNER JOIN
        unsupervised AS u
      ON
        p.id = u.pulse
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
  if (dl) {
    params <- sprintf(
      "SELECT c.id AS contour, pr.class, pr.probability
      FROM
          spectrogram AS s INNER JOIN pulse AS p ON s.id = p.spectrogram
        INNER JOIN
          contour AS c ON p.id = c.pulse
      INNER JOIN
        prediction AS pr ON p.id = pr.pulse
      WHERE s.recording = %i",
      recording
    ) %>%
      dbGetQuery(conn = connection) %>%
      group_by(.data$contour) %>%
      arrange(desc(.data$probability)) %>%
      summarise(
        class = sprintf("%s (%.0f%%)", .data$class, 100 * .data$probability) %>%
          paste(collapse = ", "),
        score = min(.data$probability)
      ) %>%
      left_join(x = select(params, -.data$class), by = "contour")
  } else {
    params <- params %>%
      mutate(
        class = as.integer(factor(.data$class)),
        score = NA
      )
  }
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
          "animal", "class", "score"
        ) %>%
        mutate(
          rowname = .data$contour
        ) %>%
        column_to_rownames()
    )
}
