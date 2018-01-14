#' Retrieve a random recording id
#' @param connection a connection to the database
#' @export
#' @importFrom RSQLite dbGetQuery
label_dl <- function(connection) {
  dbGetQuery(
    conn = connection, "
    WITH c_p AS (
      SELECT p.pulse AS pulse, min(probability) AS worst
      FROM
        prediction AS p
      LEFT JOIN
        (contour AS c INNER JOIN manual AS m ON c.id = m.contour)
      ON
        p.pulse = c.pulse
      WHERE m.species IS NULL
      GROUP BY p.pulse
    ),
    c_s AS (
      SELECT p.spectrogram, avg(1/worst) AS score
      FROM c_p INNER JOIN pulse AS p ON c_p.pulse = p.id
      GROUP BY p.spectrogram
      ORDER BY score DESC
      LIMIT 20
    )

    SELECT recording, filename, t_e_factor, left_channel
    FROM
      c_s INNER JOIN spectrogram AS s ON c_s.spectrogram = s.id
    INNER JOIN
      recording AS r ON s.recording = r.id
    ORDER BY random()
    LIMIT 1"
  )
}
