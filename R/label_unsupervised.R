#' Retrieve the recording id which contains unlabeled pulses for many classes
#' @param connection a connection to the database
#' @export
#' @importFrom RSQLite dbGetQuery
label_unsupervised <- function(connection) {
  dbGetQuery(conn = connection, "
    WITH cte_unlabeled AS (
      SELECT
        c.pulse
      FROM
        contour AS c
      LEFT JOIN
        manual AS m
      ON
        c.id = m.contour
      WHERE
        m.species IS NULL
      GROUP BY
        c.pulse
    ),
    cte_pulse AS (
      SELECT
        u.class,
        s.recording,
        COUNT(p.id) AS n
      FROM
            cte_unlabeled AS cu
          INNER JOIN
            pulse AS p
          ON
            cu.pulse = p.id
        INNER JOIN
          spectrogram AS s
        ON
          p.spectrogram = s.id
      INNER JOIN
        unsupervised AS u
      ON
        p.id = u.pulse
      GROUP BY
        u.class, s.recording
    ),
    cte_weight AS (
      SELECT
        u.class,
        1 / (COUNT(m.species) + 1) AS weight
      FROM
          unsupervised AS u
        INNER JOIN
          contour AS c
        ON
          u.pulse = c.pulse
      LEFT JOIN
        manual AS m
      ON
        c.id = m.contour
      GROUP BY
        u.class
    )

    SELECT
      cc.recording,
      SUM(cc.n * cw.weight) AS total
    FROM
      cte_pulse AS cc
    INNER JOIN
      cte_weight AS cw
    ON
      cc.class = cw.class
    GROUP BY
      recording
    ORDER BY total DESC LIMIT 1"
  )
}
