#' Connect to a rhinolophus database
#' @param path the location of the database
#' @export
#' @importFrom dplyr %>%
#' @importFrom RSQLite SQLite dbConnect dbSendQuery dbClearResult
connect_db <- function(path){
  connection <- paste(path, "rhinolophus.sqlite", sep = "/") %>%
    normalizePath(mustWork = FALSE) %>%
    dbConnect(drv = SQLite())
  res <- dbSendQuery(
    connection, "
    CREATE TABLE IF NOT EXISTS recording (
      id INTEGER PRIMARY KEY,
      fingerprint TEXT NOT NULL,
      filename TEXT NOT NULL,
      timestamp INTEGER NOT NULL,
      sample_rate INTEGER NOT NULL,
      t_e_factor INTEGER NOT NULL,
      left_channel INTEGER NOT NULL
    )
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS
      idx_recording_fingerprint
    ON
      recording (fingerprint)
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE INDEX IF NOT EXISTS idx_recording_timestamp ON recording (timestamp)
  ")
  dbClearResult(res)

  res <- dbSendQuery(
    connection, "
    CREATE TABLE IF NOT EXISTS spectrogram (
      id INTEGER PRIMARY KEY,
      fingerprint TEXT NOT NULL,
      recording INTEGER NOT NULL,
      window_ms REAL NOT NULL,
      window_n INTEGER NOT NULL,
      overlap REAL NOT NULL,
      FOREIGN KEY (recording) REFERENCES recording (id)
    )
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS
      idx_spectrogram_fingerprint
    ON
      spectrogram (fingerprint)
  ")
  dbClearResult(res)

  res <- dbSendQuery(
    connection, "
    CREATE TABLE IF NOT EXISTS pulse (
      id INTEGER PRIMARY KEY,
      fingerprint TEXT NOT NULL,
      spectrogram INTEGER NOT NULL,
      peak_x REAL NOT NULL,
      peak_y REAL NOT NULL,
      peak_amplitude REAL NOT NULL,
      FOREIGN KEY (spectrogram) REFERENCES spectrogram (id)
    )
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS
      idx_pulse_fingerprint
    ON
      pulse (fingerprint)
  ")
  dbClearResult(res)

  res <- dbSendQuery(
    connection, "
    CREATE TABLE IF NOT EXISTS contour (
      id INTEGER PRIMARY KEY,
      fingerprint TEXT NOT NULL,
      pulse INTEGER NOT NULL,
      contour_max integer NOT NULL,
      contour_step REAL NOT NULL,
      contour_amplitude REAL NOT NULL,
      FOREIGN KEY (pulse) REFERENCES pulse (id)
    )
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS
      idx_contour_fingerprint
    ON
      contour (fingerprint)
  ")
  dbClearResult(res)

  ellipse <- outer(
    1:30,
    c("sin_time", "cos_time", "sin_frequency", "cos_frequency"),
    function(...){sprintf(fmt = "h%02i_%s", ...)}
  ) %>%
    as.vector() %>%
    paste(collapse = " REAL,\n")
  res <- dbSendQuery(
    connection,
    sprintf("
      CREATE TABLE IF NOT EXISTS parameter (
        contour INTEGER PRIMARY KEY,
        d_time REAL NOT NULL,
        d_frequency REAL NOT NULL,
        %s,
        FOREIGN KEY (contour) REFERENCES contour (id)
      )",
      ellipse
    )
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection, "
    CREATE TABLE IF NOT EXISTS unsupervised (
      pulse INTEGER PRIMARY KEY,
      class INTEGER NOT NULL,
      distance NUMERIC NOT NULL,
      FOREIGN KEY(pulse) REFERENCES pulse (id)
    )
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE INDEX IF NOT EXISTS
      idx_unsupervised_class
    ON
      unsupervised (class)
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE INDEX IF NOT EXISTS
      idx_unsupervised_distance
    ON
      unsupervised (distance)
  ")
  dbClearResult(res)

  res <- dbSendQuery(
    connection, "
    CREATE TABLE IF NOT EXISTS species (
      id INTEGER PRIMARY KEY,
      parent INTEGER,
      description TEXT NOT NULL,
      abbreviation TEXT NOT NULL,
      FOREIGN KEY (parent) REFERENCES species (id)
    )
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE INDEX IF NOT EXISTS
      idx_species_parent
    ON
      species (parent)
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS
      idx_species_description
    ON
      species (description)
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS
      idx_species_abbrevation
    ON
      species (abbreviation)
  ")
  dbClearResult(res)

  res <- dbSendQuery(
    connection, "
    CREATE TABLE IF NOT EXISTS activity (
      id INTEGER PRIMARY KEY,
      description TEXT NOT NULL
    )
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS
      idx_activity_description
    ON
      activity (description)
  ")
  dbClearResult(res)

  res <- dbSendQuery(
    connection, "
    CREATE TABLE IF NOT EXISTS manual (
      contour INTEGER PRIMARY KEY,
      species INTEGER NOT NULL,
      activity INTEGER,
      animal INTEGER,
      FOREIGN KEY (contour) REFERENCES contour (id),
      FOREIGN KEY (species) REFERENCES species (id),
      FOREIGN KEY (activity) REFERENCES activity (id)
    )
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE INDEX IF NOT EXISTS
      idx_manual_species
    ON
      manual (species)
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE INDEX IF NOT EXISTS
      idx_manual_activity
    ON
      manual (activity)
  ")
  dbClearResult(res)

  return(connection)
}
