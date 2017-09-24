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
      timestamp INTEGER NOT NULL,
      sample_rate INTEGER NOT NULL,
      t_e_factor INTEGER NOT NULL,
      left_channel INTEGER NOT NULL
    ) WITHOUT ROWID
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE UNIQUE INDEX idx_recording_fingerprint ON recording (fingerprint)
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE INDEX idx_recording_timestamp ON recording (timestamp)
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
    ) WITHOUT ROWID
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE UNIQUE INDEX idx_spectrogram_fingerprint ON spectrogram (fingerprint)
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
    ) WITHOUT ROWID
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE UNIQUE INDEX idx_pulse_fingerprint ON pulse (fingerprint)
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
    ) WITHOUT ROWID
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE UNIQUE INDEX idx_contour_fingerprint ON contour (fingerprint)
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE TABLE IF NOT EXISTS parameter_type (
      id INTEGER PRIMARY KEY,
      description TEXT NOT NULL
    ) WITHOUT ROWID
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE UNIQUE INDEX
      idx_parameter_type_description
    ON
      parameter_type (description)
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE TABLE IF NOT EXISTS parameter (
      id INTEGER PRIMARY KEY,
      contour INTEGER NOT NULL,
      harmonic INTEGER NOT NULL,
      parameter_type integer NOT NULL,
      value REAL NOT NULL,
      FOREIGN KEY (contour) REFERENCES contour (id),
      FOREIGN KEY (parameter_type) REFERENCES parameter_type (id)
    ) WITHOUT ROWID
  ")
  dbClearResult(res)
  res <- dbSendQuery(
    connection, "
    CREATE UNIQUE INDEX
      idx_parameter_contour_harmonic_type
    ON
      parameter (contour, harmonic, parameter_type)
  ")
  dbClearResult(res)
  return(connection)
}
