#' Store a batPulse object into a database
#' @export
#' @param x a batPulse object
#' @param connection a DBI connection to the database
#' @importFrom digest sha1
#' @importFrom dplyr %>% tbl select inner_join collect mutate_ anti_join semi_join bind_rows transmute left_join compute n
#' @importFrom dbplyr src_dbi
#' @importFrom RSQLite dbWriteTable dbRemoveTable dbSendQuery dbClearResult
#' @importFrom rlang .data
batpulse2db <- function(x, connection){

  src <- src_dbi(connection)

  checked_id <- x@Recording %>%
    select(.data$Fingerprint, old_id = .data$ID) %>%
    check_fingerprint(src = src, table = "recording")
  if (any(checked_id$duplicate)) {
    x@Spectrogram <- checked_id %>%
      select(.data$old_id, Recording = .data$id) %>%
      inner_join(x@Spectrogram, by = c("old_id" = "Recording")) %>%
      select(-.data$old_id)
    x@Recording <- checked_id %>%
      select(.data$old_id, ID = .data$id) %>%
      inner_join(x@Recording, by = c("old_id" = "ID")) %>%
      select(-.data$old_id)
  }
  checked_id %>%
    filter(!.data$exists) %>%
    select(.data$id) %>%
    inner_join(x@Recording, by = c("id" = "ID")) %>%
    transmute(
      .data$id,
      fingerprint = .data$Fingerprint,
      filename = .data$Filename,
      timestamp = .data$as.numeric(Timestamp),
      sample_rate = .data$as.integer(SampleRate),
      t_e_factor = .data$as.integer(TEFactor),
      left_channel = .data$as.integer(LeftChannel)
    ) %>%
    dbWriteTable(conn = connection, name = "recording", append = TRUE)

  checked_id <- x@Spectrogram %>%
    select(.data$Fingerprint, old_id = .data$ID) %>%
    check_fingerprint(src = src, table = "spectrogram")
  if (any(checked_id$duplicate)) {
    x@Pulse <- checked_id %>%
      select(.data$old_id, Spectrogram = .data$id) %>%
      inner_join(x@Pulse, by = c("old_id" = "Spectrogram")) %>%
      select(-.data$old_id)
    x@Spectrogram <- checked_id %>%
      select(.data$old_id, ID = .data$id) %>%
      inner_join(x@Spectrogram, by = c("old_id" = "ID")) %>%
      select(-.data$old_id)
  }
  checked_id %>%
    filter(!.data$exists) %>%
    select(.data$id) %>%
    inner_join(x@Spectrogram, by = c("id" = "ID")) %>%
    transmute(
      .data$id,
      fingerprint = .data$Fingerprint,
      recording = as.integer(.data$Recording),
      window_ms = .data$WindowMS,
      window_n = as.integer(.data$WindowN),
      overlap = .data$Overlap
    ) %>%
    dbWriteTable(conn = connection, name = "spectrogram", append = TRUE)

  checked_id <- x@Pulse %>%
    select(.data$Fingerprint, old_id = .data$ID) %>%
    check_fingerprint(src = src, table = "pulse")
  if (any(checked_id$duplicate)) {
    x@Contour <- checked_id %>%
      select(.data$old_id, Pulse = .data$id) %>%
      inner_join(x@Contour, by = c("old_id" = "Pulse")) %>%
      select(-.data$old_id)
    x@Pulse <- checked_id %>%
      select(.data$old_id, ID = .data$id) %>%
      inner_join(x@Pulse, by = c("old_id" = "ID")) %>%
      select(-.data$old_id)
  }
  checked_id %>%
    filter(!.data$exists) %>%
    select(.data$id) %>%
    inner_join(x@Pulse, by = c("id" = "ID")) %>%
    transmute(
      .data$id,
      fingerprint = .data$Fingerprint,
      spectrogram = as.integer(.data$Spectrogram),
      peak_x = .data$PeakX,
      peak_y = .data$PeakY,
      peak_amplitude = .data$PeakAmplitude
    ) %>%
    dbWriteTable(conn = connection, name = "pulse", append = TRUE)

  checked_id <- x@Contour %>%
    select(.data$Fingerprint, old_id = .data$ID) %>%
    check_fingerprint(src = src, table = "contour")
  if (any(checked_id$duplicate)) {
    x@Parameter <- checked_id %>%
      select(.data$old_id, ID = .data$id) %>%
      inner_join(x@Parameter, by = c("old_id" = "ID")) %>%
      select(-.data$old_id)
    x@Contour <- checked_id %>%
      select(.data$old_id, ID = .data$id) %>%
      inner_join(x@Contour, by = c("old_id" = "ID")) %>%
      select(-.data$old_id)
  }
  checked_id %>%
    filter(!.data$exists) %>%
    select(.data$id) %>%
    inner_join(x@Contour, by = c("id" = "ID")) %>%
    transmute(
      .data$id,
      fingerprint = .data$Fingerprint,
      pulse = as.integer(.data$Pulse),
      contour_max = as.integer(.data$ContourMax),
      contour_step = .data$ContourStep,
      contour_amplitude = .data$ContourAmplitude
    ) %>%
    dbWriteTable(conn = connection, name = "contour", append = TRUE)

  if (all(checked_id$exists)) {
    return(invisible(NULL))
  }
  values <- checked_id %>%
    filter(!.data$exists) %>%
    select(.data$id) %>%
    inner_join(x@Parameter, by = c("id" = "ID")) %>%
    transmute(
      contour = .data$id,
      Type = ifelse(
        .data$Harmonic == 0,
        .data$Type,
        sprintf("h%02i_%s", .data$Harmonic, .data$Type)
      ),
      .data$Value
    ) %>%
    spread("Type", "Value")
  hash <- paste0("tmp", sha1(values))
  dbWriteTable(connection, hash, values, temporary = TRUE, overwrite = TRUE)
  res <- dbSendQuery(
    connection, sprintf("
    CREATE UNIQUE INDEX IF NOT EXISTS
      idx_%1$s
    ON
      %1$s (contour)
  ", hash))
  dbClearResult(res)
  junk <- tbl(src, hash) %>%
    anti_join(
      tbl(src, "parameter"),
      by = "contour"
    ) %>%
    compute(paste0(hash, "2"), temporary = TRUE, overwrite = TRUE)

  res <- dbSendQuery(
    connection,
    sprintf("
      INSERT INTO
        parameter (%1$s)
      SELECT
        %1$s
      FROM %2$s2",
      paste(colnames(values), collapse = ", "),
      hash
    )
  )
  dbClearResult(res)
  dbRemoveTable(connection, hash)
  dbRemoveTable(connection, paste0(hash, "2"))

  return(invisible(NULL))
}
