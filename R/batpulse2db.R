#' Store a batPulse object into a database
#' @export
#' @param x a batPulse object
#' @param connection a DBI connection to the database
#' @importFrom digest sha1
#' @importFrom dplyr %>% tbl select_ inner_join collect mutate_ anti_join semi_join bind_rows transmute_ summarise_ left_join compute n
#' @importFrom dbplyr src_dbi
#' @importFrom RSQLite dbWriteTable dbRemoveTable
batpulse2db <- function(x, connection){

  check_fingerprint <- function(src, table, current){
    hash <- paste0("tmp", sha1(current))
    dbWriteTable(src$con, hash, current, temporary = TRUE, overwrite = TRUE)
    # existing fingerprint
    existing <- tbl(src, table) %>%
      select_(~fingerprint, ~id) %>%
      inner_join(x = tbl(src, hash), by = c("Fingerprint" = "fingerprint")) %>%
      collect() %>%
      mutate_(
        exists = TRUE,
        duplicate = FALSE
      )
    # new fingerprint and existing id
    old_id <- tbl(src, hash) %>%
      anti_join(tbl(src, table), by = c("Fingerprint" = "fingerprint")) %>%
      semi_join(tbl(src, table), by = c("old_id" = "id")) %>%
      collect()
    if (nrow(old_id)) {
      old_id <- check_fingerprint(
        src = src,
        table = table,
        current = old_id %>%
          transmute_(~Fingerprint, old_id = ~sample(.Machine$integer.max, n()))
      ) %>%
        select_(~Fingerprint, ~id) %>%
        inner_join(old_id, by = "Fingerprint") %>%
        mutate_(
          exists = FALSE,
          duplicate = TRUE
        )
    }
    # new fingerprint and new id
    new_id <- tbl(src, hash) %>%
      anti_join(tbl(src, table), by = c("Fingerprint" = "fingerprint")) %>%
      anti_join(tbl(src, table), by = c("old_id" = "id")) %>%
      mutate_(id = ~old_id) %>%
      collect() %>%
      mutate_(
        exists = FALSE,
        duplicate = FALSE
      )
    dbRemoveTable(src$con, hash)
    bind_rows(existing, old_id, new_id)
  }

  src <- src_dbi(connection)

  checked_id <- x@Recording %>%
    select_(~Fingerprint, old_id = ~ID) %>%
    check_fingerprint(src = src, table = "recording")
  if (any(checked_id$duplicate)) {
    x@Spectrogram <- checked_id %>%
      select_(~old_id, Recording = ~id) %>%
      inner_join(x@Spectrogram, by = c("old_id" = "Recording")) %>%
      select_(~-old_id)
    x@Recording <- checked_id %>%
      select_(~old_id, ID = ~id) %>%
      inner_join(x@Recording, by = c("old_id" = "ID")) %>%
      select_(~-old_id)
  }
  checked_id %>%
    filter_(~!exists) %>%
    select_(~id) %>%
    inner_join(x@Recording, by = c("id" = "ID")) %>%
    transmute_(
      ~id,
      fingerprint = ~Fingerprint,
      filename = ~Filename,
      timestamp = ~as.numeric(Timestamp),
      sample_rate = ~as.integer(SampleRate),
      t_e_factor = ~as.integer(TEFactor),
      left_channel = ~as.integer(LeftChannel)
    ) %>%
    dbWriteTable(conn = connection, name = "recording", append = TRUE)

  checked_id <- x@Spectrogram %>%
    select_(~Fingerprint, old_id = ~ID) %>%
    check_fingerprint(src = src, table = "spectrogram")
  if (any(checked_id$duplicate)) {
    x@Pulse <- checked_id %>%
      select_(~old_id, Spectrogram = ~id) %>%
      inner_join(x@Pulse, by = c("old_id" = "Spectrogram")) %>%
      select_(~-old_id)
    x@Spectrogram <- checked_id %>%
      select_(~old_id, ID = ~id) %>%
      inner_join(x@Spectrogram, by = c("old_id" = "ID")) %>%
      select_(~-old_id)
  }
  checked_id %>%
    filter_(~!exists) %>%
    select_(~id) %>%
    inner_join(x@Spectrogram, by = c("id" = "ID")) %>%
    transmute_(
      ~id,
      fingerprint = ~Fingerprint,
      recording = ~as.integer(Recording),
      window_ms = ~WindowMS,
      window_n = ~as.integer(WindowN),
      overlap = ~Overlap
    ) %>%
    dbWriteTable(conn = connection, name = "spectrogram", append = TRUE)

  checked_id <- x@Pulse %>%
    select_(~Fingerprint, old_id = ~ID) %>%
    check_fingerprint(src = src, table = "pulse")
  if (any(checked_id$duplicate)) {
    x@Contour <- checked_id %>%
      select_(~old_id, Pulse = ~id) %>%
      inner_join(x@Contour, by = c("old_id" = "Pulse")) %>%
      select_(~-old_id)
    x@Pulse <- checked_id %>%
      select_(~old_id, ID = ~id) %>%
      inner_join(x@Pulse, by = c("old_id" = "ID")) %>%
      select_(~-old_id)
  }
  checked_id %>%
    filter_(~!exists) %>%
    select_(~id) %>%
    inner_join(x@Pulse, by = c("id" = "ID")) %>%
    transmute_(
      ~id,
      fingerprint = ~Fingerprint,
      spectrogram = ~as.integer(Spectrogram),
      peak_x = ~PeakX,
      peak_y = ~PeakY,
      peak_amplitude = ~PeakAmplitude
    ) %>%
    dbWriteTable(conn = connection, name = "pulse", append = TRUE)

  checked_id <- x@Contour %>%
    select_(~Fingerprint, old_id = ~ID) %>%
    check_fingerprint(src = src, table = "contour")
  if (any(checked_id$duplicate)) {
    x@Parameter <- checked_id %>%
      select_(~old_id, ID = ~id) %>%
      inner_join(x@Parameter, by = c("old_id" = "ID")) %>%
      select_(~-old_id)
    x@Contour <- checked_id %>%
      select_(~old_id, ID = ~id) %>%
      inner_join(x@Contour, by = c("old_id" = "ID")) %>%
      select_(~-old_id)
  }
  checked_id %>%
    filter_(~!exists) %>%
    select_(~id) %>%
    inner_join(x@Contour, by = c("id" = "ID")) %>%
    transmute_(
      ~id,
      fingerprint = ~Fingerprint,
      pulse = ~as.integer(Pulse),
      contour_max = ~as.integer(ContourMax),
      contour_step = ~ContourStep,
      contour_amplitude = ~ContourAmplitude
    ) %>%
    dbWriteTable(conn = connection, name = "contour", append = TRUE)

  if (all(checked_id$exists)) {
    return(invisible(NULL))
  }
  values <- checked_id %>%
    filter_(~!exists) %>%
    select_(~id) %>%
    inner_join(x@Parameter, by = c("id" = "ID")) %>%
    transmute_(
      contour = .data$id,
      Type = ~ifelse(
        Harmonic == 0,
        Type,
        sprintf("h%02i_%s", Harmonic, Type)
      ),
      ~Value
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
