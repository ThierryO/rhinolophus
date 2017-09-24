#' Store a batPulse object into a database
#' @export
#' @param x a batPulse object
#' @param connection a DBI connection to the database
#' @importFrom digest sha1
#' @importFrom dplyr %>% tbl select_ inner_join collect mutate_ anti_join semi_join bind_rows transmute_ summarise_ left_join compute
#' @importFrom dbplyr src_dbi
#' @importFrom RSQLite dbWriteTable dbRemoveTable
batpulse2db <- function(x, connection){

  check_fingerprint <- function(src, table, current){
    hash <- sha1(current)
    dbWriteTable(src$con, hash, current, temporary = TRUE)
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
      collect() %>%
      mutate_(
        exists = FALSE,
        duplicate = TRUE
      )
    if (nrow(old_id)) {
      stop("existing id")
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
    stop("to do: recording id")
  }
  checked_id %>%
    filter_(~!exists) %>%
    select_(~old_id, ~id) %>%
    inner_join(x@Recording, by = c("old_id" = "ID")) %>%
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
    stop("to do: spectrogram id")
  }
  checked_id %>%
    filter_(~!exists) %>%
    select_(~old_id, ~id) %>%
    inner_join(x@Spectrogram, by = c("old_id" = "ID")) %>%
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
    stop("to do: pulse id")
  }
  checked_id %>%
    filter_(~!exists) %>%
    select_(~old_id, ~id) %>%
    inner_join(x@Pulse, by = c("old_id" = "ID")) %>%
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
    stop("to do: contour id")
  }
  checked_id %>%
    filter_(~!exists) %>%
    select_(~old_id, ~id) %>%
    inner_join(x@Contour, by = c("old_id" = "ID")) %>%
    transmute_(
      ~id,
      fingerprint = ~Fingerprint,
      pulse = ~as.integer(Pulse),
      contour_max = ~as.integer(ContourMax),
      contour_step = ~ContourStep,
      contour_amplitude = ~ContourAmplitude
    ) %>%
    dbWriteTable(conn = connection, name = "contour", append = TRUE)

  repeat {
    type_id <- x@Parameter %>%
      distinct_(~Type) %>%
      left_join(
        tbl(src, "parameter_type"),
        by = c("Type" = "description"),
        copy = TRUE
      )
    if (any(is.na(type_id$id))) {
      max_id <- tbl(src, "parameter_type") %>%
        summarise_(id = ~max(id)) %>%
        collect()
      type_id %>%
        filter_(~is.na(id)) %>%
        transmute_(
          id = ~seq_along(id) + pmin(max_id$id, 0, na.rm = TRUE),
          description = ~Type
        ) %>%
        dbWriteTable(conn = connection, name = "parameter_type", append = TRUE)
    } else {
      break
    }
  }
  values <- x@Parameter %>%
    inner_join(type_id, by = "Type") %>%
    select_(
      contour = ~ID,
      harmonic = ~Harmonic,
      parameter_type = ~id,
      value = ~Value
    )
  hash <- sha1(values)
  dbWriteTable(connection, hash, values, temporary = TRUE)
  tbl(src, hash) %>%
    anti_join(
      tbl(src, "parameter"),
      by = c("contour", "harmonic", "parameter_type")
    ) %>%
    compute("parameter", append = TRUE)
  dbRemoveTable(connection, hash)

  return(invisible(NULL))
}
