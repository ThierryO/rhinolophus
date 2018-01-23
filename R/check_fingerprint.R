#' Check the matching id for a fingerprint
#' @param src a scr_dbi object
#' @param table the name of the table in the database
#' @param current the dataframe to upload
#' @export
#' @importFrom digest sha1
#' @importFrom RSQLite dbWriteTable dbRemoveTable
#' @importFrom dplyr %>% tbl select inner_join collect mutate anti_join semi_join transmute n bind_rows
#' @importFrom rlang .data
check_fingerprint <- function(src, table, current){
  hash <- paste0("tmp", sha1(current))
  dbWriteTable(src$con, hash, current, temporary = TRUE, overwrite = TRUE)
  # existing fingerprint
  existing <- tbl(src, table) %>%
    select(.data$fingerprint, .data$id) %>%
    inner_join(x = tbl(src, hash), by = c("Fingerprint" = "fingerprint")) %>%
    collect() %>%
    mutate(
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
        transmute(
          .data$Fingerprint,
          old_id = sample(.Machine$integer.max, n())
        )
    ) %>%
      select(.data$Fingerprint, .data$id) %>%
      inner_join(old_id, by = "Fingerprint") %>%
      mutate(
        exists = FALSE,
        duplicate = TRUE
      )
  }
  # new fingerprint and new id
  new_id <- tbl(src, hash) %>%
    anti_join(tbl(src, table), by = c("Fingerprint" = "fingerprint")) %>%
    anti_join(tbl(src, table), by = c("old_id" = "id")) %>%
    collect() %>%
    mutate(
      id = .data$old_id,
      exists = FALSE,
      duplicate = FALSE
    )
  dbRemoveTable(src$con, hash)
  bind_rows(existing, old_id, new_id)
}
