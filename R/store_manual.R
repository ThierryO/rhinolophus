#' write the manual classification to the database
#' @inheritParams db2sp
#' @param manual a data.frame with manual classification
#' @export
#' @importFrom dplyr %>% filter select
#' @importFrom rlang .data
#' @importFrom digest sha1
#' @importFrom RSQLite dbWriteTable dbSendQuery dbClearResult dbRemoveTable
store_manual <- function(connection, manual) {
  update <- manual %>%
    filter(!is.na(.data$species)) %>%
    select("contour", "species", "activity", "animal")
  if (nrow(update) == 0) {
    return(NULL)
  }
  hash <- paste0("manual_", sha1(update))
  dbWriteTable(connection, hash, update)
  res <- dbSendQuery(
    conn = connection,
    sprintf("
      INSERT OR REPLACE INTO
        manual (contour, species, activity, animal)
      SELECT
        h.contour, h.species, h.activity, h.animal
      FROM
        %s AS h
      ",
      hash
    )
  )
  dbClearResult(res)
  dbRemoveTable(connection, hash)
}
