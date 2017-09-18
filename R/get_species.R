#' get the species information of all recordings in a directory
#' @export
#' @importFrom dplyr data_frame %>% mutate_ select_ arrange_
#' @importFrom tidyr extract_ unnest_ spread_
#' @importFrom utils write.csv2
#' @param path the path of the recordings
#' @param write_csv store the data.frame to a csv file
get_species <- function(path, write_csv = TRUE) {
  species <- data_frame(
    filename = list.files(
      path = path,
      pattern = "WAV$",
      recursive = TRUE,
      full.names = TRUE
    )
  ) %>%
    mutate_(
      timestamp = ~file.info(filename)$mtime,
      path = ~dirname(filename),
      filename = ~basename(filename)
    ) %>%
    extract_(
      "filename",
      into = c("tmp", "channel", "original"),
      regex = "(.*)\u00B5(.*)\u00B5(.*)", #\u00B5 stands for mu
      remove = FALSE
    ) %>%
    mutate_(
      tmp = ~gsub(".*/", "", tmp) %>%
        strsplit("_")
    ) %>%
    unnest_(~tmp) %>%
    extract_(
      "tmp",
      into = c("species", "count"),
      regex = "(.*)-(.*)",
      remove = FALSE,
      convert = TRUE
    ) %>%
    select_(
      ~path, ~original, ~filename, ~channel, ~timestamp, ~species, ~count
    ) %>%
    spread_(key_col = "species", value_col = "count", fill = 0) %>%
    mutate_(
      time_expansion = ~gsub("[[:alpha:]]*", "", channel) %>%
        as.integer(),
      channel = ~gsub("[[:digit:]]*", "", channel) %>%
        as.factor()
    ) %>%
    arrange_(~rev(timestamp))
  if (write_csv) {
    as.data.frame(species) %>%
    write.csv2(
      file = paste0(path, "/determination.csv") %>%
        normalizePath(mustWork = FALSE),
      row.names = FALSE
    )
  }
  return(species)
}
