#' Shift the timestamp 2 hours back
#'
#' Note that ALL files in the path will be recursively updated
#' @param path the root folder to be changed
#' @export
#' @importFrom dplyr %>%
correct_timestamp <- function(path){
  if (Sys.info()["sysname"] != "Linux") {
    stop("This function works currently only on Linux")
  }
  path <- normalizePath(path = path)
  sprintf(
    'find %s -print | while read filename; do
        touch -d "$(date -R -r "$filename") - 2 hours" "$filename"
    done',
    path
  ) %>%
    system()
  message("timestamp updated")
}
