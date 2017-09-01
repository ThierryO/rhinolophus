#' Convert rda files to a set of patterns
#' @export
#' @param path The path of the rda files
#' @param n.part The number of fourier components to use. Defaults to 10
#' @importFrom assertthat assert_that is.string is.count
rda2pattern <- function(path, n.part = 10){
  assert_that(is.string(path))
  assert_that(is.count(n.part))

  ### Fooling R CMD check
  pulses.fft <- NULL
  rm(pulses.fft)
  ### Fooling R CMD check

  filenames <- list.files(
    path = path,
    pattern = "\\.rda$",
    recursive = TRUE,
    ignore.case = TRUE,
    full.names = TRUE
  )
  positions <- do.call(
    rbind,
    lapply(seq_len(n.part), function(i){
      if (i == 1) {
        return(matrix(1, ncol = 2, nrow = 1))
      }
      part.positions <- rbind(
        cbind(seq_len(i), i),
        cbind(i, seq_len(i - 1))
      )
      part.positions[order(rowSums(part.positions)), ]
    })
  )
  do.call(rbind, lapply(filenames, function(filename){
    load(filename)
    pattern <- t(sapply(
      pulses.fft,
      function(x){
        c(
          x$parameters,
          Mod(x$fft[positions])
        )
      }
    ))
    selection <- rev(seq(ncol(pattern), length = nrow(positions), by = -1))
    colnames(pattern)[selection] <- sprintf(
      "fft.%02i.%02i",
      positions[, 1],
      positions[, 2]
    )
    pattern <- as.data.frame(pattern)
    cbind(Filename = gsub("rda$", "WAV", filename), pattern)
  }))
}
