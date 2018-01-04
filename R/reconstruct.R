#' Reconstruct pulse from their unscales Fourier parameters
#' @param params the unscales Fourier parameters
#' @param n the number of point in each pulse
#' @importFrom assertthat assert_that is.count has_name
#' @importFrom dplyr %>% select bind_cols
#' @importFrom tidyr gather
#' @importFrom stats model.matrix
#' @export
reconstruct <- function(params, n = 100) {
  assert_that(is.count(n))

  suppressWarnings(
    n.harmonic <- gsub(
        "^h([[:digit:]]*)_(sin|cos)_(time|frequency)$",
        "\\1",
        colnames(params)
      ) %>%
      as.integer() %>%
      max(na.rm = TRUE)
  )
  z <- seq_len(n.harmonic) %>%
    sprintf(fmt = "I(sinpi(%1$i * ID)) + I(cospi(%1$i * ID))") %>%
    paste(collapse = "+") %>%
    sprintf(fmt = "~%s") %>%
    as.formula() %>%
    model.matrix(data.frame(ID = 2 * (0:n) / n))

  time <- params[, "d_time"] %>%
    cbind(params[, grep("^h[[:digit:]]+_(sin|cos)_time", colnames(params))]) %>%
    as.matrix() %>%
    tcrossprod(z) %>%
    as.data.frame()
  frequency <- params[, "d_frequency"] %>%
    "+"(params[, "peak_frequency"]) %>%
    cbind(
      params[, grep("^h[[:digit:]]+_(sin|cos)_frequency", colnames(params))]
    ) %>%
    as.matrix() %>%
    tcrossprod(z) %>%
    as.data.frame()
  if (has_name(params, "contour")) {
    time$pulse <- frequency$pulse <- params[, "contour"]
  } else {
    if (has_name(params, "pulse")) {
      time$pulse <- frequency$pulse <- params[, "pulse"]
    } else {
      time$pulse <- frequency$pulse <- rownames(params)
    }
  }
  gather(time, "order", "time", seq_len(n + 1)) %>%
    bind_cols(
      gather(frequency, "order", "frequency", seq_len(n + 1)) %>%
        select("frequency")
    )
}
