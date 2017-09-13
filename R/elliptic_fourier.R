#' Calculate the elliptic fourier transformation
#' @param x a data.frame containing X1, X2 and row_id
#' @param f_max the maximum cyclic frequency
#' @export
#' @importFrom dplyr %>% mutate_
#' @importFrom stats lm formula coef
#' @importFrom tidyr gather_
elliptic_fourier <- function(x, f_max = 30){
  f <- pmin(nrow(x) %/% 2 - 1, f_max)
  form <- seq_len(f) %>%
    sprintf(fmt = "I(sinpi(%1$i * row_id)) + I(cospi(%1$i * row_id))") %>%
    paste(collapse = "+")
  lm_x <- lm(as.formula(paste("X1~", form)), data = x)
  lm_y <- lm(as.formula(paste("X2~", form)), data = x)
  coefs <- cbind(
    X = coef(lm_x),
    Y = coef(lm_y)
  )

  params <- lapply(
    seq_len(f),
    function(i){
      data.frame(
        Harmonic = i,
        Type = c("major", "minor", "rotation"),
        Value = fourier_ellipse(fourier_param = coefs[(i - 1) * 2 + 2:3, ]),
        stringsAsFactors = FALSE
      )
    }
  ) %>%
    do.call(what = rbind)
  data.frame(
    Harmonic = 0,
    Type = c("x", "y"),
    Value = c(coef(lm_x)[1], coef(lm_y)[1]),
    stringsAsFactors = FALSE
  ) %>%
    bind_rows(params)
}
