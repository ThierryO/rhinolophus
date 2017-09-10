#' Calculate the elliptic fourier transformation
#' @param x a data.frame containing X1, X2 and row_id
#' @param f_max the maximum cyclic frequency
#' @export
#' @importFrom dplyr %>% mutate_
#' @importFrom stats lm formula coef
#' @importFrom tidyr gather_
elliptic_fourier <- function(x, f_max = 30){
  f <- pmin(nrow(x), f_max)
  form <- seq_len(f) %>%
    sprintf(fmt = "I(sinpi(%1$i * row_id)) + I(cospi(%1$i * row_id))") %>%
    paste(collapse = "+")
  lm_x <- lm(as.formula(paste("X1~", form)), data = x)
  lm_y <- lm(as.formula(paste("X2~", form)), data = x)
  coefs <- cbind(
    X = coef(lm_x),
    Y = coef(lm_y)
  ) %>%
    rbind(matrix(0, nrow = 2 * (f_max - f), ncol = 2))
  cos <- coefs[seq(3, by = 2, length.out = f_max), ]
  sin <- coefs[seq(2, by = 2, length.out = f_max), ]
  amplitude <- sqrt(cos ^ 2 + sin ^ 2)
  phase <- atan2(cos, sin)
  rbind(
    coefs[1, ],
    unname(amplitude),
    unname(phase)
  ) %>%
    as.data.frame() %>%
    mutate_(
      CycleFrequency = ~c(0, seq_len(f_max), seq_len(f_max)),
      Type = ~c(rep("Amplitude", f_max + 1), rep("Phase", f_max))
    ) %>%
    gather_("Axis", "Estimate", c("X", "Y"))
}
