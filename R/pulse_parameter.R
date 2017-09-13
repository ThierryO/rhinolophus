#' Convert pulses into parameters
#' @param pulses a SpatialPolygonsDataFrame with pulses
#' @export
#' @importFrom dplyr %>% mutate_ group_by_ slice_ filter_ summarise_ filter_ inner_join do_ n
#' @importFrom sp coordinates
#' @importFrom tidyr unnest_
pulse_parameter <- function(pulses) {
  pulse <- pulses@data
  pulse$xy <- lapply(
    pulses@polygons,
    function(x){
      x@Polygons[[1]] %>%
        coordinates() %>%
        data.frame()
    }
  )
  pulse <- unnest_(pulse, ~xy) %>%
    mutate_(
      X1 = ~X1 - X,
      X2 = ~X2 - Y
    ) %>%
    group_by_(~X, ~Y, ~level) %>%
    slice_(-1) %>%
    mutate_(
      row_id = ~seq_along(level),
      n = ~n()
    )
  pulse %>%
    filter_(~X1 < 0) %>%
    summarise_(
      start = ~which.min(abs(X2))
    ) %>%
    inner_join(x = pulse, by = c("X", "Y", "level")) %>%
    mutate_(row_id = ~ ((row_id - start) %% n) * 2 / n) %>%
    do_(
      PeakAmplitude = ~ unique(.$PeakAmplitude),
      Fourier = ~elliptic_fourier(.)
    ) %>%
    unnest_(~Fourier)
}
