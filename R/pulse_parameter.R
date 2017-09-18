#' Convert pulses into parameters
#' @param pulses a SpatialPolygonsDataFrame with pulses
#' @export
#' @importFrom dplyr %>% mutate_ group_by_ slice_ filter_ summarise_ filter_ inner_join do_ n bind_rows
#' @importFrom sp coordinates
#' @importFrom tidyr unnest_
pulse_parameter <- function(pulses) {
  pulse <- lapply(
    seq_along(pulses@Plot),
    function(i){
      pulses@Plot@polygons[[i]]@Polygons[[1]] %>%
        coordinates() %>%
        data.frame() %>%
        mutate_(ID = ~pulses@Plot$ID[i])
    }
  ) %>%
    bind_rows() %>%
    inner_join(
      pulses@Contour %>%
        select_(~ID, ~Fingerprint, ~Pulse),
      by = "ID"
    ) %>%
    inner_join(
      pulses@Pulse %>%
        select_(~ID, ~PeakX, ~PeakY),
      by = c("Pulse" = "ID")
    ) %>%
    mutate_(
      X1 = ~X1 - PeakX,
      X2 = ~X2 - PeakY
    ) %>%
    group_by_(~ID) %>%
    slice_(-1) %>%
    mutate_(
      RowID = ~seq_along(ID),
      N = ~n()
    )
  new(
    "batPulseParameter",
    Recording = pulses@Recording,
    Spectrogram = pulses@Spectrogram,
    Pulse = pulses@Pulse,
    Contour = pulses@Contour,
    Parameter = pulse %>%
      filter_(~X1 < 0) %>%
      summarise_(
        Start = ~which.min(abs(X2))
      ) %>%
      inner_join(x = pulse, by = "ID") %>%
      mutate_(RowID = ~ ((RowID - Start) %% N) * 2 / N) %>%
      do_(
        Fourier = ~elliptic_fourier(.)
      ) %>%
      unnest_(~Fourier)
  )
}
