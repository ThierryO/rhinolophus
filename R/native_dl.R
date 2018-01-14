#' Create prediction with a naive deep learning model
#' @inheritParams connect_db
#' @export
#' @importFrom RSQLite dbGetQuery dbWriteTable dbDisconnect
#' @importFrom dplyr %>% transmute count filter semi_join mutate select anti_join group_by
#' @importFrom rlang .data
#' @importFrom tidyr spread gather
#' @importFrom keras keras_model_sequential layer_dense layer_dropout compile optimizer_rmsprop fit predict_proba
naive_dl <- function(path) {
  x <- db2ml(path)
  connection <- connect_db(path)
  y <- dbGetQuery(
    conn = connection, "
    SELECT
      c.pulse, s.abbreviation AS species, a.abbreviation AS activity
    FROM
          manual AS m
        INNER JOIN
          species AS s
        ON
          m.species = s.id
      INNER JOIN
        contour AS c
      ON
        m.contour = c.id
    LEFT JOIN
      activity AS a
    ON
      m.activity = a.id"
  ) %>%
    transmute(
      .data$pulse,
      class = ifelse(
        is.na(.data$activity),
        .data$species,
        paste(.data$species, .data$activity, sep = "_")
      )
    )
  y0 <- y %>%
    count(.data$class) %>%
    filter(.data$n > 10) %>%
    semi_join(x = y, by = "class")
  if (nrow(y0) < 100) {
    stop("less that 100 observations")
  }
  if (length(unique(y0$class)) < 2) {
    stop("less than 2 classes with sufficient observations")
  }
  y_train <- y0 %>%
    mutate(junk = 1) %>%
    spread(.data$class, .data$junk, fill = 0) %>%
    select(-.data$pulse) %>%
    as.matrix()
  x_train <- x %>%
    semi_join(y0, by = "pulse") %>%
    select(-.data$pulse) %>%
    as.matrix()
  x_predict <- x %>%
    anti_join(y0, by = "pulse") %>%
    select(-.data$pulse) %>%
    as.matrix()
  x_predict_pulse <- x %>%
    select(.data$pulse) %>%
    anti_join(y0, by = "pulse")
  nodes <- seq(
    ceiling(log(ncol(x_train))),
    ceiling(log(ncol(y_train))),
    length = 4
  )
  nodes <- ceiling(exp(nodes))

  model <- keras_model_sequential() %>%
    layer_dense(units = 50, activation = 'relu', input_shape = ncol(x_train)) %>%
    layer_dropout(rate = 0.1) %>%
    layer_dense(units = 30, activation = 'relu') %>%
    layer_dropout(rate = 0.1) %>%
    layer_dense(units = ncol(y_train), activation = 'softmax') %>%
    compile(
      loss = "categorical_crossentropy",
      optimizer = optimizer_rmsprop(),
      metrics = c("accuracy")
    )
  model %>% fit(
    x_train, y_train,
    epochs = 50,
    validation_split = 0.2
  )
  prob <- predict_proba(model, x_predict)
  colnames(prob) <- colnames(y_train)
  prediction <- cbind(x_predict_pulse, prob) %>%
    gather("class", "probability", -.data$pulse) %>%
    group_by(.data$pulse) %>%
    filter(.data$probability >= max(.data$probability) - 0.1) %>%
    ungroup()
  dbWriteTable(conn = connection, "prediction", prediction, overwrite = TRUE)
  dbDisconnect(connection)
  return(invisible(NULL))
}
