#' Runs a training experiment
#'
#' Given an experiment frame, trains a model for each row and extends it to the experiment frame
#'
#' @return Experiment frame extended with the model recording
#' @export
emc_train <- function(parameters, recipe, data, method, metric, trControl, verbose = TRUE, cache_path = NULL) {

  stopifnot(is.data.frame(parameters))

  recipe_quo <- rlang::enquo(recipe)
  data_quo <- rlang::enquo(data)
  method_quo <- rlang::enquo(method)
  metric_quo <- rlang::enquo(metric)
  trControl_quo <- rlang::enquo(trControl)
  verbose_quo <- rlang::enquo(verbose)
  cache_path_quo <- rlang::enquo(cache_path)

  record_quo <-
    rlang::quo(emc_record(
          function() caret::train(!! recipe_quo, data = !! data_quo, method = !! method_quo,
                                  metric = !! metric_quo, trControl = !! trControl_quo),
          label = sprintf("Train %s", name), verbose = !! verbose_quo, cache_path = !! cache_path_quo))

  eval_row <- function(idx) {
    eval_data <- parameters[idx, ] %>% purrr::map(~ first(.))
    value <- rlang::eval_tidy(record_quo, data = eval_data)
    value
  }

  rows <- purrr::map(rlang::seq2(1, nrow(parameters)), eval_row) %>% dplyr::bind_rows()
  r <- dplyr::bind_cols(parameters, rows)
  r
}

#' @export
emc_performance <- function(train) {

  stopifnot(is.data.frame(train))

  eval_row <- function(idx) {
    errors <- train[idx, ] %>% pull(errors) %>% first()
    if (errors > 0) {
      return (tibble::tibble(Idx = idx))
    }
    train_i <- train[idx, ] %>% pull(result) %>% first()

    caret_performance <- caret::getTrainPerf(train_i) %>% dplyr::select(-method)

    cols <- colnames(caret_performance)
    out_cols <- tolower(gsub("^Train", "", cols))

    vars <- purrr::map2(cols, out_cols, ~ rlang::list2(!! .y := sym(.x))) %>% rlang::flatten()
    performance <- caret_performance %>% dplyr::mutate(Idx = idx) %>% dplyr::rename(!!!vars )
    performance
  }

  rows <- purrr::map(rlang::seq2(1, nrow(train)), eval_row) %>% bind_rows()
  r <- train %>% dplyr::select(-result, -log) %>% dplyr::bind_cols(rows) %>% dplyr::select(-Idx)
  r
}

#' @export
emc_resamples <- function(train, name = name) {
  name_quo <- rlang::enquo(name)
  stopifnot(is.data.frame(train))
  successful_train <- train %>% dplyr::filter(errors == 0)

  eval_name <- function(idx) {
    eval_data <- train[idx, ] %>% purrr::map(~ first(.))
    name <- rlang::eval_tidy(name_quo, data = eval_data)
    name
  }
  names <- purrr::map_chr(rlang::seq2(1, nrow(train)), eval_name)
  successful_train$result %>% purrr::set_names(names) %>% caret::resamples()
}


#' @export
emc_resamples_data <- function(train) {

  stopifnot(is.data.frame(train))

  successful_train <- train %>% dplyr::filter(errors == 0)
  successful_train$Idx = rlang::seq2(1, nrow(successful_train))

  eval_row <- function(idx) {

    train_i <- successful_train[idx, ] %>% pull(result) %>% first()
    train_resample <- train_i$resample

    cols <- colnames(train_resample)
    out_cols <- tolower(cols)

    vars <- purrr::map2(cols, out_cols, ~ rlang::list2(!! .y := sym(.x))) %>% rlang::flatten()
    resample <- train_resample %>% dplyr::mutate(Idx = idx) %>% dplyr::rename(!!!vars) %>% dplyr::select(resample, everything())
    resample
  }

  rows <- purrr::map(rlang::seq2(1, nrow(successful_train)), eval_row) %>% bind_rows()
  r <-
    successful_train %>%
    inner_join(rows, by = "Idx") %>% select(-result, -log, -errors, -error, -messages, -warnings, -duration, -Idx)
  r
}



