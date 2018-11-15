
#' Records a function
#'
#' Executes an function  recording all messages, warnings and errors along the way.
#'
#' @return A tibble that contains the result, error, execution times and the replay log.
#' @export
emc_record <- function(f, label = NULL, verbose = TRUE, cache_path = NULL) {

  if (!is.null(cache_path) && file.exists(cache_path)) {

    if (!verbose && !is.null(label)) {
      message(">>>> Cached ", label, " <<<<")
    }

    return (readRDS(cache_path))
  }

  result <- list(NULL)
  succeded <- FALSE
  started <- lubridate::now()
  handler <- evaluate::new_output_handler(
    value = function(v) {
      result <<- v
      succeded <<- TRUE
    }
  )

  if (!verbose && !is.null(label)) {
    message(">>>> ", label, " <<<<")
  }

  eval_quo <- function() {

    if (!is.null(label)) {
      message(">>>> ", label, " <<<<")
    }

    result <- list(f())
    evaluate::flush_console()
    result
  }

  log <- evaluate::evaluate(eval_quo, output_handler = handler, stop_on_error = 1)
  finished <- lubridate::now()
  messages <- log %>% purrr::keep(evaluate::is.message) %>% length()
  if (!is.null(label)) {
    messages <- messages - 1
  }
  warnings <- log %>% purrr::keep(evaluate::is.warning) %>% length()
  errors <- log %>% purrr::keep(evaluate::is.error) %>% length()
  first_error <- log %>% purrr::keep(evaluate::is.error) %>% dplyr::first()
  if (!is.null(first_error)) {
    error <- conditionMessage(first_error)
  }
  else {
    error <- ""
  }
  recording <- list()
  recording[["result"]] <- result
  recording[["log"]] <- list(log)
  recording[["error"]] = error
  recording[["errors"]] = errors
  recording[["messages"]] = messages
  recording[["warnings"]] = warnings
  recording[["duration"]] = as.numeric(difftime(finished, started, units = "secs"))

  if (verbose) {
    evaluate::replay(purrr::discard(log, evaluate::is.source))
  }
  final <- tibble::tibble(!!! recording)
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), showWarnings = FALSE)
    saveRDS(final, cache_path)
  }
  final
}

#' @export
emc_replay <- function(.log) {
  for (log in .log) {
    evaluate::replay(purrr::discard(log, evaluate::is.source))
  }
  invisible()
}

