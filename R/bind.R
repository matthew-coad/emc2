
#' @export
emc_bind <- function(.x, ...) {
  stopifnot(is.data.frame(.x))
  quos <- rlang::enquos(...)

  eval_row_quo <- function(x, quo, idx) {
    value <- rlang::eval_tidy(quo, data = x[idx,])
    value
  }

  x <- .x
  idx <- rlang::seq2(1, nrow(x))
  for (quo in quos) {
    rows <- purrr::map(idx, eval_row_quo, x = x, quo = quo)
    quo_df <- dplyr::bind_rows(!!! rows)
    x <- dplyr::bind_cols(x, quo_df)
  }
  x
}
