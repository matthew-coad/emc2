

#' Create parameters for an emc experiment
#'
#' Adds all arguments as a parameter.
#'
#' @export
emc_parameters <- function(..., name = NULL) {
  # args <- rlang::list2(...)
  # vector_args <- purrr::discard(args, is.data.frame)
  # frame_args <- purrr::keep(args, is.data.frame)
  # tibble::tibble(!!! vector_args)
  params <- tibble::as_tibble(dplyr::bind_cols(...))
  name <- rlang::enquo(name)
  if (rlang::quo_is_null(name)) {
    param_names <- lapply(names(params), function(n) rlang::sym(n))
    name <- quo(paste(!!! param_names, sep = ", "))
  }
  params <- dplyr::mutate(params, name = !! name)
  params <- dplyr::select(params, name, tidyselect::everything())
  params
}
