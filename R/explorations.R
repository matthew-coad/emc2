## -----------------------------------
## Exploration functions
## -----------------------------------

#' @export
emc_data_statistics <- function(.x) {
  rows <- nrow(.x)
  cols <- ncol(.x)
  tibble::tibble(rows = rows, cols = cols)
}

#' @export
emc_variable_distribution <- function(.x) {

  data <- .x
  var_names <- colnames(data)
  classes <- data %>% purrr::map(class)
  types <- classes %>% purrr::map_chr(~dplyr::first(.))
  idxs <- 1:length(var_names)
  count <- rep(TRUE, length(var_names))
  distribution <- classes %>% purrr::map_lgl(~"integer" %in% . || "numeric" %in% .)

  mins <- idxs %>% purrr::map_dbl(function (idx) { if (distribution[idx]) min(data[[idx]], na.rm = TRUE) else NA})
  means <- idxs %>% purrr::map_dbl(function (idx) { if (distribution[idx]) mean(data[[idx]], na.rm = TRUE) else NA})
  medians <- idxs %>% purrr::map_dbl(function (idx) { if (distribution[idx]) median(data[[idx]], na.rm = TRUE) else NA})
  maxes <- idxs %>% purrr::map_dbl(function (idx) { if (distribution[idx]) max(data[[idx]], na.rm = TRUE) else NA})
  firstQ <- idxs %>% purrr::map_dbl(function (idx) { if (distribution[idx]) quantile(data[[idx]], probs = c(0.25), na.rm = TRUE) else NA})
  thirdQ <- idxs %>% purrr::map_dbl(function (idx) { if (distribution[idx]) quantile(data[[idx]], probs = c(0.75), na.rm = TRUE) else NA})
  sds <- idxs %>% purrr::map_dbl(function (idx) { if (distribution[idx]) sd(data[[idx]], na.rm = TRUE) else NA})
  skews <- idxs %>% purrr::map_dbl(function (idx) { if (distribution[idx]) e1071::skewness(data[[idx]], na.rm = TRUE) else NA})

  tibble::tibble(variable = var_names, type = types, mean = means, min = mins, firstQ = firstQ, median = medians, thirdQ = thirdQ, max = maxes, sd = sds, skew = skews)
}

#' @export
emc_variable_degeneracy <- function(.x) {

  data <- .x
  var_names <- colnames(data)
  classes <- data %>% purrr::map(class)
  types <- classes %>% purrr::map_chr(~dplyr::first(.))

  continuous <- classes %>% purrr::map_lgl(~"numeric" %in% .)
  discrete <- classes %>% purrr::map_lgl(~"integer" %in% .)
  quantative <- classes %>% purrr::map_lgl(~"numeric" %in% . || "integer" %in% .)
  categorical <- classes %>% purrr::map_lgl(~"factor" %in% .)
  qualitative <- classes %>% purrr::map_lgl(~"factor" %in% .)
  two_class <- classes %>% purrr::map_lgl(~"factor" %in% . && length(levels(.)) == 2)
  multi_class <- classes %>% purrr::map_lgl(~"factor" %in% . && length(levels(.)) > 2)

  idxs <- 1:length(var_names)
  count <- rep(TRUE, length(var_names))
  classes <- categorical | discrete
  distribution <- continuous | discrete
  vals <- idxs %>% purrr::map_int(function(idx) { if (count[idx]) sum(!is.na(data[[idx]])) else NA})
  nas <- idxs %>% purrr::map_int(function(idx) { if (count[idx]) sum(is.na(data[[idx]])) else NA})
  zeros <- idxs %>% purrr::map_int(function(idx) { if (count[idx]) sum(!is.na(data[[idx]]) & data[[idx]] == 0) else NA})
  uniqs <- idxs %>% purrr::map_int(function(idx) { if (classes[idx]) length(unique(data[[idx]])) else NA})
  vars <- idxs %>% purrr::map_dbl(function (idx) { if (distribution[idx]) stats::var(data[[idx]]) else NA})

  nearZeroIdx <- caret::nearZeroVar(data)
  nearZeroVar <- if (!purrr::is_empty(nearZeroIdx)) idxs %in% nearZeroIdx else rep(FALSE, length(var_names))

  tibble::tibble(variable = var_names, type = types, vals = vals, nas = nas, zero = zeros, unique = uniqs, variance = vars, nearZeroVar = nearZeroVar)
}

#' @export
emc_level_statistics <- function(.x) {

  data <- .x
  variable_names <- colnames(data)
  classes <- data %>% purrr::map(class)
  types <- classes %>% purrr::map_chr(~dplyr::first(.))
  factor_variables <- classes %>% purrr::map_lgl(~"factor" %in% .)

  level_statistics <- function(i) {
    x <- data[[i]]
    x_levels <- levels(x)
    variable_name <- variable_names[[i]]

    counts <- x_levels %>% purrr::map_int(~sum(is.finite(x) & x == .))
    freqs <- x_levels %>% purrr::map_dbl(~sum(is.finite(x) & x == .) / length(x))
    nacount <- sum(is.na(x))
    nafreq <- sum(is.na(x)) / length(x)

    statistics <- tibble::tibble(variable = variable_name, level = x_levels, count = counts, freqs = freqs)
    statistics <- tibble::add_row(statistics, variable = variable_name, level = ".NA", count = nacount, freqs = nafreq)
    statistics
  }

  which(factor_variables) %>% purrr::map_df(level_statistics)
}
