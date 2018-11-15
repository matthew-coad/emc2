## Algorithms ------------------------

#' List emc algorithms
#'
#' \code{emc_algorithms} lists all the algorithms available to emc.
#'
#' As well as the algorithm it lists a set of tags useful for filtering the
#' list of algorithms.
#'
#' @param uninstalled If TRUE list include uninstalled algorithms
#' @return A dataframe listing available
#'
#' @importFrom magrittr %>%
#' @export
emc_algorithms <- function(uninstalled = FALSE) {

  core_algorithms <-
    c("lm", "glm", "Penalized Linear Model" = "glmnet", "pls", "pcr", "enet", "lars", "lda", "plr", "polr",
      "nnet", "earth", "svmLinear", "svmPoly", "svmRadial", "knn", "qda", "nb",
      "rpart", "M5", "M5Rules",  "treebag", "rf", "cubist", "ada", "eXtreme Gradient Boosting - Linear" = "xgbLinear", "eXtreme Gradient Boosting - Tree" = "xgbTree",
      "null", "OneR", "bagEarth", "J48", "LMT", "LogitBoost")


  core_names <- names(core_algorithms) %>% purrr::keep(~ . != "")
  label_overrides <- core_names %>% purrr::set_names(core_names %>% purrr::map(~ core_algorithms[.]))
  overridden_label <- function(algorithm, label) {
    dplyr::if_else(!is.na(label_overrides[algorithm]),label_overrides[algorithm],label) %>% purrr::set_names(NULL)
  }

  installed_packages <- utils::installed.packages() %>% tibble::as_tibble() %>% dplyr::pull(Package)
  model_installed <- function(info) {
    sum(info$library %in% installed_packages) == length(info$library)
  }

  infos <- caret::getModelInfo()
  algorithms <- names(infos)
  installed <- infos %>% purrr::map_lgl(model_installed)

  labels <- infos %>% purrr::map_chr("label")
  overridden_labels <- purrr::map2_chr(algorithms, labels, overridden_label)
  regression <- infos %>% purrr::map_lgl(~ "Regression" %in% .$type)
  classification <- infos %>% purrr::map_lgl(~ "Classification" %in% .$type)
  core <- algorithms %in% core_algorithms
  linear <- infos %>% purrr::map_lgl(~ "Linear Regression" %in% .$tags || "Linear Classifier" %in% .$tags)
  two_class_only <- infos %>% purrr::map_lgl(~ "Two Class Only" %in% .$tags)
  two_class <- classification
  multi_class <- classification & !two_class_only
  handle_missing <- infos %>% purrr::map_lgl(~ "Handle Missing Predictor Data" %in% .$tags)
  tree_based <- infos %>% purrr::map_lgl(~ "Tree-Based Model" %in% .$tags)
  rule_based <- infos %>% purrr::map_lgl(~ "Rule-Based Model" %in% .$tags)
  support_vector_machine <- infos %>% purrr::map_lgl(~ "Support Vector Machines" %in% .$tags)
  neural_network <- infos %>% purrr::map_lgl(~ "Neural Network" %in% .$tags)
  case_weights <- infos %>% purrr::map_lgl(~ "Accepts Case Weights" %in% .$tags)
  feature_selection <- infos %>% purrr::map_lgl(~ "Implicit Feature Selection" %in% .$tags)

  boosting <- infos %>% purrr::map_lgl(~ "Boosting" %in% .$tags)
  ensemble <- infos %>% purrr::map_lgl(~ "Ensemble Model" %in% .$tags)
  bagging <- infos %>% purrr::map_lgl(~ "Bagging" %in% .$tags)
  generalized_linear_model <- infos %>% purrr::map_lgl(~ "Generalized Linear Model" %in% .$tags)
  ordinal_outcomes <- infos %>% purrr::map_lgl(~ "Ordinal Outcomes" %in% .$tags)

  mars <- infos %>% purrr::map_lgl(~ "Multivariate Adaptive Regression Splines" %in% .$tags)
  discriminant_analysis  <- infos %>% purrr::map_lgl(~ "Discriminant Analysis" %in% .$tags)
  model_tree  <- infos %>% purrr::map_lgl(~ "Model Tree" %in% .$tags)
  bayesian_model <- infos %>% purrr::map_lgl(~ "Bayesian Model" %in% .$tags)
  logistic_regression <- infos %>% purrr::map_lgl(~ "Logistic Regression" %in% .$tags)
  L1_regularization <- infos %>% purrr::map_lgl(~ "L1 Regularization" %in% .$tags)
  L2_regularization <- infos %>% purrr::map_lgl(~ "L2 Regularization" %in% .$tags)

  robust_model <- infos %>% purrr::map_lgl(~ "Robust Model" %in% .$tags)
  partial_least_squares <- infos %>% purrr::map_lgl(~ "Partial Least Squares" %in% .$tags)
  random_forest <- infos %>% purrr::map_lgl(~ "Random Forest" %in% .$tags)
  gaussian_process <- infos %>% purrr::map_lgl(~ "Gaussian Process" %in% .$tags)

  quick <- !(boosting | ensemble | bagging | model_tree | neural_network | support_vector_machine | gaussian_process)

  known_tags <- c("Linear Regression", "Linear Classifier", "Two Class Only",
                  "Handle Missing Predictor Data",  "Accepts Case Weights", "Implicit Feature Selection",
                  "Tree-Based Model", "Rule-Based Model", "Support Vector Machines", "Neural Network", "Boosting",
                  "Ensemble Model", "Generalized Linear Model", "Multivariate Adaptive Regression Splines", "Discriminant Analysis",
                  "Model Tree", "Bayesian Model", "Logistic Regression",
                  "L1 Regularization", "L2 Regularization", "Ordinal Outcomes", "Bagging", "Robust Model", "Partial Least Squares", "Random Forest",
                  "Gaussian Process")

  tags <- infos %>% purrr::map_chr(~ paste(.$tags[!.$tags %in% known_tags], collapse = ", "))

  r <- tibble::tibble(
    algorithm = algorithms,
    label = overridden_labels,
    installed = installed,
    regression = regression,
    classification = classification,
    two_class = two_class,
    multi_class = multi_class,
    core = core,
    quick = quick,
    ordinal_outcomes = ordinal_outcomes,
    feature_selection = feature_selection,
    case_weights  = case_weights,
    handle_missing = handle_missing,
    linear = linear,
    tree_based  = tree_based,
    rule_based = rule_based,
    boosting = boosting,
    ensemble = ensemble,
    bagging = bagging,
    L1_regularization = L1_regularization,
    L2_regularization  = L2_regularization,
    robust_model = robust_model,

    model_tree = model_tree,
    logistic_regression = logistic_regression,
    partial_least_squares = partial_least_squares,
    random_forest = random_forest,
    svm = support_vector_machine,
    neural_net = neural_network,
    discriminant_analysis = discriminant_analysis,
    bayesian_model = bayesian_model,
    generalized_linear_model = generalized_linear_model,
    mars = mars,
    gaussian_process = gaussian_process,
    tags = tags
  )

  if (!uninstalled) {
    r <- r %>% dplyr::filter(installed)
  }
  r
}

#' List emc algorithms
#'
#' @importFrom magrittr %>%
emc_algorithm_tags <- function(algorithms) {
  infos <- caret::getModelInfo()
  info_names <- names(infos)
  info_tags <- function(info, name) {
    info$tags %>% purrr::map(~ list(algorithm = name, tag = .)) %>% dplyr::bind_rows()
  }
  tags <- purrr::map2(infos, info_names, info_tags) %>% dplyr::bind_rows()
  tibble::tibble(algorithm = algorithms) %>% dplyr::select(algorithm) %>% dplyr::inner_join(tags, by = "algorithm")
}

#' Given a vector of algorithms, return all the libraries needed by those algorithms
#'
#' @param algorithms Algorithms to list libraries for
#' @param uninstalled_only If TRUE only uninstalled libraries are returned.
#' @return Vector of library names
#'
#' @importFrom magrittr %>%
#' @export
emc_algorithm_libraries <- function(algorithms, uninstalled_only = TRUE) {

  installed_packages <- utils::installed.packages() %>% tibble::as_tibble() %>% dplyr::pull(Package)

  algorithm_libraries <- function(code) {
    libraries <- caret::getModelInfo(code, regex = FALSE)[[1]]$library
  }
  libraries <- algorithms %>% purrr::map(algorithm_libraries) %>% unlist() %>% unique()
  if (uninstalled_only) {
    libraries <- libraries[!libraries %in% installed_packages]
  }
  libraries
}

#' Generates library installation script
#'
#' Given a number of algorithms, returns a script that will install the
#' packages needed by those algorithms.
#'
#' This function makes it easy to restart R and install the required packages by
#' copying the output to the clipboard.
#'
#' @export
emc_install_algorithms_script <- function(algorithms, uninstalled_only = TRUE) {
  libraries <- emc_algorithm_libraries(algorithms, uninstalled_only = uninstalled_only)
  if (length(libraries) == 0) {
    stop("Found no packages to install")
  }
  sprintf('install.packages(c("%s"))', paste(libraries, collapse = '", "'))
}

