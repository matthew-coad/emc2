library(recipes)
library(caret)
library(emc2)


quick_spot_algorithms <- c("knn", "lm")
quick_spot_parameters <- emc_parameters(algorithm = quick_spot_algorithms, name = sprintf("Quick spot '%s'", algorithm))
quick_spot_recipe <- recipe(Species ~ ., iris)
quick_spot_metric <- "Accuracy"
quick_spot_control <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

# prior_quick_spot_emc <- quick_spot_emc

quick_spot_train <- emc_train(quick_spot_parameters, quick_spot_recipe, iris, algorithm, quick_spot_metric, quick_spot_control)
emc_performance(quick_spot_train)

# quick_spot_emc %>% dplyr::select(name, train_error, train_warnings, train_duration)
