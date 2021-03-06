% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/training.R
\name{emc_train}
\alias{emc_train}
\title{Runs a training experiment}
\usage{
emc_train(parameters, recipe, data, method, metric, trControl,
  verbose = TRUE, cache_path = NULL)
}
\value{
Experiment frame extended with the model recording
}
\description{
Given an experiment frame, trains a model for each row and extends it to the experiment frame
}
