% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/algorithms.R
\name{emc_install_algorithms_script}
\alias{emc_install_algorithms_script}
\title{Generates library installation script}
\usage{
emc_install_algorithms_script(algorithms, uninstalled_only = TRUE)
}
\description{
Given a number of algorithms, returns a script that will install the
packages needed by those algorithms.
}
\details{
This function makes it easy to restart R and install the required packages by
copying the output to the clipboard.
}
