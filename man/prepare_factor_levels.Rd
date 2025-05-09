% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_helpers.R
\name{prepare_factor_levels}
\alias{prepare_factor_levels}
\title{Prepare Factor Levels}
\usage{
prepare_factor_levels(summary_data, raw_data, x_var)
}
\arguments{
\item{summary_data}{A data frame containing the summary statistics.}

\item{raw_data}{A data frame containing the original raw data.}

\item{x_var}{A character string specifying the variable to order.}
}
\value{
A named list with elements \code{summary_data} and \code{raw_data} that
have been re-ordered.
}
\description{
Reorders the factor levels in a summary data frame based on the \code{mean} column,
and then applies that ordering to the raw data.
}
