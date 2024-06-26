% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anonymize_names.R
\name{anonymize_names}
\alias{anonymize_names}
\title{Anonymize Names in a Sentence}
\usage{
anonymize_names(sentence, names, whitelist)
}
\arguments{
\item{sentence}{A string representing the sentence to be sanitized.}

\item{names}{A vector of names to be anonymized.}

\item{whitelist}{A vector of words that should not be anonymized.}
}
\value{
A sanitized sentence with names anonymized.
}
\description{
This function sanitizes occurrences of names within a given sentence.
}
