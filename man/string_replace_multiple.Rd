% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string_.R
\name{string_replace_multiple}
\alias{string_replace_multiple}
\title{Replace Multiple Patterns in a Text Column}
\usage{
string_replace_multiple(text_column, replacements_list)
}
\arguments{
\item{text_column}{A text column in which replacements will be made.}

\item{replacements_list}{A list of named vectors where names are the old patterns and values are the new replacements.}
}
\value{
A text column with specified patterns replaced.
}
\description{
This function replaces multiple patterns in a text column based on a replacements list.
}
