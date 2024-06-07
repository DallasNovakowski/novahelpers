#' Fuzzy Abbreviation Function
#'
#' This script provides a function for applying fuzzy matching and replacements to abbreviate text based on a predefined corrections data frame.
#'
#' @section Functions:
#' \itemize{
#'   \item \code{fuzzy_abbreviate} - Applies fuzzy matching and replacements to abbreviate text.
#' }
#' 
#' @name fuzzy_abbreviation_function
NULL

#' Apply Fuzzy Matching and Replacements to Abbreviate Text
#'
#' This function applies fuzzy matching and replacements to abbreviate text based on a predefined corrections data frame.
#'
#' @param text A character vector containing the text to be abbreviated.
#' @param corrections A data frame containing the original text and its corresponding replacement values.
#' @return A character vector with abbreviated text.
#' @export
fuzzy_abbreviate <- function(text, corrections) {
  match <- fuzzyjoin::stringdist_inner_join(data.frame(text = text, stringsAsFactors = FALSE), 
                                            corrections, 
                                            by = c("text" = "original"), 
                                            method = "jw", 
                                            max_dist = 0.2)
  
  if(nrow(match) > 0) {
    return(match$replacement[1])
  } else {
    return(text)
  }
}
