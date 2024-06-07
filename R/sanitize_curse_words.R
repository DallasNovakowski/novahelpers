#' Curse Word Sanitization Function
#'
#' This script provides a function for sanitizing curse words in a given text by replacing them with a designated placeholder.
#'
#' @section Functions:
#' \itemize{
#'   \item \code{sanitize_curse_words} - Sanitizes curse words in a given text.
#' }
#' 
#' @name curse_word_sanitization_function
NULL

#' Sanitize Curse Words in Text
#'
#' This function sanitizes curse words in a given text by replacing them with a designated placeholder.
#'
#' @param text A character vector containing the text to be sanitized.
#' @param curse_words A character vector containing the curse words to be replaced.
#' @return A character vector with curse words replaced by the placeholder.
#' @export
sanitize_curse_words <- function(text, curse_words) {
  for (word in curse_words) {
    pattern <- paste0("\\b", word, "\\b")
    text <- gsub(pattern, "BAD_WORD", text, ignore.case = TRUE, perl = TRUE)
  }
  return(text)
}
