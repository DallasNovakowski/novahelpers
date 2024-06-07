#' Replace Contractions in Text
#'
#' This function replaces contractions in text with their expanded forms. It also cleans special apostrophe characters before conducting replacements.
#'
#' @param text A character vector containing the text to be processed.
#' @param replacements A named character vector specifying the contractions and their expanded forms.
#' @return A character vector with contractions replaced by their expanded forms.
#' @export
replace_contractions <- function(text, replacements) {
  # Clean special apostrophes
  text <- gsub("’", "'", text, ignore.case = TRUE)
  
  # Replace contractions
  for (pattern in names(replacements)) {
    text <- gsub(pattern, replacements[pattern], text)
  }
  
  return(text)
}

# Define replacements for contractions
replacements <- c("weren't" = "were not",
                  "Weren't" = "Were not",
                  "don’t" = "do not",
                  "Don’t" = "Do not",
                  "don't" = "do not",
                  "Don't" = "Do not",
                  "haven't" = "have not",
                  "Haven't" = "Have not",
                  "weren\'t" = "were not",
                  "Weren\'t" = "Were not",
                  "don\'t" = "do not",
                  "Don\'t" = "Do not",
                  "haven\'t" = "have not",
                  "Haven\'t" = "Have not")
