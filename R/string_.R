#' String Manipulation Functions for Variable Names, Wordclouds, and Ngrams
#'
#' This collection of functions provides various utilities for handling and manipulating strings. They are primarily 
#' designed for tasks such as renaming variables, cleaning text data, and preparing data for wordclouds and ngram analysis.
#'
#' @section Functions:
#' \itemize{
#'   \item \code{string_concatenate} - Concatenates multiple strings into one string.
#'   \item \code{string_coalesce} - Concatenates multiple strings in a row-wise manner, ignoring NA values.
#'   \item \code{string_rename_columns} - Renames columns in a dataframe based on a replacements list.
#'   \item \code{string_drop_columns_containing} - Drops columns containing a specified string from a dataframe.
#'   \item \code{string_remove_between_parentheses} - Removes text between parentheses from a string.
#'   \item \code{string_replace_multiple} - Replaces multiple patterns in a text column based on a replacements list.
#'   \item \code{string_drop_unsure} - Removes ", unsure" from a string.
#'   \item \code{string_drop_before_unsure} - Removes text before "unsure" in a string.
#'   \item \code{string_remove_unwanted} - Removes unwanted symbols from a string.
#'   \item \code{string_tokenize_data} - Tokenizes text data for wordcloud and ngram analysis.
#' }
#' 
#' @name string_functions
NULL

#' Concatenate Multiple Strings
#'
#' This function concatenates multiple strings into a single string.
#'
#' @param ... Multiple strings to concatenate.
#' @return A single concatenated string.
#' @export
string_concatenate <- function(...) {
  paste(..., collapse = "")
}

#' Coalesce Multiple Strings
#'
#' This function concatenates multiple strings in a row-wise manner, ignoring NA values.
#'
#' @param ... Multiple strings to coalesce.
#' @return A single concatenated string for each row, with NA values ignored.
#' @export
string_coalesce <- function(...) {
  apply(cbind(...), 1, function(x) {
    result <- paste(x[!is.na(x)], collapse = ", ")
    if (length(result) == 0) {
      return(NA)
    } else {
      return(result)
    }
  })
}

#' Rename Columns in a Dataframe
#'
#' This function renames columns in a dataframe based on a replacements list.
#'
#' @param data A dataframe whose columns are to be renamed.
#' @param replacements A named list of replacements where names are the patterns to replace and values are the replacements.
#' @return A dataframe with renamed columns.
#' @export
string_rename_columns <- function(data, replacements) {
  for (string in names(replacements)) {
    names(data) <- sub(string, replacements[string], names(data))
  }
  return(data)
}

#' Drop Columns Containing a Specified String
#'
#' This function drops columns from a dataframe that contain a specified string in their names.
#'
#' @param data A dataframe from which columns will be dropped.
#' @param string_to_drop A string pattern to match column names to drop.
#' @return A dataframe with specified columns dropped.
#' @export
string_drop_columns_containing <- function(data, string_to_drop) {
  columns_to_drop <- grep(string_to_drop, names(data))
  cleaned_data <- data[, -columns_to_drop, drop = FALSE]
  return(cleaned_data)
}

#' Remove Text Between Parentheses
#'
#' This function removes text between parentheses in a given string.
#'
#' @param input_string A string from which text between parentheses will be removed.
#' @return A cleaned string with text between parentheses removed.
#' @export
string_remove_between_parentheses <- function(input_string) {
  pattern <- "\\s*\\(.*?\\)"
  cleaned_string <- gsub(pattern, "", input_string)
  return(cleaned_string)
}

#' Replace Multiple Patterns in a Text Column
#'
#' This function replaces multiple patterns in a text column based on a replacements list.
#'
#' @param text_column A text column in which replacements will be made.
#' @param replacements_list A list of named vectors where names are the old patterns and values are the new replacements.
#' @return A text column with specified patterns replaced.
#' @export
string_replace_multiple <- function(text_column, replacements_list) {
  for (replacements in replacements_list) {
    text_column <- str_replace_all(text_column, setNames(replacements$new, replacements$old))
  }
  return(text_column)
}

#' Drop "Unsure" From Strings
#'
#' This function removes ", unsure" from a string.
#'
#' @param x A string from which ", unsure" will be removed.
#' @return A string with ", unsure" removed.
#' @export
string_drop_unsure <- function(x) {
  sub(",\\s*unsure", "", x)
}

#' Drop Text Before "Unsure" in Strings
#'
#' This function removes text before "unsure" in a string.
#'
#' @param x A string from which text before "unsure" will be removed.
#' @return A string with text before "unsure" removed.
#' @export
string_drop_before_unsure <- function(x) {
  sub(".*unsure", "", x)
}

#' Remove Unwanted Symbols From Strings
#'
#' This function removes unwanted symbols from a string.
#'
#' @param input_string A string from which unwanted symbols will be removed.
#' @return A cleaned string with unwanted symbols removed.
#' @export
string_remove_unwanted <- function(input_string) {
  symbols_to_exclude <- c("\\`", "\\.", "\\!", "\\@", "\\#", "\\$", "\\%", "\\^", "\\&", 
                          "\\*", "\\(", "\\)", "\\-", "\\_", "\\=", "\\+", "\\[", "\\]", 
                          "\\{", "\\}", "\\|", "\\\\","\\:", "\\;", "\\\"", "\\'", "\\<", "\\>", "\\?", "\\/", "\\~")
  cleaned_string <- gsub(paste(symbols_to_exclude, collapse = "|"), "", input_string)
  return(cleaned_string)
}

#' Tokenize Text Data for Wordclouds and Ngrams
#'
#' This function tokenizes text data, removes stop words, and prepares data for wordcloud and ngram analysis.
#'
#' @param data A dataframe containing the text data.
#' @param text_column The name of the text column to be tokenized.
#' @param ngram The number of words in each ngram. Default is 2.
#' @return A list containing wordcloud data and ngram data.
#' @export
string_tokenize_data <- function(data, text_column, ngram = 2) {
  text_data <- data %>%
    tidytext::unnest_tokens(output = word, input = {{ text_column }})
  text_data <- filter(text_data, !is.na(word))
  text_df <- tibble(word = text_data$word)
  word_counts <- text_df %>%
    count(word)
  text_df <- anti_join(text_df, stop_words, by = "word")
  word_counts_cleaned <- text_df %>% count(word)
  bigram_data <- 
    data %>%
    tidytext::unnest_tokens(bigram, text_column, token = "ngrams", n = ngram) %>%
    select(bigram)
  bi_advice_split <- bigram_data %>%
    separate(col = bigram, into = c("word1", "word2"), sep = " ")
  bigram_cleaned <-
    bi_advice_split %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  bigram_cleaned <- bigram_cleaned %>%
    count(word1, word2, sort = TRUE) %>%
    filter(complete.cases(.))
  bigram_cleaned <- bigram_cleaned %>%
    mutate(count = factor(n))
  return(list(wordcloud_data = word_counts_cleaned, ngram_data = bigram_cleaned))  
}
