#' String Anonymization and Sanitization Functions
#'
#' This script provides a collection of functions for anonymizing and sanitizing strings within text data. 
#' These functions are particularly useful for preparing textual data for analysis while ensuring sensitive 
#' information is removed or obfuscated.
#'
#' @section Functions:
#' \itemize{
#'   \item \code{max_typos} - Determines the maximum allowed typos based on word length.
#'   \item \code{anonymize_names} - Sanitizes occurrences of names within a given sentence.
#'   \item \code{anonymize_positions} - Sanitizes occurrences of specified positions within a vector of sentences.
#'   \item \code{sanitize_topics_in_sentences} - Sanitizes occurrences of specified topics within a vector of sentences.
#'   \item \code{sanitize_numbers_in_sentences} - Sanitizes occurrences of numbers within a vector of sentences.
#'   \item \code{sanitize_courses_in_sentences} - Sanitizes occurrences of specified courses within a vector of sentences.
#' }
#' 
#' @name string_anonymization_functions
NULL

# Load required libraries
if (!require(stringdist)) {
  install.packages("stringdist")
}
library(stringdist)
if (!require(textclean)) {
  install.packages("textclean")
}
library(textclean)
if (!require(lexicon)) {
  install.packages("lexicon")
}
library(lexicon)

#' Determine Maximum Allowed Typos Based on Word Length
#'
#' This function determines the maximum number of allowed typos based on the length of the word.
#'
#' @param word_length The length of the word.
#' @return The maximum number of allowed typos.
#' @export
max_typos <- function(word_length) {
  if (word_length <= 4) {
    return(0)
  } else if (word_length >= 5 & word_length <= 8) {
    return(1)
  } else {
    return(2)
  }
}

#' Anonymize Names in a Sentence
#'
#' This function sanitizes occurrences of names within a given sentence.
#'
#' @param sentence A string representing the sentence to be sanitized.
#' @param names A vector of names to be anonymized.
#' @param whitelist A vector of words that should not be anonymized.
#' @return A sanitized sentence with names anonymized.
#' @export
anonymize_names <- function(sentence, names, whitelist) {
  if (is.na(sentence)) return(NA)  # Check for NA and return NA if true
  
  # Use regex to split the sentence into words, keeping punctuation attached
  words <- unlist(strsplit(sentence, "(?<=\\W)(?=\\w)|(?<=\\w)(?=\\W)", perl = TRUE))
  
  sanitized_words <- sapply(words, function(word) {
    word_lower <- tolower(word)
    
    if (word_lower %in% tolower(whitelist)) return(word)
    
    # Remove punctuation from the word for comparison
    word_clean <- gsub("[\\p{P}]+", "", word_lower, perl = TRUE)
    
    max_dist <- max_typos(nchar(word_clean))
    
    # Filter out names that are significantly different in length
    filtered_names <- names[nchar(names) >= nchar(word_clean) - max_dist & nchar(names) <= nchar(word_clean) + max_dist]
    if (length(filtered_names) == 0) return(word)
    
    distances <- stringdist::stringdist(word_lower, tolower(filtered_names))
    distances <- na.omit(distances)  # Remove NA values from distances
    
    if (length(distances) > 0 && any(distances <= max_dist)) {
      return("SANITIZED_NAME")
    } else {
      return(word)
    }
  })
  
  sanitized_sentence <- paste(sanitized_words, collapse = "")
  
  sanitizing_sentences <- function(sentence) {
    gsub("\\bSANITIZED_NAME(\\s+SANITIZED_NAME)+\\b", "SANITIZED_NAME", sentence, perl = TRUE)
  }
  
  sanitized_sentence <- sapply(sanitized_sentence, sanitizing_sentences)
  
  sanitized_sentence <- gsub("\\b(Mr|Ms|Dr|Mrs)\\.\\s*", "", sanitized_sentence, ignore.case = TRUE)
  sanitized_sentence <- gsub("\\b(Mr|Ms|Dr|Mrs)\\b", "", sanitized_sentence, ignore.case = TRUE)
  sanitized_sentence <- gsub("x-SANITIZED_NAME", "x-ray", sanitized_sentence, ignore.case = TRUE)
  
  return(sanitized_sentence)
}

#' Anonymize Positions in Sentences
#'
#' This function sanitizes occurrences of specified positions within a vector of sentences.
#'
#' @param sentences_vector A vector of sentences to be sanitized.
#' @param positions_to_sanitize A vector of position titles to be anonymized.
#' @return A vector of sanitized sentences.
#' @export
anonymize_positions <- function(sentences_vector, positions_to_sanitize) {
  sapply(sentences_vector, function(sentence) {
    for (position in positions_to_sanitize) {
      sentence <- gsub(position, "SANITIZED_ADMIN", sentence, ignore.case = TRUE)
    }
    return(sentence)
  })
}

#' Sanitize Topics in Sentences
#'
#' This function sanitizes occurrences of specified topics within a vector of sentences.
#'
#' @param sentences_vector A vector of sentences to be sanitized.
#' @param topics_to_sanitize A vector of topics to be anonymized.
#' @return A vector of sanitized sentences.
#' @export
sanitize_topics_in_sentences <- function(sentences_vector, topics_to_sanitize) {
  sapply(sentences_vector, function(sentence) {
    for (topic in topics_to_sanitize) {
      sentence <- gsub(paste0("\\b", topic, "\\b"), "SANITIZED_TOPIC", sentence, ignore.case = TRUE)
    }
    return(sentence)
  })
}

#' Sanitize Numbers in Sentences
#'
#' This function sanitizes occurrences of numbers within a vector of sentences.
#'
#' @param sentences_vector A vector of sentences to be sanitized.
#' @return A vector of sanitized sentences.
#' @export
sanitize_numbers_in_sentences <- function(sentences_vector) {
  sanitized_sentences <- sentences_vector
  pattern <- "(?<![\\$%\\d])[0-9]{3}(?![\\$%\\d])"
  
  for (i in seq_along(sentences_vector)) {
    sanitized_sentences[i] <- gsub(pattern, "SANITIZED_NUMBER", sentences_vector[i], perl = TRUE)
  }
  
  return(sanitized_sentences)
}

#' Sanitize Courses in Sentences
#'
#' This function sanitizes occurrences of specified courses within a vector of sentences.
#'
#' @param sentences_vector A vector of sentences to be sanitized.
#' @param courses_to_sanitize A vector of course names to be anonymized.
#' @return A vector of sanitized sentences.
#' @export
sanitize_courses_in_sentences <- function(sentences_vector, courses_to_sanitize) {
  sapply(sentences_vector, function(sentence) {
    for (course in courses_to_sanitize) {
      sentence <- gsub(paste0("\\b", course, "\\b"), "SANITIZED_COURSE", sentence, ignore.case = TRUE)
    }
    return(sentence)
  })
}
