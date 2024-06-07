#' Neutralize Pronouns
#'
#' This script provides functions for neutralizing pronouns in a given text, replacing gender-specific pronouns with gender-neutral alternatives.
#' It includes functions for context-aware replacement of "her" and handling verb conjugations, as well as general gender-neutralization of pronouns.
#'
#' @section Functions:
#' \itemize{
#'   \item \code{context_aware_her_replace} - Performs context-aware replacement of "her" and handles verb conjugations.
#'   \item \code{gender_neutralize} - Replaces gender-specific pronouns with gender-neutral alternatives.
#' }
#'
#' @name pronoun_neutralization_functions
NULL

#' Perform Context-Aware Replacement of "her" and Handle Verb Conjugations
#'
#' This function performs context-aware replacement of "her" in a given text, taking into account the surrounding context and handling verb conjugations accordingly.
#'
#' @param sentence A character vector containing the text to be processed.
#' @return A character vector with "her" replaced by the appropriate gender-neutral pronoun and verb conjugations modified accordingly.
#' @export
context_aware_her_replace <- function(sentence) {
  # Process the sentence using spaCy
  doc <- spacy_parse(sentence)
  
  # Extract tokens, part-of-speech tags, and dependency labels
  tokens <- doc$token
  pos_tags <- doc$pos
  dep_labels <- doc$dep
  
  # Find instances of "her" and their surrounding context
  her_indices <- which(tokens == "her")
  for (index in her_indices) {
    # Extract the context surrounding the instance of "her"
    start <- max(1, index - 2)
    end <- min(length(tokens), index + 2)
    context_tokens <- tokens[start:end]
    context_pos <- pos_tags[start:end]
    context_deps <- dep_labels[start:end]
    
    # Analyze context and determine appropriate replacement for "her" and verb conjugation
    replacement <- "them"
    if ("NN" %in% context_pos) {
      # If a noun is found in the context, use "their" instead of "them"
      replacement <- "their"
    }
    
    # Perform the replacement for "her"
    sentence <- gsub("\\bher\\b", replacement, sentence, ignore.case = TRUE)
    
    # Modify verb conjugation based on the subject
    verb_index <- which(context_deps == "nsubj" | context_deps == "nsubjpass")
    if (length(verb_index) > 0) {
      verb <- tokens[verb_index]
      if (length(verb) > 0) {
        if (replacement == "them") {
          # If the subject is "them", change verb conjugation to plural form
          sentence <- gsub(paste0("\\b", verb, "\\b"), paste0(verb, "e"), sentence, ignore.case = TRUE)
        }
      }
    }
  }
  
  return(sentence)
}

#' Replace Gender-Specific Pronouns with Gender-Neutral Alternatives
#'
#' This function replaces gender-specific pronouns (e.g., "he", "she") with gender-neutral alternatives (e.g., "they").
#'
#' @param sentence A character vector containing the text to be processed.
#' @return A character vector with gender-specific pronouns replaced by gender-neutral alternatives.
#' @export
gender_neutralize <- function(sentence) {
  replacements <- list(
    "\\b(he's|she's)\\b" = "they're",
    "\\b(he is|she is)\\b" = "they are",
    "\\b(He's|She's)\\b" = "They're",
    "\\b(He is|She is)\\b" = "They are",
    "\\b(he'd|she'd)\\b" = "they'd",
    "\\b(he had|she had)\\b" = "they had",
    "\\b(He'd|She'd)\\b" = "They'd",
    "\\b(He had|She had)\\b" = "They had",
    "\\b(He was|She was)\\b" = "They were",
    "\\b(he was|she was)\\b" = "they were",
    "\\b(he'll|she'll)\\b" = "they'll",
    "\\b(he will|she will)\\b" = "they will",
    "\\b(He'll|She'll)\\b" = "They'll",
    "\\b(He will|She will)\\b" = "They will",
    "\\b(he|she)\\b" = "they",
    "\\b(He|She)\\b" = "They",
    "\\b(him)\\b" = "them",
    "\\b(Him)\\b" = "Them",
    "\\b(His|Her)\\b" = "Their",
    "\\b(his|her)\\b" = "their",
    "\\b(They's)\\b" = "They're",
    "\\b(they's)\\b" = "they're",
    "\\b(himself|herself)\\b" = "themselves"
  )
  
  for (pattern in names(replacements)) {
    sentence <- gsub(pattern, replacements[[pattern]], sentence, ignore.case = FALSE)
  }
  
  # If context_aware_replace is another function, it should be called before or after these replacements as needed
  sentence <- context_aware_her_replace(sentence)
  
  return(sentence)
}

