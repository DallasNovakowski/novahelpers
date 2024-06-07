#' Replacements for Contractions
#'
#' A named character vector containing contractions and their replacements.
#'
#' @format A named character vector with 14 elements:
#' \describe{
#'   \item{weren't}{Replaced with "were not"}
#'   \item{Weren't}{Replaced with "Were not"}
#'   \item{don’t}{Replaced with "do not"}
#'   \item{Don’t}{Replaced with "Do not"}
#'   \item{don't}{Replaced with "do not"}
#'   \item{Don't}{Replaced with "Do not"}
#'   \item{haven't}{Replaced with "have not"}
#'   \item{Haven't}{Replaced with "Have not"}
#'   \item{weren't}{Replaced with "were not"}
#'   \item{Weren't}{Replaced with "Were not"}
#'   \item{don't}{Replaced with "do not"}
#'   \item{Don't}{Replaced with "Do not"}
#'   \item{haven't}{Replaced with "have not"}
#'   \item{Haven't}{Replaced with "Have not"}
#' }
#' @source Generated for example purposes

#' @export
"contraction_replacements"





#' Abbreviation Corrections Data Frame
#'
#' A data frame containing corrections for abbreviations.
#'
#' This data frame is used to correct and standardize abbreviations in text data.
#'
#' @format A data frame with 2 rows and 2 variables:
#' \describe{
#'   \item{original}{Character. The original term that needs to be replaced.}
#'   \item{replacement}{Character. The standardized abbreviation.}
#' }
#' @examples
#' # Example usage
#' abbreviation_corrections
#' # To access the original terms
#' abbreviation_corrections$original
#' # To access the replacements
#' abbreviation_corrections$replacement
#' @source Generated for example purposes

#' @export
"abbreviation_corrections"






#' Whitelist of Terms
#'
#' A character vector containing terms that are whitelisted for use in text data processing.
#'
#' This vector is used to maintain specific terms in text data that should not be altered or anonymized.
#'
#' @format A character vector with 380 elements.
#' \describe{
#'   \item{Mona Lisa}{A term included in the whitelist.}
#'   \item{Emily Carr}{A term included in the whitelist.}
#'   \item{Prince George}{A term included in the whitelist.}
#'   \item{...}{Additional terms included in the whitelist.}
#' }
#' @examples
#' # Example usage
#' anonymizing_whitelist
#' # To access specific terms
#' anonymizing_whitelist[1:10]
#' @source Generated for example purposes

#' @export
"anonymizing_whitelist"






#' Positions to Sanitize
#'
#' A character vector containing positions or titles that need to be sanitized in text data.
#'
#' This vector is used to identify and sanitize specific job titles or positions in text data to ensure anonymity or consistency.
#'
#' @format A character vector with 13 elements.
#' \describe{
#'   \item{associate dean}{A position included in the list of positions to sanitize.}
#'   \item{dean}{A position included in the list of positions to sanitize.}
#'   \item{Vice President}{A position included in the list of positions to sanitize.}
#'   \item{president}{A position included in the list of positions to sanitize.}
#'   \item{department head}{A position included in the list of positions to sanitize.}
#'   \item{head of the department}{A position included in the list of positions to sanitize.}
#'   \item{program coordinator}{A position included in the list of positions to sanitize.}
#'   \item{program head}{A position included in the list of positions to sanitize.}
#'   \item{program chair}{A position included in the list of positions to sanitize.}
#'   \item{area head}{A position included in the list of positions to sanitize.}
#'   \item{area chair}{A position included in the list of positions to sanitize.}
#'   \item{chair}{A position included in the list of positions to sanitize.}
#'   \item{advisor}{A position included in the list of positions to sanitize.}
#' }
#' @examples
#' # Example usage
#' positions_to_sanitize
#' # To access specific positions
#' positions_to_sanitize[1:5]
#' @source Generated for example purposes

#' @export
"positions_to_sanitize"





#' Common names Dataset
#'
#' A comprehensive list of common names used in English-speaking countries.
#'
#' @docType data
#'
#' @usage data(person_names)
#'
#' @format A character vector with 18,610 elements, each representing a surname.
#'
#' @details
#' The `person_names` dataset includes a broad collection of names commonly used in English-speaking countries.
#' This dataset can be utilized for various purposes such as data anonymization, testing, and simulations where random or common names are required.
#'
#' @examples
#' # Load the person_names data
#' data(person_names)
#'
#' # Display the first 10 names in the dataset
#' head(person_names, 10)
#'
#' # Use the names in a random sampling scenario
#' sampled_names <- sample(person_names, 100, replace = TRUE)
#' print(sampled_names)
#'
#' @source
#' The `person_names` dataset is a synthetic compilation of common names and does not reference any specific source.
#' It is intended for illustrative and testing purposes only.

#' @export
"person_names"





#' Additional Names to Sanitize Dataset
#'
#' A dataset containing additional names that require sanitization in data processing tasks.
#'
#' @docType data
#'
#' @usage data(additional_to_sanitize)
#'
#' @format A character vector with 2 elements, each representing a name format that needs sanitization.
#'
#' @details
#' The `additional_to_sanitize` dataset includes specific names that have been identified as requiring sanitization. 
#' This dataset is used in conjunction with other sanitization processes to ensure that data is clean and consistent.
#' 
#' The current entries are:
#' - "D. S"
#' - "D.S"
#'
#' These names may represent common abbreviations or initials that should be handled appropriately during data cleaning.
#'
#' @examples
#' # Load the additional_to_sanitize data
#' data(additional_to_sanitize)
#'
#' # Display the names that need sanitization
#' print(additional_to_sanitize)
#'
#' # Example usage in a data sanitization function
#' sanitized_names <- gsub("\\s+", "", additional_to_sanitize)
#' print(sanitized_names)
#'
#' @source
#' The `additional_to_sanitize` dataset is a synthetic compilation of names identified for sanitization and does not reference any specific source.
#' It is intended for illustrative and data cleaning purposes only.

#' @export
"additional_to_sanitize"





#' Curse Words Regular Expressions Dataset
#'
#' A dataset containing regular expressions representing curse words and offensive language.
#'
#' @docType data
#'
#' @usage data(curse_words_regex)
#'
#' @format A character vector with 435 elements, each representing a regular expression for a curse word or offensive language.
#'
#' @details
#' The `curse_words_regex` dataset includes regular expressions for identifying curse words and offensive language 
#' in text data. These regular expressions can be used for text filtering, cleaning, or censorship purposes.
#' 
#' The dataset contains a variety of curse words and offensive terms in different forms and spellings, including variations 
#' with symbols, misspellings, and alternative spellings.
#' 
#' @examples
#' # Load the curse_words_regex data
#' data(curse_words_regex)
#'
#' # Display the regular expressions
#' print(curse_words_regex)
#'
#' # Example usage in text filtering
#' library(stringr)
#' text <- "You're a stupid motherfucker!"
#' filtered_text <- str_replace_all(text, curse_words_regex, "***")
#' print(filtered_text)
#'
#' @source
#' The `curse_words_regex` dataset is a synthetic compilation of regular expressions for curse words and offensive language 
#' and does not reference any specific source. It is intended for illustrative and text processing purposes only.

#' @export
"curse_words_regex"

