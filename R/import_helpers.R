#' Import Helpers
#'
#' A collection of helper functions for data processing and plotting.
#' 
#' @details
#' This package provides functions to:
#' \itemize{
#'   \item Prepare factor levels for summary and raw data.
#'   \item Find the common prefix among a vector of strings.
#'   \item Capitalize strings (the first letter and/or all character columns).
#'   \item Recode columns using a named vector for replacement.
#'   \item Compute the mean excluding a special missing value (-99).
#'   \item Replace substrings in variable names.
#'   \item Propagate non-empty header values.
#'   \item Clean variable names when pivoting from wide to long format.
#' }
#'
#' @name import_helpers
NULL

################################################################################
#' Prepare Factor Levels
#'
#' Reorders the factor levels in a summary data frame based on the `mean` column,
#' and then applies that ordering to the raw data.
#'
#' @param summary_data A data frame containing the summary statistics.
#' @param raw_data A data frame containing the original raw data.
#' @param x_var A character string specifying the variable to order.
#'
#' @return A named list with elements \code{summary_data} and \code{raw_data} that
#'         have been re-ordered.
#' @export
#'
#' @importFrom forcats fct_reorder
prepare_factor_levels <- function(summary_data, raw_data, x_var) {
  summary_data[[x_var]] <- forcats::fct_reorder(summary_data[[x_var]], summary_data$mean, .na_rm = TRUE)
  mean_order <- levels(summary_data[[x_var]])
  raw_data[[x_var]] <- factor(raw_data[[x_var]], levels = mean_order, ordered = TRUE)
  list(summary_data = summary_data, raw_data = raw_data)
}

################################################################################
#' Find Common Prefix
#'
#' Finds the longest common prefix among a vector of strings.
#'
#' @param strings A character vector.
#'
#' @return A string containing the common prefix, or an empty string if none exists.
#' @export
find_common_prefix <- function(strings) {
  if (length(strings) == 0) return("")
  
  prefix <- strings[1]
  for (s in strings[-1]) {
    while (!startsWith(s, prefix)) {
      prefix <- substr(prefix, 1, nchar(prefix) - 1)
      if (prefix == "") return("")
    }
  }
  prefix
}

################################################################################
#' Capitalize First Letter
#'
#' Capitalizes only the first letter of each string in the input vector.
#'
#' @param x A character vector.
#'
#' @return A character vector with the first letter capitalized.
#' @export
capitalize_first_letter <- function(x) {
  x <- as.character(x)
  sapply(x, function(str) {
    if (is.na(str) || str == "") return(str)
    paste0(toupper(substr(str, 1, 1)), tolower(substr(str, 2, nchar(str))))
  }, USE.NAMES = FALSE)
}

################################################################################
#' Capitalize All Character Columns
#'
#' Converts the first letter of every character column in a data frame to uppercase.
#'
#' @param df A data frame.
#'
#' @return The original data frame with all character columns capitalized.
#' @export
#'
#' @importFrom dplyr mutate across all_of
capitalize_all_columns <- function(df) {
  char_columns <- names(df)[sapply(df, is.character)]
  df <- df %>% dplyr::mutate(across(all_of(char_columns), ~ capitalize_first_letter(.x)))
  df
}

################################################################################
#' Recode Columns
#'
#' Recode values in specified columns of a data frame.
#'
#' @param df A data frame.
#' @param columns A character vector specifying which columns to recode.
#' @param old_values A vector of old values to be replaced.
#' @param new_values A vector of new values to replace the old values.
#'
#' @return The data frame with recoded columns.
#' @export
#'
#' @importFrom dplyr mutate across all_of recode
recode_columns <- function(df, columns, old_values, new_values) {
  recode_vector <- setNames(new_values, old_values)
  df <- df %>% dplyr::mutate(across(
    all_of(columns),
    ~ dplyr::recode(.x, !!!recode_vector)
  ))
  df
}

################################################################################
#' Capitalize First Letter (Alternative)
#'
#' Capitalizes the first letter of each element in a character vector.
#'
#' @param x A character vector.
#'
#' @return A character vector with the first letter in uppercase.
#' @export
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

################################################################################
#' Mean Excluding -99
#'
#' Calculates the mean of a numeric vector excluding values equal to -99.
#'
#' @param x A numeric vector.
#'
#' @return The mean of the values in \code{x} that are not \code{-99}.
#' @export
mean_excluding_minus_99 <- function(x) {
  mean(x[x != -99], na.rm = TRUE)
}

################################################################################
#' Replace Strings in Variable Names
#'
#' Replaces substrings in a given name using a named vector of replacements.
#'
#' @param name A character string.
#' @param replacements A named character vector where names indicate the substrings to be replaced
#'        and values provide the replacement strings.
#'
#' @return A character string with the replacements made.
#' @export
replace_strings <- function(name, replacements) {
  for (original in names(replacements)) {
    replacement <- replacements[[original]]
    name <- gsub(original, replacement, name, fixed = TRUE)
  }
  name
}

################################################################################
#' Propagate Headers
#'
#' Propagates non-empty header values from the first row to subsequent empty header cells.
#'
#' @param headers A character vector of header values.
#'
#' @return A character vector with propagated header values.
#' @export
propagate_headers <- function(headers) {
  for (i in seq_along(headers)[-1]) {
    if (is.na(headers[i]) || headers[i] == "") {
      headers[i] <- headers[i - 1]
    }
  }
  headers
}

################################################################################
#' Clean Variable Names
#'
#' Pivots a data frame from wide to long format for columns matching a prefix,
#' cleans up the variable names by removing the common prefix and replacing underscores,
#' and optionally applies custom labels.
#'
#' @param df A data frame.
#' @param prefix_pattern A regular expression pattern used to select columns.
#' @param custom_labels An optional named vector for replacing cleaned names. Names should match the cleaned names.
#' @param id_vars A character string or vector specifying identifier columns. Defaults to \code{"year"}.
#'
#' @return A long-format data frame with cleaned variable names.
#' @export
#'
#' @importFrom dplyr select mutate across all_of
#' @importFrom tidyr pivot_longer
#' @importFrom tools toTitleCase
clean_variable_names <- function(df, prefix_pattern, custom_labels = NULL, id_vars = "year") {
  variable_names <- names(df)[grepl(prefix_pattern, names(df))]
  long_df <- df %>%
    tidyr::pivot_longer(cols = all_of(variable_names), names_to = "variable", values_to = "value") %>%
    dplyr::mutate(variable = gsub(prefix_pattern, "", variable),
                  variable = gsub("_", " ", variable),
                  variable = tools::toTitleCase(variable))
  
  if (!is.null(custom_labels)) {
    long_df$variable <- dplyr::recode(long_df$variable, !!!custom_labels)
  }
  
  if (!is.null(id_vars)) {
    id_data <- df[id_vars]
    long_df <- cbind(id_data[rep(1:nrow(id_data), each = length(variable_names))], long_df)
  }
  
  long_df
}
