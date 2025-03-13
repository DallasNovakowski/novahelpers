#' This function generates a summary table with various statistics for the specified grouping variables and dependent variable.

#' @param data The targeted dataframe
#' @param group_vars a character string/vector of your grouping variables
#' @param  summarization_var a character string of your targeted summary variable
#' @importFrom dplyr group_by summarise n
#' @importFrom stats sd quantile median
#' @importFrom rlang sym syms .data
#' @importFrom moments skewness kurtosis
#' @return A dataframe with summary statistics
#' @export


# This function generates a summary table with various statistics for the specified grouping variables and dependent variable.

run_summary <- function(data, group_vars, summarization_var) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      missing_count = sum(is.na(.data[[summarization_var]])),
      n = sum(!is.na(.data[[summarization_var]])),  # Count non-missing observations
      prop_present = sum(!is.na(!!summarization_sym)) / n,
    
      mean = ifelse(n > 0, mean(.data[[summarization_var]], na.rm = TRUE), NA_real_),
      std_dev = ifelse(n > 1, sd(.data[[summarization_var]], na.rm = TRUE), NA_real_),
      se = ifelse(n > 1, std_dev / sqrt(n), NA_real_),
      
      loci = ifelse(n > 1, mean - 1.96 * se, NA_real_),
      upci = ifelse(n > 1, mean + 1.96 * se, NA_real_),
      
      min = ifelse(n > 0, min(.data[[summarization_var]], na.rm = TRUE), NA_real_),
      max = ifelse(n > 0, max(.data[[summarization_var]], na.rm = TRUE), NA_real_),
      
      y16 = ifelse(n > 0, quantile(.data[[summarization_var]], 0.16, na.rm = TRUE), NA_real_),
      y25 = ifelse(n > 0, quantile(.data[[summarization_var]], 0.25, na.rm = TRUE), NA_real_),
      y50 = ifelse(n > 0, median(.data[[summarization_var]], na.rm = TRUE), NA_real_),
      y75 = ifelse(n > 0, quantile(.data[[summarization_var]], 0.75, na.rm = TRUE), NA_real_),
      y84 = ifelse(n > 0, quantile(.data[[summarization_var]], 0.84, na.rm = TRUE), NA_real_),
      
      coef_var = ifelse(n > 1 & mean != 0, std_dev / mean, NA_real_),  # Avoid division by zero
      
      skewness = ifelse(n > 2, moments::skewness(.data[[summarization_var]], na.rm = TRUE), NA_real_),
      kurtosis = ifelse(n > 3, moments::kurtosis(.data[[summarization_var]], na.rm = TRUE), NA_real_)
    )
}




# run_summary(df, c("sex", "species"), "flipper_length_mm")

# custom_summarize(df, c("sex", "species"), "flipper_length_mm")
# 
# # Custom summarization function
# custom_summarize <- function(data, group_vars, summarization_var) {
#   result <- data %>%
#     group_by(!!!syms(group_vars)) %>%
#     summarise(
#       mean_value = mean(!!sym(summarization_var)),
#       sd_value = sd(!!sym(summarization_var))
#     )
#   return(result)
# }
