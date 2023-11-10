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
    group_by(!!!syms(group_vars)) %>%
    dplyr::summarise(
      missing_count = sum(is.na(!!sym(summarization_var))),
      n = n(),                         # Count the number of observations
      mean = mean(!!sym(summarization_var)),             # Calculate the mean of the dependent variable
      std_dev = sd(!!sym(summarization_var)),           # Calculate the standard deviation
      se = sd(!!sym(summarization_var)) / base::sqrt(n()), # Calculate the standard error
      loci = mean(!!sym(summarization_var)) - 1.96 * .data$se, # Calculate the lower confidence interval
      upci = mean(!!sym(summarization_var)) + 1.96 * .data$se,  # Calculate the upper confidence interval
      min = min(!!sym(summarization_var)),               # Calculate the minimum
      max = max(!!sym(summarization_var)),               # Calculate the maximum
      y25 = quantile(!!sym(summarization_var), 0.25),    # Calculate the 25th percentile
      y50 = median(!!sym(summarization_var)),           # Calculate the median (50th percentile)
      y75 = quantile(!!sym(summarization_var), 0.75),    # Calculate the 75th percentile
      coef_var = sd(!!sym(summarization_var)) / mean(!!sym(summarization_var)),  # Calculate the coefficient of variation
      skewness = moments::skewness(!!sym(summarization_var)),  # Calculate skewness
      kurtosis = moments::kurtosis(!!sym(summarization_var))  # Calculate kurtosis
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
