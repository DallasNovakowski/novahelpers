#' This function generates a summary table with various statistics for the specified grouping variables and dependent variable.

#' @param data The targeted dataframe
#' @param group_vars a character string/vector of your grouping variables
#' @param  summarization_var a character string of your targeted summary variable
#' @importFrom dplyr group_by summarise n
#' @importFrom stats sd quantile median
#' @importFrom rlang sym syms
#'
#' @return A dataframe with summary statistics
#' @export


# This function generates a summary table with various statistics for the specified grouping variables and dependent variable.
make_summary <- function(data, group_vars, summarization_var) {
  data %>%
    group_by(!!!syms(group_vars)) %>%
    dplyr::summarise(
      mean = mean(!!sym(summarization_var)),             # Calculate the mean of the dependent variable
      min = min(!!sym(summarization_var)),               # Calculate the minimum
      max = max(!!sym(summarization_var)),               # Calculate the maximum
      n = n(),                         # Count the number of observations
      std_dev = sd(!!sym(summarization_var)),           # Calculate the standard deviation
      se = sd(!!sym(summarization_var)) / base::sqrt(n()), # Calculate the standard error
      y25 = quantile(!!sym(summarization_var), 0.25),    # Calculate the 25th percentile
      y50 = median(!!sym(summarization_var)),           # Calculate the median (50th percentile)
      y75 = quantile(!!sym(summarization_var), 0.75),    # Calculate the 75th percentile
      loci = mean(!!sym(summarization_var)) - 1.96 * se, # Calculate the lower confidence interval
      upci = mean(!!sym(summarization_var)) + 1.96 * se  # Calculate the upper confidence interval
    )
  
}

# make_summary(df, c("sex", "species"), "flipper_length_mm")

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
