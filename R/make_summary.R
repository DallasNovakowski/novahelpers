#' This function generates a summary table with various statistics for the specified grouping variables and dependent variable.

#' @param data The targeted dataframe
#' @param dv The name of the variable we want to summarize
#' @param grouping1 One grouping variable
#' @param grouping2 A second grouping variable
#' @param grouping3 A third grouping variable
#' @return A dataframe with summary statistics
#' @importFrom dplyr group_by summarise n
#' @importFrom stats sd quantile median
#'
#' @export

# This function generates a summary table with various statistics for the specified grouping variables and dependent variable.
make_summary <- function(data, dv, grouping1, grouping2, grouping3){
  # Use dplyr to group the data by the specified grouping variables and calculate summary statistics

  data %>%
    group_by({{grouping1}}, {{grouping2}}, {{grouping3}}) %>% # Group by the specified variables
    dplyr::summarise(
      mean = mean({{dv}}),             # Calculate the mean of the dependent variable
      min = min({{dv}}),               # Calculate the minimum
      max = max({{dv}}),               # Calculate the maximum
      n = n(),                         # Count the number of observations
      std_dev = sd({{dv}}),           # Calculate the standard deviation
      se = sd({{dv}}) / base::sqrt(n()), # Calculate the standard error
      y25 = quantile({{dv}}, 0.25),    # Calculate the 25th percentile
      y50 = median({{dv}}),           # Calculate the median (50th percentile)
      y75 = quantile({{dv}}, 0.75),    # Calculate the 75th percentile
      loci = mean({{dv}}) - 1.96 * se, # Calculate the lower confidence interval
      upci = mean({{dv}}) + 1.96 * se  # Calculate the upper confidence interval
    )
}
