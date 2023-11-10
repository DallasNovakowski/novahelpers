#' This function loops over multiple variables with run_summary, returning each one to a list
#' 
#' @param data The targeted dataframe
#' @param group_vars character strings of our grouping variables
#' @param summarization_vars 1+ character strings with numeric variable names
#' @return A list of dataframes with with summary statistics
#' @export


# Function to group and summarize data for multiple variables
run_many_summaries <- function(data, group_vars, summarization_vars) {
  results <- list()
  for (var in summarization_vars) {
    result <- run_summary(data, group_vars, var)
    results[[var]] <- result
  }
  return(results)
}

 
# my_summaries <- run_many_summaries(data = df, 
#                   summarization_vars = c("bill_length_mm", "flipper_length_mm", "body_mass_g"), 
#                   group_vars= c("species", "sex"))



