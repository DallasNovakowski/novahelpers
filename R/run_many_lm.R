#' This function loops over multiple variables with run_anova, returning each one to a list
#' 
#' @param data The targeted dataframe
#' @param independent_variable character string of our independent variable
#' @param dependent_variables 1+ character strings with numeric variable names
#' @return A list of dataframes with ANOVA results
#' @importFrom stats as.formula lm
#' @export


# Function to run lm for multiple dependent variables
run_many_lm <- function(data, independent_variable, dependent_variables) {
  results <- list()
  for (var in dependent_variables) {
    formula <- as.formula(paste(var, "~", paste(independent_variable, collapse = "*")))
    model <- lm(formula, data = data)
    results[[var]] <- model
  }
  return(results)
}

# Example usage:
# Assuming you have a data frame 'df' with columns 'IndependentVar', 'DependentVar1', 'DependentVar2'
# and you want to model the effect of 'IndependentVar' on both 'DependentVar1' and 'DependentVar2'

# lm_results_list <- run_many_lm(data = df, 
#                                independent_variable = 'IndependentVar', 
#                                dependent_variables = c('DependentVar1', 'DependentVar2'))