#' This function runs car::Anova on lm models, returning each result to a list
#' 
#' @param lm_results_list A list of lm model objects
#' @param type An optional character string specifying the type of ANOVA to be performed (default is "III")
#' @return A list of ANOVA results
#' @importFrom car Anova
#' @export

# Function to run ANOVA on lm models
run_many_anovas <- function(lm_results_list, type = NA) {
  # if (!requireNamespace("car", quietly = TRUE)) {
  #   install.packages("car")
  # }
  
  results <- list()
  for (var in names(lm_results_list)) {
    model <- lm_results_list[[var]]
    anova_result <- car::Anova(model, type = type)
    results[[var]] <- anova_result
  }
  return(results)
}

# Example usage:
# Assuming you have a list of lm models 'lm_results_list' from run_many_lm

# anova_results_list <- run_many_anovas(lm_results_list, type = "III")
