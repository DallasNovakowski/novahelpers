#' Run emmeans on a list of lm results
#'
#' This function takes a list of lm results and performs emmeans analysis on each
#' model based on the specified emmeans specifications.
#'
#' @param lm_results_list A list of lm results.
#' @param emmeans_specs The emmeans specifications to be passed to the emmeans function.
#'
#' @return A list of emmeans results corresponding to each lm model in the input list.
#'

#' @import emmeans emmeans
#' @export
#' 


run_many_emmeans <- function(lm_results_list, emmeans_specs) {
  emmeans_results <- list()
  
  for (var in names(lm_results_list)) {
    lm_model <- lm_results_list[[var]]
    emmeans_model <- emmeans::emmeans(lm_model, specs = emmeans_specs)
    emmeans_results[[var]] <- emmeans_model
  }
  
  return(emmeans_results)
}


