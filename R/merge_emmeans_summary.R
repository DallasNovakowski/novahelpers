#' This function takes summary data and a tidy emmeans object and adds emmeans-related columns to the summary data

#' @param summary_data The  dataframe output of make_summary
#' @param emmean_obj The tidy dataframe
#' @return A dataframe with summary statistics
#' @export


merge_emmeans_summary <- function(summary_data, emmean_obj) {
  # Add emmeans-related columns to the summary_data

  emmeans_tidy <- data.frame(emmean_obj$emmeans)
  
  # Assign the 'emmean' values from the emmeans_tidy to a new column 'emmean' in summary_data
  summary_data$emmean <- emmeans_tidy$emmean

  # Assign the 'SE' values from emmeans_tidy to a new column 'emmean_se' in summary_data
  summary_data$emmean_se <- emmeans_tidy$SE

  # Assign the 'lower.CL' values from emmeans_tidy to a new column 'emmean_loci' in summary_data
  summary_data$emmean_loci <- emmeans_tidy$lower.CL

  # Assign the 'upper.CL' values from emmeans_tidy to a new column 'emmean_upci' in summary_data
  summary_data$emmean_upci <- emmeans_tidy$upper.CL

  # Return the summary_data with the added emmeans-related columns
  return(summary_data)
}
