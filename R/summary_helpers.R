#' Generate summary statistics by grouping variables
#'
#' This function generates a summary table with various statistics for the specified grouping variables and target variable.
#' Missing values are defined as \code{NA} or any value included in \code{na_vals} (by default, \code{-99} is treated as missing).
#'
#' @param data A data frame containing the variables of interest.
#' @param group_vars Character vector of grouping variable names.
#' @param summarization_var A character string of the variable to summarize.
#' @param na.rm Logical; whether to exclude NA values from summary statistics.
#' @param na_vals Values to treat as missing (e.g., \code{-99}). Defaults to \code{c(NA, -99)}.
#'
#' @return A data frame with summary statistics by group.
#'
#' @importFrom dplyr group_by summarise mutate filter across all_of n
#' @importFrom stats sd quantile median
#' @importFrom rlang sym
#' @importFrom moments skewness kurtosis
#'
#' @examples
#' \dontrun{
#   library(palmerpenguins)
#   library(dplyr)
# 
#   # Prepare the data: remove NA values and filter out a specific combination.
#   df <- palmerpenguins::penguins %>%
#         na.omit() %>%
#         filter(!(species == "Adelie" & sex == "male"))
# 
#   # Run a summary grouped by species.
#   summary_single <- run_summary(df, group_vars = c("species"), summarization_var = "flipper_length_mm")
#   print(summary_single)
# }
#'
#' @export
run_summary <- function(data, group_vars, summarization_var, na.rm = TRUE, na_vals = c(NA, -99)) {
  summarization_sym <- rlang::sym(summarization_var)
  
  # Create temporary columns unlikely to conflict with user data.
  data <- data %>%
    dplyr::mutate(
      .__tmp_value__ = !!summarization_sym,
      .__tmp_valid__ = !is.na(.__tmp_value__) & !(.__tmp_value__ %in% na_vals)
    )
  
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      missing_count = sum(!.__tmp_valid__),
      n_present     = sum(.__tmp_valid__),
      group_size    = dplyr::n(),
      prop_present  = n_present / group_size,
      
      mean    = ifelse(n_present > 0, mean(.__tmp_value__[.__tmp_valid__], na.rm = na.rm), NA_real_),
      std_dev = ifelse(n_present > 1, sd(.__tmp_value__[.__tmp_valid__], na.rm = na.rm), NA_real_),
      se      = ifelse(n_present > 1, std_dev / sqrt(n_present), NA_real_),
      loci    = ifelse(n_present > 1, mean - 1.96 * se, NA_real_),
      upci    = ifelse(n_present > 1, mean + 1.96 * se, NA_real_),
      
      min = ifelse(n_present > 0, min(.__tmp_value__[.__tmp_valid__], na.rm = na.rm), NA_real_),
      max = ifelse(n_present > 0, max(.__tmp_value__[.__tmp_valid__], na.rm = na.rm), NA_real_),
      
      y16 = ifelse(n_present > 0, quantile(.__tmp_value__[.__tmp_valid__], 0.16, na.rm = na.rm), NA_real_),
      y25 = ifelse(n_present > 0, quantile(.__tmp_value__[.__tmp_valid__], 0.25, na.rm = na.rm), NA_real_),
      y50 = ifelse(n_present > 0, median(.__tmp_value__[.__tmp_valid__], na.rm = na.rm), NA_real_),
      y75 = ifelse(n_present > 0, quantile(.__tmp_value__[.__tmp_valid__], 0.75, na.rm = na.rm), NA_real_),
      y84 = ifelse(n_present > 0, quantile(.__tmp_value__[.__tmp_valid__], 0.84, na.rm = na.rm), NA_real_),
      
      coef_var = ifelse(n_present > 1 && mean != 0, std_dev / mean, NA_real_),
      skewness = ifelse(n_present > 2, moments::skewness(.__tmp_value__[.__tmp_valid__], na.rm = na.rm), NA_real_),
      kurtosis = ifelse(n_present > 3, moments::kurtosis(.__tmp_value__[.__tmp_valid__], na.rm = na.rm), NA_real_),
      .groups = "drop"
    )
}

#' Generate summary statistics for all combinations of grouping variables
#'
#' This function generates summaries for all non-empty combinations of grouping variables supplied in \code{group_var_list}.
#' For each combination, it calls \code{run_summary()} and appends a column \code{grouping_vars} indicating the grouping configuration.
#'
#' @param data A data frame.
#' @param summarization_var A character string of the variable to summarize.
#' @param group_var_list Character vector of potential grouping variable names.
#'
#' @return A named list of data frames, each corresponding to a unique grouping combination.
#'
#' @importFrom dplyr mutate filter
#' @importFrom utils combn
#'
#' @examples
#' \dontrun{
#   library(palmerpenguins)
#   library(dplyr)
# 
#   # Prepare the data.
#   df <- palmerpenguins::penguins %>%
#         na.omit() %>%
#         dplyr::filter(!(species == "Adelie" & sex == "male"))
# 
#   # Define grouping variables and the summarization variable.
#   group_var_list <- c("species", "sex", "island")
#   summarization_var <- "flipper_length_mm"
# 
#   # Generate summary statistics for all combinations of grouping variables.
#   all_summaries <- run_grouped_summaries(df, summarization_var, group_var_list)
# 
#   # Print each summary data frame.
#   lapply(all_summaries, print)
# }
#'
#' @export
run_grouped_summaries <- function(data, summarization_var, group_var_list) {
  # Generate all non-empty combinations of grouping variables.
  group_combinations <- lapply(1:length(group_var_list), function(i) {
    utils::combn(group_var_list, i, simplify = FALSE)
  }) %>% unlist(recursive = FALSE)
  
  summary_dataframes <- lapply(group_combinations, function(group_vars) {
    summary <- run_summary(data, group_vars, summarization_var)
    group_name <- paste(group_vars, collapse = "_")
    
    summary <- summary %>%
      dplyr::mutate(grouping_vars = group_name) %>%
      dplyr::filter(n_present > 0)
    
    summary
  })
  
  names(summary_dataframes) <- sapply(group_combinations, function(x) paste(x, collapse = "_"))
  summary_dataframes
}
