#' Generate summary statistics by grouping variables
#'
#' This function returns summary statistics for a numeric variable, grouped by one or more categorical variables.
#' Missing values are defined as \code{NA} or any values in \code{na_vals}.
#'
#' @param data A data frame containing the relevant variables.
#' @param group_vars Character vector of grouping variable names.
#' @param summarization_var Character string naming the variable to summarize.
#' @param na.rm Logical; whether to remove missing values before calculation.
#' @param na_vals Vector of additional values to treat as missing (e.g., \code{-99}). Defaults to \code{c(NA, -99)}.
#'
#' @return A data frame with grouped summary statistics.
#'
#' @importFrom dplyr group_by summarise mutate filter across all_of n
#' @importFrom stats sd quantile median
#' @importFrom rlang sym
#' @importFrom moments skewness kurtosis
#' @export
run_summary <- function(data, group_vars, summarization_var, na.rm = TRUE, na_vals = c(NA, -99)) {
  summarization_sym <- rlang::sym(summarization_var)
  
  # Temporary column creation
  data <- data %>%
    dplyr::mutate(
      .value = !!summarization_sym,
      .valid = !is.na(.value) & !(.value %in% na_vals)
    )
  
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      missing_count = sum(!.valid),
      n     = sum(.valid),
      group_size    = dplyr::n(),
      prop_present  = n_present / group_size,
      
      mean    = if (n_present > 0) mean(.value[.valid], na.rm = na.rm) else NA_real_,
      std_dev = if (n_present > 1) sd(.value[.valid], na.rm = na.rm) else NA_real_,
      se      = if (n_present > 1) std_dev / sqrt(n_present) else NA_real_,
      loci    = if (n_present > 1) mean - 1.96 * se else NA_real_,
      upci    = if (n_present > 1) mean + 1.96 * se else NA_real_,
      
      min = if (n_present > 0) min(.value[.valid], na.rm = na.rm) else NA_real_,
      max = if (n_present > 0) max(.value[.valid], na.rm = na.rm) else NA_real_,
      
      y16 = if (n_present > 0) quantile(.value[.valid], 0.16, na.rm = na.rm) else NA_real_,
      y25 = if (n_present > 0) quantile(.value[.valid], 0.25, na.rm = na.rm) else NA_real_,
      y50 = if (n_present > 0) median(.value[.valid], na.rm = na.rm) else NA_real_,
      y75 = if (n_present > 0) quantile(.value[.valid], 0.75, na.rm = na.rm) else NA_real_,
      y84 = if (n_present > 0) quantile(.value[.valid], 0.84, na.rm = na.rm) else NA_real_,
      
      coef_var = if (n_present > 1 && mean != 0) std_dev / mean else NA_real_,
      skewness = if (n_present > 2) moments::skewness(.value[.valid], na.rm = na.rm) else NA_real_,
      kurtosis = if (n_present > 3) moments::kurtosis(.value[.valid], na.rm = na.rm) else NA_real_,
      .groups = "drop"
    )
}

#' Generate grouped summary statistics for all combinations of grouping variables
#'
#' This function produces summaries for all non-empty combinations of the provided grouping variables.
#' It calls \code{run_summary()} for each grouping combination.
#'
#' @param data A data frame containing the relevant variables.
#' @param summarization_var A character string of the variable to summarize.
#' @param group_var_list Character vector of grouping variable names.
#'
#' @return A named list of summary data frames, each corresponding to a unique grouping combination.
#'
#' @importFrom dplyr mutate filter
#' @importFrom utils combn
#' @export
run_grouped_summaries <- function(data, summarization_var, group_var_list) {
  group_combinations <- unlist(
    lapply(1:length(group_var_list), function(i) {
      utils::combn(group_var_list, i, simplify = FALSE)
    }),
    recursive = FALSE
  )
  
  summary_dataframes <- lapply(group_combinations, function(group_vars) {
    summary <- run_summary(data, group_vars, summarization_var)
    group_name <- paste(group_vars, collapse = "_")
    
    summary %>%
      dplyr::mutate(grouping_vars = group_name) %>%
      dplyr::filter(n_present > 0)
  })
  
  names(summary_dataframes) <- sapply(group_combinations, paste, collapse = "_")
  summary_dataframes
}
