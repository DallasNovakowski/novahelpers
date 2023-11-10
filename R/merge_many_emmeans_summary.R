#' Merge and Rename Columns in Multiple Dataframes
#'
#' Merges two lists of dataframes by specified grouping variables and renames
#' selected columns.
#'
#' @param x A list of summary dataframes.
#' @param y Another list emmean dataframes.
#' @param grouping_vars A character vector specifying the grouping variables for merging.
#'
#' @return A list of dataframes with specified columns renamed.
#' @importFrom dplyr rename
#' @importFrom rlang .data

#' @examples
#' \dontrun{
#' # Example usage
#' # This is an example, replace df1, df2, df3, df4 with your actual data frames
#' result <- merge_many_emmeans_summary(list(df1, df2), list(df3, df4), c("species", "sex"))
#' }
#'
#' @export

merge_many_emmeans_summary <- function(x, y, grouping_vars) {
  Map(function(x_df, y_df) {
    # Merge dataframes
    merged_df <- merge(x_df, y_df, by = grouping_vars)
    # Rename the "SE" column to "emmean_se"
    
    merged_df %>%
      dplyr::rename(emmean_se = .data$SE,
             emmean_loci = .data$lower.CL,
             emmean_upci = .data$upper.CL)
  },
  x,
  y
  )
}