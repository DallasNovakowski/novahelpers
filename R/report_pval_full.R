#' report p-value
#'
#' @param pval  the p value
#' @param italicize Whether the symbols should be italicized
#' @return A character string of p-value
#' @export


# This function formats a p-value and optionally italicizes it for reporting.
report_pval_full <- function(pval, italicize = TRUE) {
  # Check if the p-value is less than .001
  if (pval < .001) {
    # If p-value is very small, format it as "*p* < .001" (with or without italics)
    result <- ifelse(italicize == TRUE, "*p* < .001", "p < .001")
  } else {
    # If p-value is not very small, format it as "*p* = " with either 2 or 3 decimal places
    if (pval >= .01) {
      # Use 2 decimal places for p-values >= .01
      result <- ifelse(italicize == TRUE, "*p*", "p") # Start with "*p*" or "p"
      result <- paste0(result, " = ", weights::rd(pval, 2)) # Append the formatted p-value
    } else {
      # Use 3 decimal places for p-values less than .01
      result <- ifelse(italicize == TRUE, "*p*", "p") # Start with "*p*" or "p"
      result <- paste0(result, " = ", weights::rd(pval, 3)) # Append the formatted p-value
    }
  }

  # Return the formatted p-value
  return(result)
}
