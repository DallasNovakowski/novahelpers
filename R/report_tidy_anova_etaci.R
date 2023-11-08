#' In-text and in-plot reporting of anova
#'
#' @param tidy_frame Your tidy pairwise comparison result
#' @param term The predictor in your tidy ANOVA dataframe
#' @param effsize Display the effect size
#' @param ci95 Display the 95% CI
#' @param ci_lab Display the "95% CI" label
#' @param pval  Whether to include the p value
#' @param teststat Display the F-score and degrees of freedom
#' @return A character string of stats
#' @export
#'
# This function generates a text summary of ANOVA results based on user-specified options.
report_tidy_anova_etaci <- function(tidy_frame, # Your tidy ANOVA dataframe
                                    term, # The predictor in your tidy ANOVA dataframe
                                    effsize = TRUE, # Display the effect size
                                    ci95 = TRUE, # Display the 95% CI
                                    ci_lab = TRUE, # Display the "95% CI" label
                                    teststat = TRUE, # Display the F-score and degrees of freedom
                                    pval = TRUE # Display the p-value
){
  # Initialize an empty string to store the result text
  text <- ""

  # Conditionally add effect size (eta square) to the result text
  if (effsize == TRUE) {
    text <- paste0(text,
                   "\u03b7^2^ = ", # Unicode for eta square
                   round(as.numeric(tidy_frame[term, "pes"]), 2)
    )
  }

  # Conditionally add 95% CI to the result text
  if (ci95 == TRUE) {
    text <- paste0(text,
                   ifelse(ci_lab == TRUE,
                          paste0(", 95% CI ["), " ["),
                   round(as.numeric(tidy_frame[term, "pes_ci95_lo"]), 2), ", ",
                   round(as.numeric(tidy_frame[term, "pes_ci95_hi"]), 2), "]"
    )
  }

  # Conditionally add F-score and degrees of freedom to the result text
  if (teststat == TRUE) {
    text <- paste0(text,
                   ", *F*(", tidy_frame[term, "Df"],
                   ", ", tidy_frame["Residuals", "Df"], ") = ",
                   round(as.numeric(tidy_frame[term, "F.value"], 2))
    )
  }

  # Conditionally add p-value to the result text
  if (pval == TRUE) {
    text <- paste0(text,
                   ", ",  report_pval_full(tidy_frame[term, "Pr..F."])
    )
  }

  # Return the generated text summary
  return(text)
}
