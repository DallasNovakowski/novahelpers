#' In-text and in-plot reporting of pairwise comparisons
#'
#'@import magrittr
#'
#' @param tidy_frame Your tidy pairwise comparison result
#' @param italicize Whether the symbols should be italicized
#' @param ci Whether to include confidence intervals
#' @param ci_lab Whether to include the label for confidence intervals
#' @param point Whether to include the point estimate for cohen's d
#' @param pval  Whether to include the p value
#' @param pval_comma  Whether to include a leading comma for p values
#' @param teststat Whether to include the test statistic
#' @return A character string of stats
#'
#' @export

# This function generates a text summary based on user-specified options using a tidy t-test dataframe.
report_tidy_t <- function(tidy_frame,
                          italicize = TRUE,
                          ci = TRUE,
                          ci_lab = TRUE,
                          teststat = FALSE,
                          point = TRUE,
                          pval = TRUE,
                          pval_comma = TRUE
){
  # Initialize an empty string to store the result text
  text <- ""

  # Conditionally add effect size (d) to the result text
  if (point == TRUE) {
    text <- paste0(text,
                   ifelse(italicize == TRUE, "*d* = ", "d = "), # Italicized or not
                   round(tidy_frame$d, 2) # Rounded d value
    )
  }

  # Conditionally add 95% CI to the result text
  if (ci == TRUE) {
    text <- paste0(text,
                   ifelse(ci_lab == TRUE,
                          paste0(", 95% CI [",
                                 round(tidy_frame$d_ci_low, 2), ", ",
                                 round(tidy_frame$d_ci_high, 2), "]"), # Label and rounded CI values
                          paste0(" [",
                                 round(tidy_frame$d_ci_low, 2), ", ",
                                 round(tidy_frame$d_ci_high, 2), "]") # Only rounded CI values
                   )
    )
  }

  # Conditionally add t-statistic and degrees of freedom to the result text
  if (teststat == TRUE) {
    text <- paste0(text,
                   ", *t* (", round(tidy_frame$df_error, 2), ") = ", round(tidy_frame$t, 2) # t-statistic and df
    )
  }

  # Conditionally add p-value to the result text
  if (pval == TRUE) {
    text <- paste0(text,
                   ifelse(pval_comma == TRUE,
                          ifelse(italicize == TRUE, ", *p* ", ", p "),
                          ifelse(italicize == TRUE, "*p* ", "p ")  # Italicized or not
                   ),
                   ifelse(tidy_frame$p < .001, "< .001", # Formatting based on p-value
                          ifelse(tidy_frame$p > .01,
                                 paste("=", tidy_frame$p %>% round(2)),
                                 paste("=", tidy_frame$p %>% round(3))
                          )
                   )
    )
  }

  # Return the generated text summary
  return(text)
}
