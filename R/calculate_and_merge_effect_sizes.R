#' This function takes an emmeans object and a model fit and returns a dataframe with effect sizes.
#'
#' @param emmeans_obj An emmeans object created using data.frame(emmeans::emmeans(model)$contrasts)
#' @param model A model fit
#' @return A dataframe with effect sizes
#' @importFrom stats sigma df.residual df
#' @importFrom dplyr select
#'
#' @export

# This function calculates and merges effect sizes from an emmeans object and a model fit.
# It then returns a dataframe with the calculated effect sizes.

# Function Input:
#   emmeans_obj: An emmeans object created using data.frame(emmeans::emmeans(model)$contrasts).
#   model: A model fit.

calculate_and_merge_effect_sizes <- function(emmeans_obj, model) {
  # Access variables from emmeans_obj

  # Create a dataframe of the contrasts from the emmeans_obj
  contrasts <- data.frame(emmeans_obj$contrasts)

  # Calculate effect sizes using the eff_size function from the emmeans package
  # This function calculates pairwise effect sizes with confidence intervals.
  emmean_d <- data.frame(emmeans::eff_size(
    emmeans_obj,
    method = "pairwise",  # Method for effect size calculation
    sigma = sigma(model),  # Residual standard error from the model
    edf = df.residual(model)  # Degrees of freedom for residuals
  ))

  # Combine the contrast and effect size data into a single dataframe
  combined_dataframe <- data.frame(contrasts, emmean_d)

  # Access additional variables from the emmeans_obj
  contrast.1 <- emmeans_obj$contrast.1
  df.1 <- emmeans_obj$df.1
  effect.size <- emmeans_obj$effect.size
  lower.CL <- emmeans_obj$lower.CL
  upper.CL <- emmeans_obj$upper.CL
  SE.1 <- emmeans_obj$SE.1
  p.value <- emmeans_obj$p.value

  # Rename some columns for clarity using the dplyr package
  combined_dataframe <- combined_dataframe %>%
    select(-contrast.1, -df.1) %>%  # Remove unnecessary columns
    dplyr::rename(d = effect.size,  # Rename columns for effect size and confidence intervals
                  d_ci_low = lower.CL,
                  d_ci_high = upper.CL,
                  d_se = SE.1,
                  df_error = df,  # Rename degrees of freedom for error
                  p = p.value  # Rename p-value
    )

  # Return the combined dataframe with effect size results
  return(combined_dataframe)
}
