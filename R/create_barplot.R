#' Create a Bar Count Plot
#'
#' This function creates a bar count plot from input data. It allows programmatic specification of the x,
#' y, and fill variables via character strings. In addition to a standard bar plot,
#' it displays conditional numeric labels (including percentage and counts) based on the data type.
#'
#' @param data A data frame containing the variables to plot.
#' @param x_var Character string specifying the variable for the x-axis.
#' @param y_var Character string specifying the variable for the y-axis.
#' @param fill_var Character string specifying the variable used to color bars.
#' @param filter_type Character string; the filter value for the column \code{type}. Defaults to \code{"prop"}.
#' @param x_label Character string for the x-axis label. Defaults to \code{"Webinar Event"}.
#' @param y_label Character string for the y-axis label. Defaults to \code{"Proportion of Same-Year Respondents"}.
#' @param fill_label Character string for the fill (legend) label. Defaults to \code{"Year"}.
#' @param color_palette A character vector of hex codes used for fill and color scales. Defaults to \code{cnc_palette}.
#' @param theme_custom A ggplot2 theme object (or function returning a theme). Defaults to \code{my_theme}.
#' @param label_wrap_width Numeric; width for wrapping long x-axis labels. Defaults to 20.
#'
#' @return A ggplot2 plot object.
#'
#' @import ggplot2
#' @importFrom dplyr filter
#' @importFrom forcats fct_reorder
#' @importFrom rlang sym !!
#' @importFrom stringr str_wrap
#'
#' @examples
#' \dontrun{
#'   # Example usage with a sample dataframe:
#'   data <- data.frame(
#'     type = rep(c("prop", "count"), each = 10),
#'     event = rep(letters[1:10], 2),
#'     proportion = runif(20, 0, 1),
#'     year = rep(c(2020, 2021), each = 10)
#'   )
#'
#'   # Define your palette and custom theme (if not already defined)
#'   cnc_palette <- c("#78AAA9", "#FFDB6E", "#604D75", "#A03030",
#'                    "#3B8EAD", "#8E4921", "#CC79A7", "#B3D7E5")
#'   my_theme <- ggplot2::theme_minimal()
#'
#'   p <- create_barcount(data,
#'                        x_var = "event",
#'                        y_var = "proportion",
#'                        fill_var = "year",
#'                        filter_type = "prop",
#'                        x_label = "Event",
#'                        y_label = "Proportion",
#'                        fill_label = "Year",
#'                        color_palette = cnc_palette,
#'                        theme_custom = my_theme,
#'                        label_wrap_width = 20)
#'   print(p)
#' }
#'
#' @export
create_barcount <- function(data, 
                            x_var, 
                            y_var, 
                            fill_var, 
                            filter_type = "prop", 
                            x_label = "Webinar Event", 
                            y_label = "Proportion of Same-Year Respondents", 
                            fill_label = "Year", 
                            color_palette = cnc_palette, 
                            theme_custom = my_theme, 
                            label_wrap_width = 20) {
  
  # Convert variables to symbols for flexible evaluation within ggplot2
  x_sym <- rlang::sym(x_var)
  y_sym <- rlang::sym(y_var)
  fill_sym <- rlang::sym(fill_var)
  
  # Filter data based on the filter_type specified
  filtered_data <- data %>% dplyr::filter(type == filter_type)
  
  # Build the plot
  p <- filtered_data %>%
    ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(!!x_sym, !!y_sym), 
                                 y = !!y_sym, 
                                 fill = !!fill_sym,
                                 label = round(!!y_sym, 2))) +
    ggplot2::geom_bar(stat = "identity", 
                      position = "dodge", 
                      na.rm = TRUE, 
                      width = 0.7, 
                      alpha = 0.6) +
    # Conditional text labels for missing or zero data
    ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(!!y_sym) | is.nan(!!y_sym) | !!y_sym == 0,
                                                   "No Data Available", ""),
                                    color = !!fill_sym, 
                                    y = ifelse(is.na(!!y_sym) | is.nan(!!y_sym), 0, !!y_sym) + 0.01),
                       size = 3, 
                       position = ggplot2::position_dodge(width = 0.7),
                       angle = 90, 
                       hjust = -0.05,
                       na.rm = TRUE) +
    # Add percentage labels
    ggplot2::geom_text(ggplot2::aes(label = ifelse(!is.na(!!y_sym) & !is.nan(!!y_sym) & !!y_sym != 0, 
                                                   paste0(round(!!y_sym * 100, 0), "%"), ""),
                                    y = !!y_sym),
                       color = "black",
                       size = 2.5, 
                       position = ggplot2::position_dodge(width = 0.8),
                       vjust = -0.5) +
    # Add count labels for type 'count'
    ggplot2::geom_text(data = data %>% dplyr::filter(type == "count"),
                       ggplot2::aes(label = ifelse(!is.na(!!y_sym) & !is.nan(!!y_sym) & !!y_sym != 0, 
                                                   paste0("n = ", round(!!y_sym, 0)), ""),
                                    y = 0.005),
                       color = "grey80",
                       size = 2.5, 
                       position = ggplot2::position_dodge(width = 0.4),
                       angle = 90, 
                       hjust = -0.2) +
    ggplot2::labs(x = x_label,
                  y = y_label,
                  fill = fill_label) +
    ggplot2::scale_colour_manual(values = color_palette, aesthetics = c("fill", "color")) + 
    ggplot2::guides(colour = "none") + 
    theme_custom +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, angle = 35, hjust = 0.5),
                   axis.title = ggplot2::element_text(size = 12)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = label_wrap_width)) +
    ggplot2::coord_cartesian(ylim = c(0, 1))
  
  return(p)
}

#' Create a Bar Plot with Confidence Intervals and Text Labels
#'
#' This function builds a bar plot displaying the mean values along with their confidence intervals.
#' It further adds text labels for means and sample sizes, allowing programmatic specification of the variable names.
#'
#' @param data A data frame containing the variables to plot.
#' @param x_var Character string specifying the variable for the x-axis.
#' @param y_var Character string specifying the variable representing the mean. Defaults to \code{"mean"}.
#' @param fill_var Character string specifying the variable used for fill color. Defaults to \code{"year"}.
#' @param group_var Character string specifying the variable to group for text color. Defaults to \code{"year"}.
#' @param lower_ci Character string specifying the variable for the lower bound of the confidence interval. Defaults to \code{"loci"}.
#' @param upper_ci Character string specifying the variable for the upper bound of the confidence interval. Defaults to \code{"upci"}.
#' @param label_var Character string specifying the variable for the labels on the bars. Defaults to \code{"mean"}.
#' @param responded_var Character string specifying the variable representing sample size or response count. Defaults to \code{"responded"}.
#' @param cnc_palette A character vector of colors used for fill and color scales.
#' @param x_label Optional character string for the x-axis label.
#' @param y_label Optional character string for the y-axis label.
#' @param limits_y Numeric vector of length two specifying the y-axis limits. Defaults to \code{c(0, 4)}.
#' @param my_theme A ggplot2 theme object (or function returning a theme) to be applied to the plot.
#'
#' @return A ggplot2 plot object.
#'
#' @import ggplot2
#' @importFrom dplyr filter
#' @importFrom forcats fct_reorder
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'   # Example usage with sample data:
#'   data <- data.frame(
#'     x = rep(letters[1:5], each = 2),
#'     mean = runif(10, 1, 3),
#'     loci = runif(10, 0.5, 1),
#'     upci = runif(10, 3, 3.5),
#'     responded = sample(50:100, 10, replace = TRUE),
#'     year = rep(c("2020", "2021"), 5)
#'   )
#'
#'   cnc_palette <- c("#78AAA9", "#FFDB6E", "#604D75", "#A03030",
#'                    "#3B8EAD", "#8E4921", "#CC79A7", "#B3D7E5")
#'   my_theme <- ggplot2::theme_minimal()
#'
#'   p <- create_barplot(data,
#'                       x_var = "x",
#'                       y_var = "mean",
#'                       fill_var = "year",
#'                       group_var = "year",
#'                       lower_ci = "loci",
#'                       upper_ci = "upci",
#'                       label_var = "mean",
#'                       responded_var = "responded",
#'                       cnc_palette = cnc_palette,
#'                       x_label = "Category",
#'                       y_label = "Mean Value",
#'                       limits_y = c(0, 4),
#'                       my_theme = my_theme)
#'   print(p)
#' }
#'
#' @export
create_barplot <- function(data, 
                           x_var, 
                           y_var = "mean", 
                           fill_var = "year", 
                           group_var = "year",
                           lower_ci = "loci", 
                           upper_ci = "upci", 
                           label_var = "mean", 
                           responded_var = "responded", 
                           cnc_palette, 
                           x_label = NULL, 
                           y_label = NULL, 
                           limits_y = c(0, 4),
                           my_theme) {
  
  ggplot2::ggplot(data = data, 
                  ggplot2::aes(x = forcats::fct_reorder(.data[[x_var]], .data[[y_var]]), 
                               y = .data[[y_var]], 
                               fill = .data[[fill_var]])) +
    # Bar plot
    ggplot2::geom_bar(stat = "identity",
                      position = "dodge",
                      alpha = 0.6,
                      size = 0.2,
                      width = 0.7) +
    # Confidence interval error bars
    ggplot2::geom_pointrange(ggplot2::aes(ymin = .data[[lower_ci]], 
                                          ymax = .data[[upper_ci]]),
                             fatten = 1,
                             alpha = 0.75,
                             show.legend = FALSE,
                             position = ggplot2::position_dodge(width = 0.7)) +
    # Conditional text for missing/zero data
    ggplot2::geom_text(ggplot2::aes(label = ifelse(.data[[y_var]] == 0 | .data[[responded_var]] == 0, 
                                                   "No Data Available", ""),
                                    color = .data[[group_var]],
                                    y = ifelse(.data[[y_var]] == 0 | .data[[responded_var]] == 0, 1, .data[[y_var]]) + 0.1),
                       size = 2.5,
                       position = ggplot2::position_dodge(width = 0.7),
                       angle = 90,
                       hjust = 0) +
    # Labels for the mean values
    ggplot2::geom_text(ggplot2::aes(label = ifelse(.data[[y_var]] == 0 | .data[[responded_var]] == 0, 
                                                   '', round(.data[[label_var]], 1))),
                       color = "black",
                       size = 2,
                       position = ggplot2::position_dodge(width = 0.7),
                       vjust = 5) +
    # Sample size label
    ggplot2::geom_text(ggplot2::aes(x = forcats::fct_reorder(.data[[x_var]], .data[[responded_var]]),
                                    y = 1.3,
                                    label = ifelse(!is.na(.data[[responded_var]]) & 
                                                     !is.nan(.data[[responded_var]]) & 
                                                     .data[[responded_var]] != 0,
                                                   paste0("n = ", round(.data[[responded_var]], 0)), "")),
                       position = ggplot2::position_dodge(width = 0.7),
                       color = "grey80",
                       size = 2.75,
                       angle = 90) +
    ggplot2::guides(colour = "none") +
    ggplot2::scale_colour_manual(values = cnc_palette, 
                                 aesthetics = c("fill", "color")) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)),
                                limits = limits_y) +
    ggplot2::labs(x = x_label, y = y_label) +
    my_theme +
    ggplot2::theme(axis.title = ggplot2::element_text(size = 12))
}
