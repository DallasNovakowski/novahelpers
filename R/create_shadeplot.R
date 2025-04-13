#' Plot Summary Data with Distribution and Labels
#'
#' This function creates a summary plot combining raw data distributions and summary statistics. It renders a plot with violins/slabs (using \code{ggdist}), optional dot plots,
#' confidence interval ranges, sample size labels, and mean value text. The plot can also be faceted and/or have its coordinates flipped.
#'
#' @param raw_data A data.frame containing the raw observations.
#' @param summary_data A data.frame containing summary statistics. Must include the columns \code{mean}, \code{loci}, \code{upci}, \code{n} (sample size) and \code{min} (minimum value for label placement).
#' @param y_var A string specifying the name of the y variable (numeric) to be plotted.
#' @param x_var A string specifying the name of the x variable (typically categorical) to be plotted.
#' @param fill_var (Optional) A string naming a variable in the data for grouping by fill color.
#' @param facet_var (Optional) A string naming a variable on which to facet the plot.
#' @param x_title (Optional) A string to set as the x-axis label.
#' @param y_title (Optional) A string to set as the y-axis label.
#' @param fill_title (Optional) A string to set as the legend title for fill.
#' @param slabpha Numeric. The alpha transparency for the slab distribution. Default is 0.4.
#' @param slabjust Numeric. An adjustment parameter for the slab. Default is 1.
#' @param scaling Numeric. A scaling factor for the distribution. Default is 0.8.
#' @param base_palette A vector of colors to be used as the base palette. The default value is \code{cnc_palette}, which should be defined in your package or user workspace.
#' @param dots Logical. If \code{TRUE}, dot plots will be added to the distribution. Default is \code{FALSE}.
#' @param flipped Logical. If \code{TRUE}, the coordinate system is flipped. Default is \code{FALSE}.
#' @param mean_nudge_x Numeric. Horizontal nudge for the mean labels. Default is \code{-0.2}.
#' @param mean_nudge_y Numeric. Vertical nudge for the mean labels. Default is \code{0.5}.
#' @param stagger Numeric. A factor for dodging positions for elements in the plot. Default is \code{0.5}.
#' @param base_size Numeric. Base font size for the plot. Default is \code{16}.
#' @param axis_text_rel Numeric. Relative size for axis text. Default is \code{1.5}.
#' @param axis_title_rel Numeric. Relative size for axis titles. Default is \code{2}.
#' @param label_text_size Numeric. Font size for the mean text labels. Default is \code{5}.
#' @param n_label_rel Numeric. Relative size for the sample size (n) labels. Default is \code{0.8}.
#' @param dotsize Numeric. Dot size when dot plots are added. Default is \code{0.4}.
#' @param n_nudge Numeric. A nudge parameter for adjusting sample size labels. Default is \code{0.5}.
#' @param direction A string specifying the side for the slab ("left" or "right"). Default is \code{"left"}.
#'
#' @return A \code{ggplot2} object.
#'
#' @import ggplot2
#' @importFrom ggdist stat_slab stat_dots
#' @import colorspace
#' @import forcats
#' @import scales
#' @import cowplot
#' @import stringr
#' @importFrom grid unit
#'
#' @examples
#' \dontrun{
#'   library(ggplot2)
#'
#'   # Create some example raw data
#'   set.seed(123)
#'   data_raw <- data.frame(
#'     x = rep(letters[1:3], each = 20),
#'     y = rnorm(60),
#'     group = rep(c("A", "B", "C"), each = 20)
#'   )
#'
#'   # Create corresponding summary data
#'   data_summary <- data.frame(
#'     x = letters[1:3],
#'     mean = tapply(data_raw$y, data_raw$x, mean),
#'     loci = tapply(data_raw$y, data_raw$x, function(x) mean(x) - sd(x)),
#'     upci = tapply(data_raw$y, data_raw$x, function(x) mean(x) + sd(x)),
#'     n = tapply(data_raw$y, data_raw$x, length),
#'     min = tapply(data_raw$y, data_raw$x, min)
#'   )
#'
#'   # Define a custom palette if needed
#'   cnc_palette <- c("steelblue", "tomato", "seagreen")
#'
#'   # Generate the plot
#'   p <- create_shadeplot(
#'         raw_data = data_raw,
#'         summary_data = data_summary,
#'         y_var = "y",
#'         x_var = "x",
#'         fill_var = "group",
#'         x_title = "Category",
#'         y_title = "Value",
#'         fill_title = "Group",
#'         base_palette = cnc_palette
#'       )
#'   print(p)
#' }
#'
#' @export
create_shadeplot <- function(raw_data, summary_data, y_var, x_var,
                         fill_var = NULL, facet_var = NULL,
                         x_title = NULL, y_title = NULL, fill_title = NULL,
                         slabpha = 0.4,
                         slabjust = 1,
                         scaling = 0.8,
                         base_palette = cnc_palette,
                         dots = FALSE,
                         flipped = FALSE,
                         mean_nudge_x = -0.2,
                         mean_nudge_y = 0.5,
                         stagger = 0.5,
                         base_size = 16,
                         axis_text_rel = 1.5,
                         axis_title_rel = 2,
                         label_text_size = 5,
                         n_label_rel = 0.8,
                         dotsize = 0.4,
                         n_nudge = 0.5,
                         direction = "left") {
  
  # ---- VALIDATION & PREPARATION ----
  
  # Ensure summary_data has the required columns.
  required_cols <- c("mean", "loci", "upci")
  if (!all(required_cols %in% colnames(summary_data))) {
    stop("Summary data must contain 'mean', 'loci', and 'upci' columns.")
  }
  # Check for columns used in sample size labels.
  if (!("n" %in% colnames(summary_data)) || !("min" %in% colnames(summary_data))) {
    stop("Summary data must also contain 'n' and 'min' columns for sample labels.")
  }
  
  # Create a contrasting color palette for outlines/text.
  contrast_palette <- colorspace::darken(base_palette, amount = 0.5, space = "HLS")
  
  # Reorder the x-axis factor by mean value in summary_data for visual clarity.
  summary_data[[x_var]] <- forcats::fct_reorder(summary_data[[x_var]], summary_data$mean, .na_rm = TRUE)
  
  # Apply the same factor level ordering to raw_data.
  mean_order <- levels(summary_data[[x_var]])
  raw_data[[x_var]] <- factor(raw_data[[x_var]], levels = mean_order, ordered = TRUE)
  
  # ---- STYLING TWEAKS ----
  
  # Default positioning for n labels.
  n_location <- min(summary_data$min, na.rm = TRUE)
  
  # Range of y-axis values from raw data.
  y_min <- min(raw_data[[y_var]], na.rm = TRUE)
  y_max <- max(raw_data[[y_var]], na.rm = TRUE)
  
  # Adjust nudges and layout for flipped orientation.
  if (flipped) {
    mean_nudge_x <- -mean_nudge_x
    mean_nudge_y <- -mean_nudge_y
    direction <- "right"
    hbump <- -0.1
    vbump <- 0
  } else {
    stagger <- stagger * 1.5
    hbump <- 0.5
    vbump <- 0
  }
  
  # ---- INITIAL PLOT SETUP ----
  
  p <- ggplot2::ggplot(raw_data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
    ggplot2::theme_bw(base_size = base_size) +
    theme_basic() +
    ggplot2::theme(
      legend.position = "top",
      legend.justification = "center",
      axis.text = ggplot2::element_text(size = ggplot2::rel(axis_text_rel)),
      legend.key.size = grid::unit(1.5 * axis_text_rel, "lines"),
      axis.title = ggplot2::element_text(size = ggplot2::rel(axis_title_rel), face = "bold"),
      legend.title = ggplot2::element_text(size = ggplot2::rel(axis_title_rel), face = "bold", hjust = 0.5),
      legend.text = ggplot2::element_text(size = ggplot2::rel(axis_text_rel), hjust = 0.5),
      strip.text = ggplot2::element_text(size = ggplot2::rel(axis_text_rel))
    ) +
    ggplot2::scale_fill_manual(values = base_palette, labels = function(x) stringr::str_wrap(x, width = 5)) +
    # ggplot2::scale_fill_continuous(from = "white", to = "black", aesthetics = "fill") +
    ggplot2::scale_color_manual(values = contrast_palette) +
    ggplot2::guides(fill = "none")  # Hide redundant legend
  
  # ---- VIOLIN (SLAB) DISTRIBUTIONS ----
  
  if (!is.null(fill_var)) {
    p <- p + ggdist::stat_slab(
      ggplot2::aes(fill = .data[[fill_var]]),
      alpha = slabpha, adjust = slabjust, side = direction, scale = scaling,
      normalize = "panels", height = 0.2,
      position = ggplot2::position_dodge(width = stagger)
    )
  } else {
    p <- p + ggplot2::aes(fill = contrast_palette[[1]]) +
      ggdist::stat_slab(
        alpha = slabpha, adjust = slabjust, side = direction, scale = scaling,
        normalize = "panels", height = 0.2,
        position = ggplot2::position_dodge(width = stagger),
        show.legend = FALSE
      )
  }
  
  # ---- DOT PLOTS (Optional) ----
  
  if (dots) {
    
    if (!is.null(fill_var)) {
      p <- p + ggdist::stat_dots(        ggplot2::aes(fill = .data[[fill_var]]),
        alpha = 0.35, side = direction, scale = scaling, binwidth = 1, dotsize = dotsize,
        position = ggplot2::position_dodge(width = stagger), show.legend = FALSE
      )
    } else {
      p <- p + ggdist::stat_dots(alpha = 0.35, side = direction, scale = scaling, 
                                 binwidth = 1, dotsize = dotsize,
                                 position = ggplot2::position_dodge(width = stagger), 
                                 show.legend = FALSE
      )
    }
    
    
    
    
    
    
  }
  
  # ---- MEAN + CONFIDENCE INTERVAL POINTS ----
  
  p <- p + ggplot2::geom_pointrange(
    data = summary_data,
    ggplot2::aes(x = .data[[x_var]], y = mean, ymin = loci, ymax = upci),
    inherit.aes = FALSE,
    show.legend = FALSE,
    position = ggplot2::position_dodge2(width = stagger),
    color = "grey30"
  )
  
  # ---- SAMPLE SIZE LABEL (n =) ----
  
  p <- p + ggplot2::geom_label(
    data = summary_data,
    ggplot2::aes(x = .data[[x_var]], y = n_location, label = paste("n =", n)),
    inherit.aes = FALSE,
    size = label_text_size * n_label_rel,
    position = ggplot2::position_dodge2(width = stagger),
    hjust = hbump,
    vjust = vbump,
    fill = "grey50", color = "grey10",
    label.size = 0, alpha = 0.1
  )
  
  # ---- MEAN TEXT LABEL ----
  
  if (!is.null(fill_var)) {
    p <- p + ggplot2::geom_text(
      data = summary_data,
      ggplot2::aes(
        x = .data[[x_var]], y = mean, label = round(mean, 1),
        colour = .data[[fill_var]]
      ),
      inherit.aes = FALSE,
      size = label_text_size, show.legend = FALSE,
      position = ggplot2::position_dodge2(width = stagger)
    )
  } else {
    p <- p + ggplot2::geom_text(
      data = summary_data,
      ggplot2::aes(x = .data[[x_var]], y = mean, label = round(mean, 1)),
      color = "grey30", inherit.aes = FALSE, size = label_text_size,
      position = ggplot2::position_dodge2(width = stagger)
    )
  }
  
  # ---- FACETING (Optional) ----
  
  if (!is.null(facet_var)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]), ncol = 1) +
      cowplot::panel_border()
  }
  
  # ---- FINAL COORDINATE + AXIS TWEAKS ----
  
  if (flipped) {
    p <- p +
      ggplot2::scale_y_continuous(
        limits = c(y_min, y_max),
        expand = ggplot2::expansion(mult = c(0, 0)),
        oob = scales::rescale_none,
        breaks = scales::pretty_breaks(n = 3)
      ) +
      ggplot2::coord_flip() +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.major.x = ggplot2::element_line(
                         color = "grey80",
                         size = 0.5
                       )
                       
                       )
  }
  
  # ---- CUSTOM AXIS + FILL LABELS ----
  
  if (!is.null(x_title)) {
    p <- p + ggplot2::xlab(x_title)
  }
  if (!is.null(y_title)) {
    p <- p + ggplot2::ylab(y_title)
  }
  if (!is.null(fill_title)) {
    p <- p + ggplot2::labs(fill = fill_title)
  }
  
  return(p)
}



#' @export
adjust_flipped_layout <- function(flipped, mean_nudge_x, mean_nudge_y, stagger) {
  if (flipped) {
    list(
      mean_nudge_x = -mean_nudge_x,
      mean_nudge_y = -mean_nudge_y,
      direction = "right",
      hbump = -0.1,
      vbump = 0,
      stagger = stagger,
      contraction = c(0, 0)
    )
  } else {
    list(
      mean_nudge_x = mean_nudge_x,
      mean_nudge_y = mean_nudge_y,
      direction = "left",
      hbump = 0.5,
      vbump = 0,
      stagger = stagger * 1.5,
      contraction = c(0, 0)
    )
  }
}



# example
# df <- palmerpenguins::penguins |> na.omit()
# 
# 
# df <- df[!(df$species == "Adelie" & df$sex == "male"), ]
# 
# # Example usage
# group_var_list <- c("species", "sex", "island")
# summarization_var <- "flipper_length_mm"
# 
# flipper_summaries <- run_grouped_summaries(df, summarization_var, group_var_list)
# 
# # Access the summary for a specific combination
# flipper_summaries[["species"]] # Summary for "species"
# flipper_summaries[["species_sex"]] # Summary for "sex" and "species"
# flipper_summaries[["species_sex_island"]] # Summary for "sex", "species", and "island"
# 
# 
# 
# 
# 
# # Generate plots for different levels of grouping
# create_shadeplot(df, flipper_summaries[["species"]], y_var = "flipper_length_mm", "species")
# create_shadeplot(df, flipper_summaries[["species_sex"]], y_var = "flipper_length_mm", "species", "sex")
# create_shadeplot(df, flipper_summaries[["species_sex_island"]],
#              y_var = "flipper_length_mm", "species", "sex", "island",
#              flipped = TRUE, scaling = .6, dots = TRUE, dotsize = .6)

