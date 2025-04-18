% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_barplot.R
\name{create_barcount}
\alias{create_barcount}
\title{Create a Bar Count Plot}
\usage{
create_barcount(
  data,
  x_var,
  y_var,
  fill_var,
  filter_type = "prop",
  x_label = "Webinar Event",
  y_label = "Proportion of Same-Year Respondents",
  fill_label = "Year",
  color_palette = cnc_palette,
  theme_custom = my_theme,
  label_wrap_width = 20
)
}
\arguments{
\item{data}{A data frame containing the variables to plot.}

\item{x_var}{Character string specifying the variable for the x-axis.}

\item{y_var}{Character string specifying the variable for the y-axis.}

\item{fill_var}{Character string specifying the variable used to color bars.}

\item{filter_type}{Character string; the filter value for the column \code{type}. Defaults to \code{"prop"}.}

\item{x_label}{Character string for the x-axis label. Defaults to \code{"Webinar Event"}.}

\item{y_label}{Character string for the y-axis label. Defaults to \code{"Proportion of Same-Year Respondents"}.}

\item{fill_label}{Character string for the fill (legend) label. Defaults to \code{"Year"}.}

\item{color_palette}{A character vector of hex codes used for fill and color scales. Defaults to \code{cnc_palette}.}

\item{theme_custom}{A ggplot2 theme object (or function returning a theme). Defaults to \code{my_theme}.}

\item{label_wrap_width}{Numeric; width for wrapping long x-axis labels. Defaults to 20.}
}
\value{
A ggplot2 plot object.
}
\description{
This function creates a bar count plot from input data. It allows programmatic specification of the x,
y, and fill variables via character strings. In addition to a standard bar plot,
it displays conditional numeric labels (including percentage and counts) based on the data type.
}
\examples{
\dontrun{
  # Example usage with a sample dataframe:
  data <- data.frame(
    type = rep(c("prop", "count"), each = 10),
    event = rep(letters[1:10], 2),
    proportion = runif(20, 0, 1),
    year = rep(c(2020, 2021), each = 10)
  )

  # Define your palette and custom theme (if not already defined)
  cnc_palette <- c("#78AAA9", "#FFDB6E", "#604D75", "#A03030",
                   "#3B8EAD", "#8E4921", "#CC79A7", "#B3D7E5")
  my_theme <- ggplot2::theme_minimal()

  p <- create_barcount(data,
                       x_var = "event",
                       y_var = "proportion",
                       fill_var = "year",
                       filter_type = "prop",
                       x_label = "Event",
                       y_label = "Proportion",
                       fill_label = "Year",
                       color_palette = cnc_palette,
                       theme_custom = my_theme,
                       label_wrap_width = 20)
  print(p)
}

}
