#' A clean default ggplot2 theme
#'
#' `theme_basic()` creates a clean plot theme inspired by `cowplot::theme_half_open()`,
#' with bold axis/legend titles and markdown support for subtitles.
#'
#' @return A ggplot2 theme object.
#' @export
#'
#' @importFrom cowplot theme_half_open
#' @importFrom ggtext element_markdown
#' @importFrom ggplot2 theme element_text element_line
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   theme_basic()




#' Nova color palette
#'
#' A character vector of hex color codes used as the Nova default palette.
#'
#' @export
nova_palette <- c( "#78AAA9", "#FFDB6E", "#604D75", "#A03030",
                   "#3B8EAD", "#8E4921", "#CC79A7", "#B3D7E5")



# nova_palette <- c("#78AAA9", "#FFDB6E")
# 
# nova_palette <- c(
#   "#78AAA9",  # muted teal (original)
#   "#FFDB6E",  # soft yellow (original)
#   "#E69F00",  # orange (Okabe-Ito)
#   "#56B4E9",  # sky blue (Okabe-Ito)
#   "#009E73",  # green (Okabe-Ito)
#   "#F0E442",  # yellow (distinct from #FFDB6E)
# 
# )




#https://davidmathlogic.com/colorblind/#%231E716F-%23E8AE00


# 
# nova_palette_grey_friendly <- c("#598D8B","#FFBF00", "#E5E1EA")
# 
# 
# grayscale_palette <- colorspace::desaturate(nova_palette_grey_friendly) ## grayscale palette
# 
# 
# #https://davidmathlogic.com/colorblind/#%231E716F-%23E8AE00
# 
# other_colors <- c( "#1E716F", # Dark teal
#                    "#E8AE00", # Dark yellow
#                    "#604D75", # dark desaturated purple
#                    "#412357", # dark purple
#                    "#A03030", #maroon
#                    "#3B8EAD", # light blue
#                    "#8E4921",  # Terracotta
#                    "#B3D7E5" # extra-light blue
# 
# 
# )



# grayscale_palette <- colorspace::desaturate(nova_palette_grey_friendly) ## grayscale palette






theme_basic <- function() {
  cowplot::theme_half_open() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_text(margin = ggplot2::unit(c(3, 0, 0, 0), "mm")),
      axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 3, 0, 0), "mm")),
      legend.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggtext::element_markdown(),
      panel.grid.major.y = element_line(
        color = "grey80",
        size = 0.5
        )
    )
}
