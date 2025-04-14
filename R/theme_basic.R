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




#' Socia color palette
#'
#' A character vector of hex color codes used as the Socia default palette.
#'
#' @export
socia_palette <- c( "#78AAA9", "#FFDB6E", "#604D75", "#A03030",
                   "#3B8EAD", "#8E4921", "#CC79A7", "#B3D7E5")




#' CNC color palette
#'
#' A character vector of hex color codes used as the cnc default palette.
#'
#' @export
cnc_palette <- c(
  "#EA1525", # Red - KEEP
  "#547B81", # Dark Teal - KEEP
  "#20B2AA", # Light Sea Green - KEEP
  "#F2A900", # Golden Yellow - KEEP
  "#B07AA1", # Muted Purple
  "#94C973", # Soft Green
  "#E17C05", # Orange-Brown
  "#7F3C8D", # Dark Plum
  "#999999", # Neutral Gray
  "#D1BCA3", # Sand
  "#1170AA", # Rich Blue
  "#DD8452", # Warm Tan
  "#8C564B", # Brown
  "#84B1ED", # Light Periwinkle
  "#4C4C4C" # Charcoal
)




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





#' @export
theme_basic <- function() {
  cowplot::theme_half_open() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_text(margin = ggplot2::unit(c(3, 0, 0, 0), "mm")),
      axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 3, 0, 0), "mm")),
      legend.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggtext::element_markdown(),
      panel.grid.major.y = ggplot2::element_line(
        color = "grey80",
        size = 0.5
        )
    )
}
