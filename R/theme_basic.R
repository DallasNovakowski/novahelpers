# ************t*************************************
#                     Themes
# *************************************************

#' Create my default theme, borrowing from cowplot - https://github.com/wilkelab/cowplot/blob/1f35f385b0631c74daecaf6366725a8573242bde/R/themes.R#L11

#' library(ggplot2)
#' library(ggtext)
#' library(cowplot)
#' @importFrom cowplot theme_half_open
#' @importFrom ggtext element_markdown
#' @importFrom ggplot2 theme element_text
#' @return A ggplot2 
# @export

theme_basic <- function() {
  cowplot::theme_half_open() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(face = "bold"),
      legend.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggtext::element_markdown()
    )
}

