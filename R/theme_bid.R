
#' Our ggplot2 theme
#'
#' @description
#' \code{theme_bid} applies our ggplot2 theme to a ggplot2 plot object.
#'
#' @param tam_fonte Set font size used in the plot.
#' @param fonte Set main font used in the plot.
#'
#' @import ggplot2
#' @export

theme_bid <- function(tam_fonte = 14, fonte = "sans"){

  ggplot2::theme_minimal(base_size = tam_fonte, base_family = fonte) +
    ggplot2::theme(axis.text = ggplot2::element_text(color = "black"),
                   legend.text = ggplot2::element_text(),
                   legend.position = "top",
                   plot.subtitle = ggplot2::element_text(size = tam_fonte - 2),
                   plot.title = ggplot2::element_text(size = tam_fonte + 2, face = "bold"),
                   panel.grid = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(color = "gray90"),
                   axis.line.x = ggplot2::element_line()
    )
}
