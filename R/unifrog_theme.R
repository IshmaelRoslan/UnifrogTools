#' A ggplot theme for Unifrog employees based on theme_classic()
#' @param base_size The default font_size in pts
#' @export
#' @import ggtext
#' @import ggplot2

unifrog_theme <- function(base_size = 10) {
  base_size <- base_size
  theme_classic() +
    theme(
      line = element_line(
        colour = unifrog_colors("darkgrey"),
        linewidth = 0.5,
        linetype = "solid"
      ),
      text = element_text(
        face = "plain",
        color = unifrog_colors("main"),
        family = "Open Sans",
        size = base_size,
        hjust = 0
      ),
      title = element_text(
        face = "bold",
        color = unifrog_colors("main"),
        family = "Open Sans",
        size = base_size * 1.6,
        hjust = 0
      ),
      rect = element_rect(fill = unifrog_colors("background")),
      plot.title.position = "plot",
      plot.title = element_text(),
      plot.subtitle = element_text(size = base_size, face = "plain", color = unifrog_colors('darkgrey')),
      plot.caption = element_text(size = base_size, hjust = 1),
      axis.line = element_line(
        colour = unifrog_colors("darkgrey"),
        linewidth = 0.5,
        linetype = "solid"
      ),
      axis.ticks = element_line(
        colour = unifrog_colors("darkgrey"),
        linewidth = 1,
        linetype = "solid"
      ),
      axis.text.x = element_text(
        color = unifrog_colors("main"),
        family = "Open Sans",
        size = base_size,
        hjust = 0.5
      ),
      axis.text.y = element_text(
        color = unifrog_colors("main"),
        family = "Open Sans",
        size = base_size,
        hjust = 0
      ),
      axis.title.x = element_text(
        face = "bold",
        color = unifrog_colors("main"),
        family = "Open Sans",
        size = base_size * 1.2,
        hjust = 0
      ),
      axis.title.y = element_text(
        face = "bold",
        color = unifrog_colors("main"),
        family = "Open Sans",
        size = base_size * 1.2,
        hjust = 1
      ),
      legend.position = "top",
      legend.direction = "horizontal",
      strip.background = element_rect(fill = unifrog_colors("neutralgrey"), colour = unifrog_colors("darkgrey")),
      strip.text = element_text(face = "bold"),
      panel.background = element_rect(fill = unifrog_colors("background")),
      legend.text = element_text(size = base_size)
    )
}
