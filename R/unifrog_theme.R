#' A ggplot theme for Unifrog employees based on theme_classic()
#' @param base_size The default font_size in pts
#' @export
#' @import ggtext
#' @import ggplot2

unifrog_theme <- function(base_size = 11) {
  base_size <- base_size
  theme_classic() +
    theme(
      line = element_line(
        colour = unifrog_colors("grey"),
        linewidth = 0.5,
        linetype = "solid"
      ),
      title = element_markdown(
        face = "bold",
        color = unifrog_colors("main"),
        family = "Open Sans",
        size = base_size * 1.2,
        hjust = 0
      ),
      rect = element_rect(fill = unifrog_colors("background")),
      plot.title.position = "plot",
      plot.title = element_markdown(),
      plot.subtitle = element_markdown(),
      axis.line = element_line(
        colour = unifrog_colors("grey"),
        linewidth = 0.5,
        linetype = "solid"
      ),
      axis.ticks = element_line(
        colour = unifrog_colors("grey"),
        linewidth = 1,
        linetype = "solid"
      ),
      axis.text.x = element_markdown(
        color = unifrog_colors("main"),
        family = "Open Sans",
        size = base_size,
        hjust = 0.5
      ),
      axis.text.y = element_markdown(
        color = unifrog_colors("main"),
        family = "Open Sans",
        size = base_size,
        hjust = 0
      ),
      axis.title.x = element_markdown(
        face = "bold",
        color = unifrog_colors("main"),
        family = "Open Sans",
        size = base_size * 1.2,
        hjust = 0
      ),
      axis.title.y = element_markdown(
        face = "bold",
        color = unifrog_colors("main"),
        family = "Open Sans",
        size = base_size * 1.2,
        vjust = 1
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      strip.background = element_rect(fill = unifrog_colors("background"), colour = unifrog_colors("grey")),
      strip.text = element_markdown(face = "bold"),
      panel.background = element_rect(fill = unifrog_colors("background"))
    )
}
