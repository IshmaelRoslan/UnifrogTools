#' A ggplot theme for Unifrog employees based on theme_grey()
#' @param base_size The default font_size in pts
#' @param base_family The default font family for text
#' @param header_family The font family for title, axis titles, subtitle and caption.
#' @param base_line_size Default line width
#' @param base_rect_size Default rectangle line width
#' @param grid_col Colour of major gridlines
#' @param grid Should gridlines be shown? (Default = TRUE) False means hide both. X shows major X. Xx shows major and minor x. XY shows both.
#' @param axis_col = Colour of axes
#' @param axis show axes? TRUE/FALSE/'x'/'y'
#' @export
#' @import marquee
#' @import ggplot2

unifrog_theme <-
  function(base_size = 14,
           base_family = "Roboto",
           header_family = "Open Sans",
           base_line_size = 0.5,
           base_rect_size = 0.5,
           grid_col = unifrog_colors("lightgrey"), grid = TRUE,
           axis_col = unifrog_colors("lightgrey"), axis = FALSE) {
    ret <- theme_grey(
      base_size = base_size,
      base_family = base_family,
      base_line_size = base_line_size,
      base_rect_size = base_rect_size
    )
    ret <- ret +
      theme(
        line = element_line(colour = unifrog_colors("darkgrey"), linetype = "solid"),
        text = element_text(
          face = "plain",
          color = unifrog_colors("main"),
          family = base_family,
          size = base_size,
          lineheight = 20/14,
          hjust = 0.5
        ),
        title = element_text(
          face = "bold",
          color = unifrog_colors("main"),
          family = header_family,
          size = base_size,
          lineheight = 20/14,
          hjust = 0
        )
        ,
        rect = element_rect(fill = unifrog_colors("neutralgrey"))
      )

    ret <- ret + theme(legend.background = element_blank())
    ret <- ret + theme(legend.key = element_blank())

    if (inherits(grid, "character") | grid == TRUE) {
      ret <- ret + theme(panel.grid = element_line(color = grid_col, linewidth = base_line_size))
      ret <- ret + theme(panel.grid.major = element_line(color = grid_col, linewidth = base_line_size))
      ret <- ret + theme(panel.grid.minor = element_line(color = grid_col, linewidth = base_line_size * 0.75))

      if (inherits(grid, "character")) {
        if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x = element_blank())
        if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y = element_blank())
        if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x = element_blank())
        if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y = element_blank())
      }
    } else {
      ret <- ret + theme(panel.grid = element_blank())
      ret <- ret + theme(panel.grid.major = element_blank())
      ret <- ret + theme(panel.grid.major.x = element_blank())
      ret <- ret + theme(panel.grid.major.y = element_blank())
      ret <- ret + theme(panel.grid.minor = element_blank())
      ret <- ret + theme(panel.grid.minor.x = element_blank())
      ret <- ret + theme(panel.grid.minor.y = element_blank())
    }

    if (inherits(axis, "character") | axis == TRUE) {
      ret <- ret + theme(axis.line = element_line(color = axis_col, linewidth = base_line_size * 1.5))
      if (inherits(axis, "character")) {
        axis <- tolower(axis)
        if (regexpr("x", axis)[1] < 0) {
          ret <- ret + theme(axis.line.x = element_blank())
        } else {
          ret <- ret + theme(axis.line.x = element_line(color = axis_col, linewidth = base_line_size * 1.5))
        }
        if (regexpr("y", axis)[1] < 0) {
          ret <- ret + theme(axis.line.y = element_blank())
        } else {
          ret <- ret + theme(axis.line.y = element_line(color = axis_col, linewidth = base_line_size * 1.5))
        }
      } else {
        ret <- ret + theme(axis.line.x = element_line(color = axis_col, linewidth = base_line_size * 1.5))
        ret <- ret + theme(axis.line.y = element_line(color = axis_col, linewidth = base_line_size * 1.5))
      }
    } else {
      ret <- ret + theme(axis.line = element_blank())
    }

    ret <- ret +
      theme(
        plot.title.position = "plot",
        plot.title = element_text(size = rel(1.2)),
        plot.background = element_rect(fill = unifrog_colors("background")),
        plot.subtitle = element_text(face = "plain"),
        plot.caption = element_text(face = "plain", size = rel(0.8)),
        plot.caption.position = "plot",
        axis.ticks = element_blank(),
        axis.title = element_text(),
        axis.title.x = element_text(),
        axis.title.y = element_text(hjust = 1, angle = 90),
        axis.text = element_text(margin = margin(t = 0, r = 0), color = unifrog_colors("darkgrey")),
        axis.text.x = element_text(hjust = 0.5, vjust = 1, margin = margin(t = base_line_size)),
        axis.text.y = element_text(hjust = 1, vjust = 0.5, margin = margin(r = base_line_size)),
        strip.text = element_text(),
        legend.title = element_text(),
        legend.text = element_text(hjust = 0, vjust = 0.5, size = base_size, margin = margin(l = base_size / 2, r = base_size)),
        strip.background = element_rect(colour = unifrog_colors("neutralgrey")),
        panel.background = element_rect(fill = unifrog_colors("background")),
        panel.spacing.x = unit(base_size, "pt"),
        panel.spacing.y = unit(2 * base_size, "pt"),
        legend.position = "top",
        legend.justification = "left",
        legend.key.size = unit(base_size, "pt"),
      )
    return(ret)
  }

#' Set geom defaults for unifrog theme
#' @param point boolean set defaults for "point" geom
#' @param line boolean set defaults for "line" geom
#' @param bars boolean set defaults for "bar" and "col" geoms
#' @param text boolean set defaults for "text" and "label" geoms
#' @export
#' @import marquee
#' @import ggplot2

set_unifrog_geoms <- function(point = TRUE,
                              line = TRUE,
                              bars = TRUE,
                              text = TRUE) {
  lightgrey <- unifrog_colors("lightgrey")
  if (point) {
    update_geom_defaults("point", list(color = lightgrey))
  }

  if (line) {
    update_geom_defaults("line", list(color = lightgrey, linewidth = 1.5))
  }
  if (bars) {
    update_geom_defaults("col", list(fill = lightgrey, width = 0.6))
    update_geom_defaults("bar", list(fill = lightgrey, width = 0.6))
  }
  if (text) {
    update_geom_defaults("text", list(family = "Roboto", size = 12 / .pt))
    update_geom_defaults("label", list(family = "Roboto", size = 12 / .pt))
    update_geom_defaults("marquee", list(size = 12 / .pt, lineheight = 20 / 14, family = "Roboto"))
  }
}
