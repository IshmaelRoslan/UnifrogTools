#' Unifrog colour palette
#'
#' The palette is based on the colours for Unifrog tools as shown in the tool.tool_hex on Azure.
#' Use `scale_color_unifrog_d` for *discrete* categories and
#' `scale_color_unifrog_c` for a *continuous* scale.
#'
#' @inheritParams palette_unifrog
#' @inheritParams unifrog_palettes
#'
#' @param discrete Boolean indicating whether color aesthetic is discrete or not.
#' @param aesthetics A vector of names of the aesthetics that this scale
#'   should be applied to (e.g., `c('color', 'fill')`).
#' @param ... Additional arguments passed to `discrete_scale()` when `discrete`
#'   is `TRUE` or to `scale_color_gradientn()` when `discrete` is `FALSE`.
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_boxplot() +
#'   scale_fill_unifrog_d() +
#'   theme_bw()
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violin() +
#'   scale_fill_unifrog_d(palette = "likert3") +
#'   theme_bw()
#'
#' ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
#'   geom_point() +
#'   scale_color_unifrog_c(palette = "likert7") +
#'   theme_bw()
#' @export
scale_color_unifrog <-
  function(palette = "unifrog_main",
           discrete = TRUE,
           reverse = FALSE,
           aesthetics = "color",
           ...) {
    if (discrete & palette == "unifrog_main") {
      pal <- usecol(unifrog_main)
      if (reverse) {
        pal <- rev(pal)
      }
      scale_fill_manual(values = pal)
    } else {
      pal <- palette_unifrog(palette = palette, reverse = reverse)

      if (discrete) {
        discrete_scale(
          aesthetics = aesthetics,
          paste0("unifrog_", palette),
          palette = pal,
          ...
        )
      } else {
        scale_color_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
      }
    }
  }




# Aliases -----------------------------------------------------------------


#' @rdname scale_color_unifrog
#' @export
scale_color_unifrog_d <-
  function(palette = "unifrog_main",
           discrete = TRUE,
           reverse = FALSE,
           aesthetics = "color",
           ...) {
    scale_color_unifrog(
      palette = palette,
      discrete = discrete,
      reverse = reverse,
      aesthetics = aesthetics,
      ...
    )
  }

#' @rdname scale_color_unifrog
#' @export
scale_color_unifrog_c <-
  function(palette = "likert3",
           discrete = FALSE,
           reverse = FALSE,
           aesthetics = "color",
           ...) {
    scale_color_unifrog(
      palette = palette,
      discrete = discrete,
      reverse = reverse,
      aesthetics = aesthetics,
      ...
    )
  }

#' @rdname scale_color_unifrog
#' @export
scale_colour_unifrog <- scale_color_unifrog

#' @rdname scale_color_unifrog
#' @export
scale_colour_unifrog_c <- scale_color_unifrog_c

#' @rdname scale_color_unifrog
#' @export
scale_colour_unifrog_d <- scale_color_unifrog_d





# Fill --------------------------------------------------------------------



#' @rdname scale_color_unifrog
#' @inheritParams palette_unifrog
#' @inheritParams unifrog_palettes
#' @export
scale_fill_unifrog <-
  function(palette = "unifrog_main",
           discrete = TRUE,
           reverse = FALSE,
           aesthetics = "fill",
           ...) {
    if (discrete & palette == "unifrog_main") {
      pal <- usecol(unifrog_main)
      if (reverse) {
        pal <- rev(pal)
      }
      scale_fill_manual(values = pal)
    } else {
      pal <- palette_unifrog(palette = palette, reverse = reverse)

      if (discrete) {
        discrete_scale(
          aesthetics = aesthetics,
          paste0("unifrog_", palette),
          palette = pal,
          ...
        )
      } else {
        scale_fill_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
      }
    }
  }

#' @rdname scale_color_unifrog
#' @export
scale_fill_unifrog_d <-
  function(palette = "unifrog_main",
           discrete = TRUE,
           reverse = FALSE,
           aesthetics = "fill",
           ...) {
    scale_fill_unifrog(
      palette = palette,
      discrete = discrete,
      reverse = reverse,
      aesthetics = aesthetics,
      ...
    )
  }

#' @rdname scale_color_unifrog
#' @export
scale_fill_unifrog_c <-
  function(palette = "likert3",
           discrete = FALSE,
           reverse = FALSE,
           aesthetics = "fill",
           ...) {
    scale_fill_unifrog(
      palette = palette,
      discrete = discrete,
      reverse = reverse,
      aesthetics = aesthetics,
      ...
    )
  }





# Palette --------------------------------------------------------------------




# The palette is based on the colours for Unifrog tools as shown in the tool.tool_hex on Azure.
unifrog_colors_list <- c(
  `red` = "#a50036",
  `orange` = "#ff7901",
  `yellow` = "#c89801",
  `grey` = "#cccccc",
  `darkgrey` = "#8c8c8c",
  `lightblue` = "#17a0ff",
  `blue` = "#1561c8",
  `green` = "#33cc99",
  `purple` = "#6815ad",
  `brown` = "#b54f0d",
  `pink` = "#ec44f2",
  `background` = "#FFFFFF",
  `main` = "#000000"
)



#' Extract Unifrog colours as hex codes
#'
#' Can be used to get the hex code of specific colors from the Unifrog color
#' palette. Use `unifrog_colors()` to see all available colors.
#'
#' @param ... Character names of colors.
#'
#' @return A character vector with color-codes.
#'
#' @examples
#' unifrog_colors()
#'
#' unifrog_colors("green", "lightblue", "darkgrey")
#' @export
unifrog_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(unifrog_colors_list)
  }

  unifrog_colors_list[cols]
}
#' Unifrog Main palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(unifrog_main, main = "unifrog_main")
unifrog_main <-
  unikn::newpal(
    col = unifrog_colors("green", "lightblue", "orange", "darkgrey", "red", "purple"),
    names = c("green", "lightblue", "orange", "darkgrey", "red", "purple"),
    as_df = F
  )
#' Unifrog greens palette
#' @import unikn
#' @export
#' @examples
#' library(unikn)
#' seecol(unifrog_greens, main = "unifrog_greens")
unifrog_greens <-
  unikn::shades_of(n = 5, col_1 = "#D4F4E9", col_n = unifrog_colors("green"))
#' Unifrog blues palette
#' @import unikn
#' @export
#' @examples
#' library(unikn)
#' seecol(unifrog_blues, main = "unifrog_blues")
unifrog_blues <-
  unikn::shades_of(n = 5, col_n = unifrog_colors("blue"), col_1 = "#CDDDF3")
#' Unifrog oranges palette
#' @import unikn
#' @export
#' @examples
#' library(unikn)
#' seecol(unifrog_oranges, main = "unifrog_oranges")
unifrog_oranges <-
  unikn::shades_of(n = 5, col_n = unifrog_colors("orange"), col_1 = "#ffe2c9")
#' Unifrog red palette
#' @import unikn
#' @export
#' @examples
#' library(unikn)
#' seecol(unifrog_reds, main = "unifrog_reds")
unifrog_reds <-
  unikn::shades_of(n = 5, col_n = unifrog_colors("red"), col_1 = "#ecc9d4")
#' Unifrog likert3 palette
#' @import unikn
#' @export
#' @examples
#' library(unikn)
#' seecol(likert3, main = "likert3")
likert3 <-
  unikn::newpal(
    col = unifrog_colors("red", "grey", "blue"),
    names = c("red", "grey", "blue"),
    as_df = F
  )
#' Unifrog likert5 palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(likert5, main = "likert5")
likert5 <-
  unikn::newpal(
    col = unifrog_colors("red", "orange", "grey", "yellow", "green"),
    names = c("red", "orange", "grey", "yellow", "green"),
    as_df = F
  )
#' Unifrog likert7 palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(likert7, main = "likert7")
likert7 <-
  unikn::newpal(
    col = unifrog_colors("red", "orange", "yellow", "grey", "green", "blue", "purple"),
    names = c("red", "orange", "yellow", "grey", "green", "blue", "purple"),
    as_df = F
  )
#' List of all Unifrog palettes
#' @import unikn
#' @export
#' @examples
#' library(unikn)
#' unikn::seecol(unifrog_palettes, main = "All Unifrog Palettes", pal_names = names(unifrog_palettes))
unifrog_palettes <- list(
  "unifrog_main" = unifrog_main,
  "unifrog_blues" = unifrog_blues,
  "unifrog_greens" = unifrog_greens,
  "unifrog_oranges" = unifrog_oranges,
  "unifrog_reds" = unifrog_reds,
  "likert3" = likert3,
  "likert5" = likert5,
  "likert7" = likert7
)





#' Unifrog colour palette
#'
#' The palette is based on the colours for Unifrog tools as shown in the tool.tool_hex on Azure.
#'
#' @param palette Character name of palette. Depending on the color scale, can
#'   be `"main"` (default, discrete), or `"likert3"`, `"likert5"`, `"likert7"` for diverging scales.
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param ... Additional arguments to pass to [`colorRampPalette()`][colorRampPalette].
#'
#' @details This function is usually not called directly, but from within
#'   [`scale_color_unifrog()`][scale_color_unifrog].
#' @import unikn
#' @export
palette_unifrog <-
  function(palette = "unifrog_main",
           reverse = FALSE,
           ...) {
    .retrieve_palette(palette, unifrog_palettes, reverse = reverse, ...)
  }


# helper -----------------------

.retrieve_palette <-
  function(palette, palette_list, reverse = FALSE, ...) {
    if (!palette %in% names(palette_list)) {
      msg <- c(
        paste0(
          "Palette name not available. `palette` must be one of ",
          datawizard::text_concatenate(names(palette_list), last = " or ", enclose = "`"),
          "."
        ),
        "Using default palette now."
      )
      warning(insight::format_message(msg), call. = FALSE)
      palette <- 1
    }
    pal <- palette_list[[palette]]

    if (reverse) {
      pal <- rev(pal)
    }

    grDevices::colorRampPalette(pal, ...)
  }
