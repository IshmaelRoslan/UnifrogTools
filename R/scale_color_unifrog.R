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
      scale_color_manual(values = pal)
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
  `pink0` = "#feddef", `pink1` = "#fcabd8",`pink2` = "#fb85c6",`pink` = "#f95cb3",`darkpink` = "#be5787",
  `orange0` = "#ffe4cc",`orange1` = "#ffc999",`orange2` = "#ffaf67",`orange` = "#ff7901",`darkorange` = "#af5d13",
  `yellow0` = "#fff2cc",`yellow1` = "#ffe699",`yellow2` = "#ffd966", `yellow` = "#ffc000",`darkyellow` = "#d88300",
  `green0` = "#cdf3e6", `green1` = "#adebd6",`green2` = "#85e0c2",`green` = "#33cc99",`darkgreen` = "#188f67",
  `teal0` = "#c9f0ef",`teal1` = "#b7e9e9",`teal2` = "#93ddde",`teal` = "#4bc7c8",`darkteal` = "#348b8b",
  `blue0` = "#dae9f6",`blue1` = "#bdd7ee",`blue2` = "#9dc3e6",`blue` = "#5b9bd5",`darkblue` = "#167ad5",
  `indigo0` = "#e8e9fe",`indigo1` = "#dbddfd",`indigo2` = "#bcbffc",`indigo` = "#9ba0fb",`darkindigo` = "#4f58fc",
  `purple0` = "#ecdff5",`purple1` = "#ddc5ed",`purple2` = "#cba7e3",`purple` = "#bd90dc",`darkpurple` = "#9036d6",
  `background` = "#Ffffff",
  `neutralgrey` = "#F9f9f9",
  'lightgrey' = "#CCCCCC",
  `darkgrey` = "#999999",
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

  unifrog_colors_list[cols] |> unname()
}
#' Unifrog Main palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(unifrog_main, main = "unifrog_main")
unifrog_main <-
  unikn::newpal(
    col = unifrog_colors("blue", "orange", "lightgrey","teal", "purple","yellow","pink","darkgrey", "indigo"),
    names = c("blue", "orange","lightgrey", "teal", "purple","yellow","pink","darkgrey", "indigo"),
    as_df = F
  )
#' Unifrog purples palette
#' @import unikn
#' @export
#' @examples
#' library(unikn)
#' seecol(unifrog_purples, main = "unifrog_purples")
unifrog_purples <-
  unikn::newpal(
    col = unifrog_colors("background","purple0", "purple1", "purple2" , "purple" , "darkpurple"),
    names = c("background","purple0", "purple1", "purple2" , "purple" , "darkpurple" ),
    as_df = F
  )
#' Unifrog blues palette
#' @import unikn
#' @export
#' @examples
#' library(unikn)
#' seecol(unifrog_blues, main = "unifrog_blues")
unifrog_blues <-
  unikn::newpal(
    col = unifrog_colors("background","blue0", "blue1", "blue2" , "blue" , "darkblue" ),
    names = c("background","blue0", "blue1", "blue2" , "blue" , "darkblue" ),
    as_df = F
  )
#' Unifrog oranges palette
#' @import unikn
#' @export
#' @examples
#' library(unikn)
#' seecol(unifrog_oranges, main = "unifrog_oranges")
unifrog_oranges <-
  unikn::newpal(
    col = unifrog_colors("background","orange0", "orange1", "orange2" , "orange" , "darkorange" ),
    names = c("background","orange0", "orange1", "orange2" , "orange" , "darkorange" ),
    as_df = F
  )
#' Unifrog teals palette
#' @import unikn
#' @export
#' @examples
#' library(unikn)
#' seecol(unifrog_teals, main = "unifrog_teals")
unifrog_teals <-
  unikn::newpal(
    col = unifrog_colors("background","teal0", "teal1", "teal2" , "teal" , "darkteal" ),
    names = c("background","teal0", "teal1", "teal2" , "teal" , "darkteal" ),
    as_df = F
  )
#' Unifrog likert3 palette
#' @import unikn
#' @export
#' @examples
#' library(unikn)
#' seecol(likert3, main = "likert3")
likert3 <-
  unikn::newpal(
    col = unifrog_colors("orange", "lightgrey", "blue"),
    names = c("orange", "lightgrey", "blue"),
    as_df = F
  )
#' Unifrog likert5 palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(likert5, main = "likert5")
likert5 <-
  unikn::newpal(
    col = unifrog_colors("darkorange","orange", "lightgrey", "blue", "darkblue"),
    names = c("darkorange","orange", "lightgrey", "blue", "darkblue"),
    as_df = F
  )
#' Unifrog likert7 palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(likert7, main = "likert7")
likert7 <-
  unikn::newpal(
    col = unifrog_colors("darkorange", "orange", "yellow", "lightgrey", "blue", "darkblue", "darkpurple"),
    names = c("darkorange", "orange", "yellow", "lightgrey", "blue", "darkblue", "darkpurple"),
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
  "unifrog_purples" = unifrog_purples,
  "unifrog_oranges" = unifrog_oranges,
  "unifrog_teals" = unifrog_teals,
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
