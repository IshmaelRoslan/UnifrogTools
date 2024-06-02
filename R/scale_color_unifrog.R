#' Unifrog colour palette
#'
#' The palette is based on the colours for Unifrog tools as shown in the tool.tool_hex on Azure.
#' Use `scale_color_unifrog_d` for *discrete* categories and
#' `scale_color_unifrog_c` for a *continuous* scale.
#'
#' @inheritParams palette_unifrog
#' @inheritParams cb_palettes
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
          #paste0("unifrog_", palette),
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
          #paste0("unifrog_", palette),
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

#' Colour-Blind Friendly Qualitative Bright Palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(qual_bright, main = "qual_bright")
qual_bright <-
  unikn::newpal(
    col = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB'),
    names = c("blue", "cyan", "green", "yellow", "red", "purple", "grey"),
    as_df = F
  )

#' Colour-Blind Friendly Qualitative High-Contrast Palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(qual_hc, main = "qual_hc")
qual_hc <-
  unikn::newpal(
    col = c('#004488', '#DDAA33', '#BB5566'),
    names = c("blue","yellow","red"),
    as_df = F
  )

#' Colour-Blind Friendly Qualitative Vibrant Palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(qual_vibrant, main = "qual_vibrant")
qual_vibrant <-
  unikn::newpal(
    col = c('#EE7733', '#0077BB', '#33BBEE', '#EE3377', '#CC3311', '#009988', '#BBBBBB'),
    names = c("orange", "blue", "cyan", "magenta", "red", "teal", "grey"),
    as_df = F
  )

#' Colour-Blind Friendly Qualitative Muted Palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(qual_muted, main = "qual_muted")
qual_muted <-
  unikn::newpal(
    col = c( '#CC6677', '#332288', '#DDCC77', '#117733', '#88CCEE', '#882255', '#44AA99', '#999933', '#AA4499'),
    names = c("rose", "indigo", "sand", "green","cyan","wine",
              "teal", "olive", "purple"),
    as_df = F
  )

#' Colour-Blind Friendly Qualitative Medium Contrast Palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(qual_mc, main = "qual_mc")
qual_mc <-
  unikn::newpal(
    col = c(  '#6699CC', '#004488', '#EECC66', '#994455', '#997700', '#EE99AA'),
    names = c("light blue", "dark blue", "light yellow", "dark red", "dark yellow", "dark yellow"),
    as_df = F
  )

#' Colour-Blind Friendly Qualitative Pale Palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(qual_pale, main = "qual_pale")
qual_pale <-
  unikn::newpal(
    col = c('#BBCCEE', '#CCEEFF', '#CCDDAA', '#EEEEBB', '#FFCCCC', '#DDDDDD'),
    names = c("pale blue", "pale cyan","pale green","pale yellow",
              "pale red", "pale grey"),
    as_df = F
  )

#' Colour-Blind Friendly Qualitative Light Palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(qual_light, main = "qual_light")
qual_light <-
  unikn::newpal(
    col = c( '#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF', '#44BB99', '#BBCC33', '#AAAA00', '#DDDDDD'),
    names = c("light blue", "orange","light yellow", "pink", "light cyan",
              "mint","pear","olive", "pale grey"),
    as_df = F
  )

#' Colour-Blind Friendly Diverging Sunset Palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(div_sunset, main = "div_sunset")
div_sunset <-
  unikn::newpal(
    col = c( '#364B9A', '#4A7BB7', '#6EA6CD', '#98CAE1', '#C2E4EF', '#EAECCC', '#FEDA8B', '#FDB366', '#F67E4B', '#DD3D2D', '#A50026'),
    as_df = F
  )

#' Colour-Blind Friendly Diverging Purple-Green Palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(div_PrGn, main = "div_PrGn")
div_PrGn <-
  unikn::newpal(
    col = c( '#762A83', '#9970AB', '#C2A5CF', '#E7D4E8', '#F7F7F7', '#D9F0D3', '#ACD39E', '#5AAE61', '#1B7837'),
    as_df = F
  )

#' Colour-Blind Friendly Diverging Nightfall Palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(div_nightfall, main = "div_nightfall")
div_nightfall <-
  unikn::newpal(
    col = c('#125A56', '#00767B', '#238F9D', '#42A7C6', '#60BCE9', '#9DCCEF', '#C6DBED', '#DEE6E7', '#ECEADA', '#F0E6B2', '#F9D576', '#FFB954', '#FD9A44', '#F57634', '#E94C1F', '#D11807', '#A01813'),
    as_df = F
  )

#' Colour-Blind Friendly Diverging Blue-Red Palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(div_BuRd, main = "div_BuRd")
div_BuRd <-
  unikn::newpal(
    col = c(  '#2166AC', '#4393C3', '#92C5DE', '#D1E5F0', '#F7F7F7', '#FDDBC7', '#F4A582', '#D6604D', '#B2182B'),
    as_df = F
  )


#' Colour-Blind Friendly Sequential Yellow-Brown Palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(seq_YlBr, main = "seq_YlBr")
seq_YlBr <-
  unikn::newpal(
    col = c( '#FFFFE5', '#FFF7BC', '#FEE391', '#FEC44F', '#FB9A29', '#EC7014', '#CC4C02', '#993404', '#662506'),
    as_df = F
  )


#' Colour-Blind Friendly Sequential Iridescent Palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(seq_iridescent, main = "seq_iridescent")
seq_iridescent <-
  unikn::newpal(
    col = c('#FEFBE9', '#FCF7D5', '#F5F3C1', '#EAF0B5', '#DDECBF', '#D0E7CA', '#C2E3D2', '#B5DDD8', '#A8D8DC', '#9BD2E1', '#8DCBE4', '#81C4E7', '#7BBCE7', '#7EB2E4', '#88A5DD', '#9398D2', '#9B8AC4', '#9D7DB2', '#9A709E', '#906388', '#805770', '#684957', '#46353A'),
    as_df = F
  )


#' Colour-Blind Friendly Sequential Incandescent Palette
#' @import unikn
#' @export
#' @examples
#' unikn::seecol(seq_incandescent, main = "seq_incandescent")
seq_incandescent <-
  unikn::newpal(
    col = c('#CEFFFF', '#C6F7D6', '#A2F49B', '#BBE453', '#D5CE04', '#E7B503', '#F19903', '#F6790B', '#F94902', '#E40515', '#A80003'),
    as_df = F
  )

#' List of all Colour-Blind Friendly palettes
#' @import unikn
#' @export
#' @examples
#' library(unikn)
#' unikn::seecol(cb_palettes, main = "All CB-friendly Palettes", pal_names = names(cb_palettes))
cb_palettes <- list(
  "qual_bright" = qual_bright,
  "qual_vibrant\nbad data DDDDDD" = qual_vibrant,
  "qual_hc" = qual_hc,
  "qual_mc\nuse for pairs" = qual_mc,
  "qual_pale\n cell bg" = qual_pale,
  "qual_light\n cell bg" = qual_light,
  "div_sunset\n bad data white" = div_sunset,
  "div_nightfall\n bad data white" = div_nightfall,
  "div_PrGn\n bad data FFEE99" = div_PrGn,
  "div_BuRd\n bad data FFEE99" = div_BuRd,
  "seq_YlBr\n bad data 888888" = seq_YlBr,
  "seq_iridescent\n bad data 999999" = seq_iridescent,
  "seq_incandescent\n bad data 888888" = seq_incandescent
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
    palette_list <- c(unifrog_palettes,cb_palettes)
    names(palette_list) <-
      names(palette_list) |> stringr::str_replace("(.*)\\n(.*)", "\\1")
    .retrieve_palette(palette, palette_list, reverse = reverse, ...)
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
