.onAttach <- function(libname, pkgname) {
  # theme_set(unifrog_theme())
  # packageStartupMessage("Default ggplot theme set to unifrog_theme(). Unifrog colour palettes applied.")
  options(ggplot2.continuous.colour = scale_color_unifrog_c)
  options(ggplot2.continuous.fill = scale_fill_unifrog_c)
  options(ggplot2.discrete.colour = scale_color_unifrog_d)
  options(ggplot2.discrete.fill = scale_fill_unifrog_d)
}
