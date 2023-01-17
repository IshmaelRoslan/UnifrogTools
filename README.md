
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Unifrog Tools

<!-- badges: start -->
<!-- badges: end -->

The goal is to provide a set of function to help the production of
documents for reproducible research in the Unifrog House Style. It
contains to main part:

1.  Functions for interaction with the Azure database, including a
    secure way to store credentials using the `keyring` package.

2.  A corporate `ggplot2` theme for Unifrog - `unifrog_theme()`

3.  Corporate colour palettes including discrete, sequential and
    diverging versions.

## Before Installation

### Quarto

Install the latest version of
[Quarto](https://quarto.org/docs/get-started/)

### Fonts

- Download Open Sans and Fira Code fonts on your system using [this
  link](https://fonts.google.com/share?selection.family=Fira%20Code:wght@300%7COpen%20Sans:ital,wght@0,300;0,400;0,500;0,600;0,700;0,800;1,300;1,400;1,500;1,600;1,700;1,800).

- Select All Styles

- Download All

- Open the zip file and in each font folder double-click any files
  ending in `.ttf` . There should be 2 for Open Sans and 1 for Fira Code

- Click Install

### Installing Required Packages

## Installation

You can install the development version of unifrog from
[GitHub](https://github.com/IshmaelRoslan/UnifrogTools) with:

``` r
# install.packages("devtools")
devtools::install_github("IshmaelRoslan/UnifrogTools")
```

## Vignettes

See `vignette("Azure")` and `vignette("Palettes")` for examples or view
the online documentation
[here](https://ishmaelroslan.github.io/UnifrogTools/).
