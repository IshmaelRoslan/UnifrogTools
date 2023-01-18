
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Unifrog Tools

<!-- badges: start -->
<!-- badges: end -->

The goal is to provide a set of function to help the production of
documents for reproducible research in the Unifrog House Style. It
contains three main parts:

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

### Package compiler

Install
[Rtools](https://cran.r-project.org/bin/windows/Rtools/rtools42/files/rtools42-5355-5357.exe)
if you are running windows or
[Xcode](https://developer.apple.com/xcode/) if you have a Mac

### Installing Required Packages

You can run the code this code to install most of the packages.

``` r
install.packages(c("devtools", "datawizard", "keyring", "DBI", "odbc", "ggtext", "glue", "ragg", "tidyverse", "insight", "unikn"))
```

## Installation

You can install the development version of unifrog from
[GitHub](https://github.com/IshmaelRoslan/UnifrogTools) with:

``` r
# install.packages("devtools")
devtools::install_github("IshmaelRoslan/UnifrogTools")
```

## In RStudio

Tools \> Global Options \> General \> Graphics

Set backend to AGG. (Better rendering.)

Tools \> Global Options Set your default working directory to the
projects folder on your hard drive.

## Vignettes

See `vignette("Azure")` to get setup your database connections,
`vignette("Palettes")` for examples of ggplot themes and palettes or
view the online documentation
[here](https://ishmaelroslan.github.io/UnifrogTools/).
