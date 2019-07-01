#' Customize ggplot
#'
#' @description These functions provide customization for ggplot2.
#' @import ggplot2
#' @import hrbrthemes
#' @name my_ggplot

#### load packages ---------------------------------------------------
library(ggplot2)

#' @rdname my_ggplot
#' @export my_scale_color
#' @param type One of "div", "seq", or "qual": the type of palette desired.
#' @param data One of "cont" or "discrete: the type of data being plotted.
#' @param other Logical. Use the "other" version of the palette if true.
#' @param bw Logical. Print a black and white version of the figure.
my_scale_color <- function(type = "div",
                           data = "cont",
                           other = FALSE,
                           bw = FALSE,
                           direction = 1,
                           ...) {
  if (bw) return(scale_color_grey(...))

  scale <- function(..., data, palette, direction) {
    switch(data,
           cont = scale_color_distiller(..., palette = palette, direction = direction),
           discrete = scale_color_brewer(..., palette = palette, direction = direction)
    )
  }

  if (!other) {
    palette <- switch(type,
                      div = "RdYlBu",
                      seq = "YlGnBu",
                      qual = "Dark2"
    )
  } else {
    palette <- switch(type,
                      div = "PRGn",
                      seq = "Blues",
                      qual = "Set2"
    )
  }

  scale(..., data = data, palette = palette, direction = direction)
}

#' @rdname my_ggplot
#' @export my_scale_fill
my_scale_fill <- function(type = "div",
                          data = "cont",
                          other = FALSE,
                          bw = FALSE,
                          direction = 1,
                          ...) {
  if (bw) return(scale_fill_grey(...))

  scale <- function(..., data, palette, direction) {
    switch(data,
           cont = scale_fill_distiller(..., palette = palette, direction = direction),
           discrete = scale_fill_brewer(..., palette = palette, direction = direction)
    )
  }

  if (!other) {
    palette <- switch(type,
                      div = "RdYlBu",
                      seq = "YlGnBu",
                      qual = "Dark2"
    )
  } else {
    palette <- switch(type,
                      div = "PRGn",
                      seq = "Blues",
                      qual = "Set2"
    )
  }

  scale(..., data = data, palette = palette, direction = direction)
}

#' @export my_theme
#' @rdname my_ggplot
#' @param font Logical. Whether the figure should be plotted with the Roboto Condensed font (requires installation).
my_theme <- function(..., font = TRUE, axis = "c") {
  if (!font) {
    hrbrthemes::theme_ipsum(
      axis_title_just = axis,
      base_size = 12,
      axis_title_size = 12,
      axis_text_size = 10,
      ...
    )
  } else {
    hrbrthemes::theme_ipsum_rc(
      axis_title_just = axis,
      base_size = 12,
      axis_title_size = 12,
      axis_text_size = 10,
      ...
    )
  }
}
