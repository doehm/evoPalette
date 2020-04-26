

#' evoPalette palette function
#'
#' Generates a colour palette function for a discrete or continuous scale
#'
#' @inheritParams scale_fill_evo
#'
#' @rdname scales_evo
#'
#' @return
#' @export
#'
#' @importFrom grDevices colorRampPalette
#'
#' @examples
evo_pal <- function(name, scale_type = "d", reverse = FALSE, ...) {
  if(length(palette_box()) == 0) stop("palette box is empty. Run 'launch_evo_palette() to generate a palette")
  cols <- palette_box()[[name]]
  if(reverse) cols <- rev(cols)
  switch(
    str_sub(scale_type, 1, 1),
    d = function(n) {
      if(n > length(cols)){
        colorRampPalette(cols)(n)
      }else{
        cols[1:n]
      }
    },
    c = function(n) {
      colorRampPalette(cols[seq(1, length(cols), 3)])(200)[floor(n*199)+1]
    }
  )
}




#' Scale fill aesthetic
#'
#' Generalises the scale aesthetics for evoPalettes
#'
#' @param name Name of palette in the palette box
#' @param scale_type Discrete or continuous. Input \code{c} / \code{d}.
#' @param reverse Logical. Reverse the palette?
#' @param ... Dots
#'
#' @rdname scales_evo
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @importFrom stringr str_sub
#'
#' @examples
scale_fill_evo <- function(name, scale_type = "d", reverse = FALSE, ...) {
  switch(
    str_sub(scale_type, 1, 1),
    d = ggplot2::discrete_scale("fill", "evo", evo_pal(name, scale_type, reverse = reverse, ...)),
    c = ggplot2::continuous_scale("fill", "evo", evo_pal(name, scale_type, reverse = reverse, ...), guide = "colorbar", ...)
  )
}



#' Scale colour aesthetic
#'
#' Generalises the scale aesthetics
#'
#' @inheritParams scale_fill_evo
#'
#' @rdname scales_evo
#'
#' @return
#' @export
#'
#' @import ggplot2
#'
#' @examples
scale_colour_evo <- function(name, scale_type = "d", reverse = FALSE, ...) {
  switch(
    str_sub(scale_type, 1, 1),
    d = ggplot2::discrete_scale("colour", "evo", evo_pal(name, scale_type, reverse = reverse, ...)),
    c = ggplot2::continuous_scale("colour", "evo", evo_pal(name, scale_type, reverse = reverse, ...), guide = "colorbar", ...)
  )
}
