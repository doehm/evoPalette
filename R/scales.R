

#' evoPalette palette function
#'
#' Generates a colour palette function for a discrete or continuous scale
#'
#' @inheritParams scale_fill_evo
#'
#' @rdname scales
#'
#' @return
#' @export
#'
#' @importFrom grDevices colorRampPalette
#'
#' @examples
evo_pal <- function(name, discrete = TRUE, reverse = FALSE, ...) {
  if(length(palette_box()) == 0) stop("palette box is empty. Run 'launch_evo_palette() to generate a palette")
  cols <- palette_box()[[name]]
  if(reverse) cols <- rev(cols)
  if(discrete){
    function(n) {
      if(n > length(cols)){
        colorRampPalette(cols)(n)
      }else{
        cols[1:n]
      }
    }
  }else{
    function(n) {
      colorRampPalette(cols[seq(1, length(cols), 3)])(200)[floor(n*199)+1]
    }
  }
}




#' Scale fill aesthetic
#'
#' Generalises the scale aesthetics for evoPalettes
#'
#' @param name Name of palette in the palette box
#' @param discrete Logical. Is the aesthetic discrete?
#' @param reverse Logical. Reverse the palette?
#' @param ... Dots
#'
#' @rdname scales
#'
#' @return
#' @export
#'
#' @import ggplot2
#'
#' @examples
scale_fill_evo <- function(name, discrete = TRUE, reverse = FALSE, ...) {
  if(discrete){
    ggplot2::discrete_scale("fill", "evo", evo_pal(name, reverse = reverse, ...))
  }else{
    ggplot2::continuous_scale("fill", "evo", evo_pal(name, FALSE, reverse = reverse), guide = "colorbar", ...)
  }
}



#' Scale colour aesthetic
#'
#' Generalises the scale aesthetics
#'
#' @inheritParams scale_fill_evo
#'
#' @rdname scales
#'
#' @return
#' @export
#'
#' @import ggplot2
#'
#' @examples
scale_colour_evo <- function(name, discrete = TRUE, reverse = FALSE, ...) {
  if(discrete){
    ggplot2::discrete_scale("colour", "evo", evo_pal(name, reverse = reverse), ...)
  }else{
    ggplot2::continuous_scale("colour", "evo", evo_pal(name, FALSE, reverse = reverse), guide = "colorbar", ...)
  }
}
