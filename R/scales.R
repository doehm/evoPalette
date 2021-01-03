

#' evoPalette palette function
#'
#' Generates a colour palette function for a discrete or continuous scale
#'
#' @param name Name of palette in the palette box
#' @param scale_type Discrete or continuous. Input \code{d} / \code{c}.
#' @param lum lum level 0-1.
#' @param reverse Logical. Reverse the palette?
#' @param divergent Use divergent scale? Use a two element vector to customise the scale e.g. \code{c(2, 5)}
#' @param sequential Use divergent scale? Use an integer to customise the scale
#' @param ... Dots
#'
#' @rdname scales_evo
#'
#' @details The evoPalette app needs to be run and at least one palette saved to use the scale functions. The scales
#' refer to the palette by name.
#'
#' @return
#' @export
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom glue glue
#' @importFrom crayon green
#'
#' @examples \dontrun{}
evo_pal <- function(name, scale_type = "d", divergent = FALSE, sequential = FALSE, lum = 1, reverse = FALSE, ...) {
  if(length(palette_box()) == 0) stop("palette box is empty. Run 'launch_evo_palette()' to generate a palette")
  if(is.null(name)) {
    name <- names(palette_box())[1]
    message(green(glue("Note: No name provided. Selecting '{to_title_case(name)}' from palette_box")))
  }
  cols <- palette_box()[[name]]

  a <- max(100*round(lum, 2), 1)
  for(k in 1:length(cols)) {
    cols[k] <- colorRampPalette(c("white", cols[k]))(100)[a]
  }

  if(reverse) cols <- rev(cols)
  switch(
    str_sub(scale_type, 1, 1),
    d = function(n) {
      if(n > length(cols)){
        colorRampPalette(cols)(n)
      }else{
        cols[seq(1, length(cols), length = n)]
      }
    },
    c = function(n) {
      if(!isFALSE(divergent)) {
        p <- c(divergent[1], "white", cols[divergent[2]])
        colorRampPalette(p)(200)[floor(n*199)+1]
      }else if(!isFALSE(sequential)) {
        colorRampPalette(c("white", cols[sequential]))(200)[floor(n*199)+1]
      }
    }
  )
}




#' Scale fill aesthetic
#'
#' @param name Name of palette in the palette box
#' @param scale_type Discrete or continuous. Input \code{d} / \code{c}.
#' @param reverse Logical. Reverse the palette?
#' @param lum lum level 0-1.
#' @param divergent Use divergent scale? Use a two element vector to customise the scale e.g. \code{c(2, 5)}
#' @param sequential Use divergent scale? Use an integer to customise the scale
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
#' @examples \dontrun{}
scale_fill_evo <- function(
  name = NULL,
  scale_type = "d",
  lum = 1,
  reverse = FALSE,
  divergent = FALSE,
  sequential = FALSE,
  ...) {
  switch(
    str_sub(scale_type, 1, 1),
    d = ggplot2::discrete_scale("fill", "evo", evo_pal(name, scale_type, reverse = reverse, lum = lum, ...)),
    c = ggplot2::continuous_scale("fill", "evo", evo_pal(name, scale_type, reverse = reverse, lum = lum, sequential = sequential, divergent = divergent, ...), guide = "colorbar", ...)
  )
}



#' Scale colour aesthetic
#'
#' @param name Name of palette in the palette box
#' @param scale_type Discrete or continuous. Input \code{d} / \code{c}.
#' @param reverse Logical. Reverse the palette?
#' @param lum lum level 0-1.
#' @param divergent Use divergent scale? Use a two element vector to customise the scale e.g. \code{c(2, 5)}
#' @param sequential Use divergent scale? Use an integer to customise the scale
#' @param ... Dots
#'
#' @rdname scales_evo
#'
#' @return
#' @export
#'
#' @import ggplot2
#'
#' @examples \dontrun{}
scale_colour_evo <- function(
  name,
  scale_type = "d",
  lum = 1,
  reverse = FALSE,
  divergent = FALSE,
  sequential = FALSE,
  ...) {
  switch(
    str_sub(scale_type, 1, 1),
    d = ggplot2::discrete_scale("colour", "evo", evo_pal(name, scale_type, reverse = reverse, lum = lum, ...)),
    c = ggplot2::continuous_scale("colour", "evo", evo_pal(name, scale_type, reverse = reverse, lum = lum, sequential = sequential, divergent = divergent, ...), guide = "colorbar", ...)
  )
}
