


#' Get hex code
#'
#' @param id
#'
#' @return
#' @export
#'
#' @import purrr
#'
#' @examples
get_hex <- function(id){
  map_chr(id, ~{
    rgb <- rgb_grid[.x,]*10/255
    rgb(rgb[1], rgb[2], rgb[3])
  })
}





#' Title
#'
#' @param x
#' @param n
#'
#' @return
#' @export
#'
#' @import binaryLogic
#'
#' @examples
chromosome <- function(x, n = 15){
  map(x, ~{
    x_b <- as.binary(.x)
    c(rep(0, 15-length(x_b)), x_b)
  }) %>%
    do.call(c, .)
}



#' Title
#'
#' @param parents
#'
#' @return
#' @export
#'
#' @examples
crossover <- function(parents) {
  n <- length(parents[[1]])
  n_crossover <- sample(1:n, 1)
  id <- sample(1:n, n_crossover)
  child <- parents[[1]]
  child[id] <- parents[[2]][id]
  child
}



#' Title
#'
#' @param chromosome
#' @param n
#'
#' @return
#' @export
#'
#' @import dplyr
#'
#' @examples
make_palette <- function(chromosome, n = 15) {
  pal <- tibble(
    id = floor((1:length(chromosome)-1)/n),
    chromosome = chromosome
  ) %>%
    split(.$id) %>%
    map_dbl(~{
      y <- structure(.x$chromosome, class = c("binary", "logical"), signed = FALSE, littleEndian = FALSE)
      as.integer(as.binary(y))
    }) %>%
    unname() %>%
    get_hex

  pal[get_pal_order(pal)]
}







#' Show palette
#'
#' Shows the palette
#'
#' @param p Palette object
#' @param n Number of colours to show. Defaults to c(length(pal), 200)
#'
#' @return
#' @export
#'
#' @import ggplot2
#'
#' @examples
show_palette <- function(pal, n = NULL, labels = FALSE){

  if(is.null(n)) n <- c(length(pal), 200)

  x <- seq(0, 1, length = n[1]+1)
  y <- c(0.05, 1)
  type <- paste0("x", 1:(n[1]+1))
  df1 <- data.frame(xmin = x[-(n[1]+1)], xmax = x[-1]+0.001, ymin = y[1], ymax = y[2], type = type[-1])

  x <- seq(0, 1, length = n[2]+1)
  y <- c(-1, -0.05)
  type <- paste0("x", 1:(n[2]+1))
  df2 <- data.frame(xmin = x[-(n[2]+1)], xmax = x[-1]+0.001, ymin = y[1], ymax = y[2], type = type[-1])

  x_text <- seq(0, 1, length = n[1]+1)
  d <- mean(df1$xmin[1:2])
  g <- ggplot() +
    theme_void() +
    annotate("rect", xmin = df1$xmin, xmax = df1$xmax, ymin = df1$ymin, ymax = df1$ymax, fill = colorRampPalette(pal)(n[1])) +
    annotate("rect", xmin = df2$xmin, xmax = df2$xmax, ymin = df2$ymin, ymax = df2$ymax, fill = colorRampPalette(pal[c(1, 3, 5)])(n[2]))

  if(labels) g <- g + annotate("text", x = (df1$xmin + df1$xmax)/2, y = (df1$ymin + df1$ymax)/2, label = pal)

  return(g)
}





#' Hue
#'
#' Gets hue from a colour palette
#'
#' @param pal The hex code for a colour
#'
#' @return
#' @export
#'
#' @importFrom grDevices col2rgb rgb2hsv
#'
#' @examples
get_pal_order <- function(pal){
  .rgb <- t(col2rgb(pal))
  .rgb <- cbind(.rgb, t(rgb2hsv(.rgb[,1], .rgb[,2], .rgb[,3])))
  d <- dist(.rgb)
  begin <- which(pal == sort(pal)[1])
  order <- as.numeric(names(sort(as.matrix(d)[begin,])))
  pal[order]
}




#' Test plot
#'
#' Plots a classic histogram using MPG data to test the colour palette
#'
#' @param pal
#'
#' @return
#' @export
#'
#' @examples
plot_palette <- function(pal) {
  mpg %>%
    ggplot(aes(x = displ, fill = class)) +
    geom_histogram() +
    scale_fill_manual(values = colorRampPalette(pal)(7)) +
    theme_minimal()
}

