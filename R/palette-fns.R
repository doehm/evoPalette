

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
  child <- get_pal_order(child)
  child
}




#' Title
#'
#' @param n_cols
#' @param n_parents
#'
#' @return
#' @export
#'
#' @importFrom glue glue
#' @import paletteer
#'
#' @examples
random_palette <- function(n_cols, n_parents, from = "palettes") {

  if(from == "random"){
    return(
      map(1:n_parents, ~{
        rgb(r = runif(n_cols), g = runif(n_cols), b = runif(n_cols), runif(n_cols))
        }) %>%
        map(~get_pal_order(.x))
    )
  }

  if(from == "palettes") {
    return(
      map(1:n_parents, ~{
        pal <- palettes_d_names[sample(1:nrow(palettes_d_names), 1),]
        pal <- paletteer_d(glue("{pal$package}::{pal$palette}"))
        colorRampPalette(pal)(n_cols)
      }) %>%
        map(~get_pal_order(.x))
    )
  }

}




#' Title
#'
#' @param parents
#'
#' @return
#' @export
#'
#' @examples
mutation <- function(parents, mutation_rate = 0.05) {
  map(parents, ~col2rgb(.x)/255) %>%
    map(~matrix(rbeta(length(.x), .x*255/5 + 1, 255/5*(1 - .x) + 1), nrow = 3)) %>%
    map(~rgb(r = .x[1,], g = .x[2,], b = .x[3,])) %>%
    map(~{
      df <- .x
      mutation_id <- runif(length(.x)) < mutation_rate
      random_rgb <- random_palette(length(.x), 1, from = "random")[[1]]
      df[mutation_id] <- random_rgb[mutation_id]
      df
    }) %>%
    map(~get_pal_order(.x))
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
show_palette <- function(pal, n = NULL, labels = FALSE, n_continuous = 3){

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
    annotate("rect", xmin = df2$xmin, xmax = df2$xmax, ymin = df2$ymin, ymax = df2$ymax, fill = colorRampPalette(pal[seq(1, n[1], length = n_continuous)])(n[2]))

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
#' @import ggplot2
#'
#' @examples
plot_palette <- function(pal) {
  mpg %>%
    ggplot(aes(x = displ, fill = class)) +
    geom_histogram() +
    scale_fill_manual(values = colorRampPalette(pal)(7)) +
    theme_minimal()
}

