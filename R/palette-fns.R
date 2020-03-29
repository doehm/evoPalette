


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
random_palette <- function(n_cols, n_palettes, from = "palettes") {

  if(from == "random"){
    return(
      map(1:n_palettes, ~{
        rgb(r = runif(n_cols), g = runif(n_cols), b = runif(n_cols), runif(n_cols))
        }) %>%
        map(~get_pal_order(.x))
    )
  }

  if(from == "palettes") {
      palette_draws <- map(1:n_palettes, ~{
        pal_names <- paletteer::palettes_d_names[sample(1:nrow(palettes_d_names), 1),]
      })
      names(palette_draws) <- map_chr(palette_draws, ~{.x$palette}) %>% to_title_case()
      palette_draws <- map(palette_draws, ~{
        pal <- paletteer::paletteer_d(glue("{.x$package}::{.x$palette}"))
        colorRampPalette(pal)(n_cols)
      }) %>%
      map(~get_pal_order(.x))
      return(palette_draws)
  }
}





#' Title
#'
#' @param parents
#' @param mutation_rate
#' @param variation_parameter
#'
#' @return
#' @export
#'
#' @examples
mutation <- function(parents, mutation_rate = 0.05, variation_parameter = 0.01) {
  N <- 1
  map(parents, ~col2rgb(.x)/255) %>%
    map(~matrix(rbeta(length(.x), .x*N/variation_parameter + 1, N/variation_parameter*(1 - .x) + 1), nrow = 3)) %>%
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
#' @param n Number of colours to show. Defaults to c(length(pal), 200)
#' @param pal
#' @param title
#' @param labels
#' @param n_continuous
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @import snakecase
#'
#' @examples
show_palette <- function(pal, title = NULL, n = NULL, labels = FALSE, n_continuous = 3){

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
  if(!is.null(title)) {
    g <- g +
      labs(title = to_title_case(title)) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18)
      )
  }

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
#' @import patchwork
#' @import dplyr
#'
#' @examples
plot_palette <- function(pal, aes = "fill") {
  if(aes == "fill"){
    g <- mpg %>%
      ggplot(aes(x = displ, fill = class)) +
      geom_histogram()
    imap(pal, ~g + scale_fill_manual(values = colorRampPalette(.x)(7)) +
          labs(
            title = to_title_case(.y)
          ) +
           theme(
             plot.title = element_text(hjust = 0.5, size = 18)
           )) %>%
      wrap_plots()
  }else{
    g <- mpg %>%
      ggplot(aes(x = displ, y = hwy, colour = displ)) +
      geom_point(size = 7)
    imap(pal, ~g + scale_colour_gradientn(colours = colorRampPalette(.x[seq(1, length(.x), length = 3)])(200)) +
          labs(
            title = to_title_case(.y)
          ) +
           theme(
             plot.title = element_text(hjust = 0.5, size = 18)
           )) %>%
      wrap_plots()
  }
}






#' Title
#'
#' @param n_children
#' @param mutation_rate
#' @param selected_parents
#'
#' @return
#' @export
#'
#' @examples
evolve <- function(selected_parents, n_children, mutation_rate = 0.05, variation = 2) {
  n <- length(selected_parents)
  evolved <- map(1:n_children, ~{
    parents <- sample(1:n, 2, replace = TRUE)
    crossover(selected_parents[parents])
    }) %>%
    mutation(mutation_rate = mutation_rate, variation_parameter = variation) %>%
    map(~get_pal_order(.x))
  names(evolved) <- generate_palette_name(n_children)
  evolved
}




#' Title
#'
#' @return
#' @export
#'
#' @examples
launch_evo_palette <- function() {
  shiny::runApp(system.file('evoPaletteDash', package = 'evoPalette'))
}



#' Title
#'
#' @param clear Set to TRUE to clear the palette box. Default FALSE
#'
#' @return
#' @export
#'
#' @examples
open_palette_box <-  function(clear = FALSE) {
  if(clear) {
    gallery$palette_box <- NULL
    message("palette box cleared")
  }
  gallery$palette_box
}




#' Title
#'
#' @param n
#'
#' @return
#' @export
#'
#' @import snakecase
#' @import readr
#' @import dplyr
#'
#' @examples
generate_palette_name <- function(n) {
  adj <- read_rds(list.files(system.file('data/', package = 'evoPalette'), full.names = TRUE)[2]) %>%
    filter(type == "adjective") %>%
    .$word %>%
    sample(., n)
  food <- read_rds(list.files(system.file('data/', package = 'evoPalette'), full.names = TRUE)[1]) %>%
    .$food_word %>%
    sample(., n)
  to_title_case(paste(adj, food))
}
