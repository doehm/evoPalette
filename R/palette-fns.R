


#' Crossover
#'
#' Implements the crossover step in the evolutionary algorithm.
#'
#' @param parents List of two palettes of the same length.
#'
#' @details A random number of genes are selected. The child gets the selected genes from one parent and the remaining from the other
#' parent. The output is a new palette of the same length as the parents.
#'
#' @return
#' @export
#'
#' @examples
#' pals <- random_palette(5, 2)
#' new_pals <- map(1:2, ~crossover(pals))
#' parents <- c(pals, new_pals)
#' names(parents) <- c("parent1", "parent2", "child1", "child2")
#' imap(parents, ~show_palette(.x, .y)) %>% wrap_plots()
crossover <- function(parents) {
  n <- length(parents[[1]])
  id <- which(round(runif(n)) == 0)
  child <- parents[[1]]
  child[id] <- parents[[2]][id]
  child <- get_pal_order(child)
  child
}




#' Random palette
#'
#' Selects a random palette either from an exisiting set of palettes or completely random.
#'
#' @param n_palettes Number of palettes to generate. Numeric.
#' @param from Where to select or generate the palette from. See details.
#' @param n_cols Number of colours in the palette. Numeric.
#'
#' @details Palettes can be selected from \code{paletteer} or \code{random} for a completely random palette and if you're
#' feeling very lucky.
#'
#' @return
#' @export
#'
#' @importFrom glue glue
#' @import paletteer
#' @import purrr
#'
#' @examples
#' pals <- random_palette(5, 4)
#' wrap_plots(map(pals, ~show_palettes(.x)))
random_palette <- function(n_cols, n_palettes, from = "paletteer") {

  if(from == "random"){
    return(
      map(1:n_palettes, ~{
        rgb(r = runif(n_cols), g = runif(n_cols), b = runif(n_cols), runif(n_cols))
        }) %>%
        map(~get_pal_order(.x))
    )
  }

  if(from == "paletteer") {
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





#' Mutation
#'
#' Applies the mutation step in the evolutionary algorithm
#'
#' @param parents List of parents to apply the mutation step.
#' @param mutation_rate Mutation rate. Numeric value between 0-1 representing the probability of mutation of a single gene.
#' @param variation_parameter Random variation applied to every gene. Numeric value between 0-1.
#'
#' @details \code{variation_parameter} is a numeric value between 0-1 which is essentially the standard deviation of a beta
#' distribution. A colour is converted to RGB and each value is randomly drawn from a beta distribution with mean the existing
#' red, green or blue value on the 0-1 scale and approximate standard deviation of \code{variation_parameter}. In short smaller
#' values are less variable and larger values more variable.
#'
#' @return
#' @export
#'
#' @examples
#' pal <- random_palette(5, 2)
#' map(pal, ~show_palette(.x)) %>%
#'    wrap_plots()
#' pal %>%
#'    mutation() %>%
#'    map(~show_palette(.x)) %>%
#'    wrap_plots()
mutation <- function(parents, mutation_rate = 0.05, variation_parameter = 0.01) {
  if(is.character(parents)) {
    parents <- list(parents)
  }
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
#' plots the discrete and continuous colour palettes
#'
#' @param n Number of colours to show. Defaults to c(length(pal), 200)
#' @param pal Palette. Character vector.
#' @param title To add a title to the plot. Default NULL.
#' @param labels Adds number labels to the colour palette.
#' @param n_continuous Number of colours from the palette the continuous gradient uses. Default 3.
#'
#' @details By default the continuous scale is set by taking 3 equally spaced colours along the colour palette.
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @import snakecase
#'
#' @examples
#' random_palette(5, 2) %>%
#'     map(~show_palette(.x)) %>%
#'     wrap_plots()
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






#' Palette colour order
#'
#' Sorts colour palette by hue.
#'
#' @param pal Palette. CHaracter vector.
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
#' Plots a classic histogram using MPG data to test the colour palette and a continuous colour aesthetic example
#'
#' @param pal The palette. Character.
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @import patchwork
#' @import dplyr
#'
#' @examples
#' random_palette(5, 1)[[1]] %>%
#'     plot_palette()
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






#' Evolve function
#'
#' Takes a set of parents and spawns a given number of children.
#'
#' @param n_children Number of children to spawn
#' @param mutation_rate Mutation rate. Numeric (0-1)
#' @param selected_parents List of selected parents.
#' @param variation
#'
#' @details It is suitable to evolve only 1 selected parent. In this case crossover has not effect however random mutations can
#' still occur and the variation parameter will generate slight variations of the parent. Useful for tweaking a chosen palette.
#'
#' @return
#' @export
#'
#' @examples
evolve <- function(selected_parents, n_children, mutation_rate = 0.05, variation = 0.01) {
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




#' evoPalette app
#'
#' Launches the Shiny application for selecting and evolving colour palettes.
#'
#' @return
#' @export
#'
#' @import shinyalert
#' @import shinydashboard
#'
#' @examples
launch_evo_palette <- function() {
  shiny::runApp(system.file('evoPaletteDash', package = 'evoPalette'))
}



#' Palette box
#'
#' Opens the palette box containing saved palettes from the evoPalette Shiny app
#'
#' @param clear Set to TRUE to clear the palette box. Default FALSE
#'
#' @details Any palettes saved while using the shiny app are saved here. Opening the box displays a list of the saved palettes. More
#' Palettes will be saved here everytime you use the app during a single session. Remember to save the palette box before closing the
#' session.
#'
#' @return
#' @export
#'
#' @examples
open_palette_box <-  function(clear = FALSE, save = NULL) {
  if(clear) {
    gallery$palette_box <- NULL
    message("palette box cleared")
  }
  gallery$palette_box
}




#' Generate palette name
#'
#' Generates a palette name for evolved palettes. Names are generated from a random combination of adjectives and food related words.
#'
#' @param n Number of names to generate.
#'
#' @return
#' @export
#'
#' @import snakecase
#' @import readr
#' @import dplyr
#'
#' @examples
#' generate_palette_name(5)
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
