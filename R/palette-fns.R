


#' Crossover
#'
#' Implements the crossover step in the evolutionary algorithm.
#'
#' @param parents List of two palettes of the same length.
#'
#' @details A random number of genes are selected. The child gets the selected genes from one parent and the remaining from the other
#' parent. The output is a new palette of the same length as the parents.
#'
#' @importFrom stats runif
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pals <- random_palette(5, 2)
#' new_pals <- map(1:2, ~crossover(pals))
#' parents <- c(pals, new_pals)
#' names(parents) <- c("parent1", "parent2", "child1", "child2")
#' imap(parents, ~show_palette(.x, .y)) %>%
#'     wrap_plots()
#'}
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
#' @param n_cols Number of colours in the palette. Numeric.
#' @param feeling_lucky If TRUE generates completely random colours for the palette. Default FALSE
#'
#' @details Palettes can be randomly selected from existing palettes from \code{evoPalette} or \code{paletteer},
#' or completely random colours if you're feeling very lucky.
#'
#' @return
#' @export
#'
#' @import paletteer
#' @importFrom glue glue
#' @importFrom purrr map set_names
#' @importFrom patchwork wrap_plots
#'
#' @examples
#' \dontrun{
#' pals <- random_palette(5, 4)
#' map(pals, ~show_palette(.x)) %>%
#'     wrap_plots
#'}
random_palette <- function(n_cols, n_palettes, feeling_lucky = FALSE) {

  if(feeling_lucky){
    return(
      map(1:n_palettes, ~{
        rgb(r = runif(n_cols), g = runif(n_cols), b = runif(n_cols), runif(n_cols))
        }) %>%
        map(~get_pal_order(.x)) %>%
        set_names(generate_palette_name(n_palettes))
    )
  }else{
    pal_df <- get_palette_data() %>%
      filter(n_cols < 10) %>%
      sample_n(n_palettes)

    pal_df$palette %>%
      map(~colorRampPalette(.x)(n_cols)) %>%
      set_names(pal_df$name)
  }
}





#' Read palette data
#'
#' Pulls together evoPalette and paletteer palettes ready to sample
#'
#' @return
#' @export
#'
#' @importFrom purrr map2
#' @import dplyr
#'
#' @examples
#' \dontrun{get_palette_data()}
get_palette_data <- function() {
  # not using tidyverse syntax in order to pass the R CMD checks
  # more clunky but it works
  evo_df <- readRDS("C:/Users/Dan/Google Drive/R Code/my-packages/evoPalette/inst/extdata/palettes.rds")
  pltr_df <- as_tibble(paletteer::palettes_d_names)
  pltr_df$name <- pltr_df$palette
  pltr_df$n_cols <- pltr_df$length
  pltr_df$category <- pltr_df$type
  evo_df %>%
    bind_rows(
      pltr_df %>%
        mutate(palette = purrr::map2(pltr_df$package, pltr_df$name, ~as.character(paletteer::paletteer_d(glue("{.x}::{.y}")))))
    ) %>%
  select(1, 5, 2, 4, 3)
}






#' Mutation
#'
#' Applies the mutation step in the evolutionary algorithm
#'
#' @param parents List of parents to apply the mutation step.
#' @param mutation_rate Mutation rate. Numeric value between 0-1 representing the probability of mutation of a single gene.
#' @param variation_parameter Random variation applied to every gene. Numeric value between 0-1.
#'
#' @details A colour is converted to RGB and each value is randomly drawn from a beta distribution
#' red, green or blue values on the 0-1 scale as the mean and approximate standard deviation of \code{variation_parameter}. In short smaller
#' values are less variable and larger values more variable.
#'
#' The \code{mutation_rate} is the probability any given colour will be replaced with a completely random one.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pal <- random_palette(5, 2)
#' original <- map(pal, ~show_palette(.x, "original"))
#' mutated <- pal %>%
#'     mutation() %>%
#'     map(~show_palette(.x, "mutated"))
#' c(original, mutated) %>%
#'     wrap_plots
#' }
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
      random_rgb <- random_palette(length(.x), 1, feeling_lucky = TRUE)[[1]]
      df[mutation_id] <- random_rgb[mutation_id]
      df
    }) %>%
    map(~get_pal_order(.x))
}








#' Show palette
#'
#' plots a given discrete and continuous colour palettes
#'
#' @param n Number of colours to show. Defaults to \code{c(length(pal), 200)}.
#' @param pal Palette. Character vector.
#' @param title To add a title to the plot. Default \code{NULL}.
#' @param labels Adds number labels to the colour palette.
#' @param n_continuous Number of colours from the palette the continuous gradient uses. Default 3.
#'
#' @details By default the continuous scale is set by taking 3 equally spaced colours along the colour palette. Can set to more
#' or less if desired.
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @import snakecase
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' \dontrun{
#' random_palette(5, 2) %>%
#'     map(~show_palette(.x)) %>%
#'     wrap_plots()
#'     }
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
#' @param pal Palette. Character vector of hex codes.
#' @param rgb_hsv Sort by either rgb, hsv or both
#'
#' @return
#' @export
#'
#' @importFrom grDevices col2rgb rgb2hsv
#' @importFrom stats dist
#'
#' @examples
#' \dontrun{
#' pal <- c("#564862", "#EEFBFD", "#594543", "#8E4B3E", "#BF6856")
#' list(
#'     unsorted = pal,
#'     sorted = get_pal_order(pal)
#'     ) %>%
#'     imap(~show_palette(.x, .y)) %>%
#'     wrap_plots()
#' }
get_pal_order <- function(pal, rgb_hsv = "both"){
  .rgb <- t(col2rgb(pal))
  col_vector <- switch(rgb_hsv,
    rgb = .rgb,
    hsv = t(rgb2hsv(.rgb[,1], .rgb[,2], .rgb[,3])),
    both = cbind(.rgb, t(rgb2hsv(.rgb[,1], .rgb[,2], .rgb[,3])))
  )
  d <- dist(col_vector)
  begin <- which(pal == sort(pal)[1])
  order <- as.numeric(names(sort(as.matrix(d)[begin,])))
  pal[order]
}





#' Evolve function
#'
#' Takes a set of parent palettes and spawns a given number of children.
#'
#' @param n_children Number of children to spawn.
#' @param mutation_rate Mutation rate. Numeric (0-1).
#' @param selected_parents List of selected parents.
#' @param variation Variation parameter. Controls the extent a single colour varies during mutation.
#'
#' @details It is possible to evolve only 1 selected parent. In this case crossover has no effect however random mutations can
#' still occur and the variation parameter will generate slight variations of the parent. Useful for tweaking a chosen palette.
#'
#' If selecting 3 or more parents, two are chosen at random to spawn a single child. This is repeated \code{n_children} times.
#'
#' The \code{variation} parameter varies a single colour by adjusting the standard error of a beta distribution.
#'
#' @importFrom purrr map
#' @importFrom dplyr %>%
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
#' @details To begin select the number of colours for each palette and the number of palettes to generate. Click evolve.
#'
#' Select the palettes you like the most using the checkbox. Click evolve to spawn the next generation of palettes.
#'
#' To begin again uncheck all boxes and click evolve.
#'
#' To start with existing palettes enter the object name into the load palettes text input. The object should be a list of
#' palettes, ideally with names.
#'
#' Use 'parameters' to adjust the extent of mutations and variations in the algorthm.
#'
#' Save a palette by selecting the checkbox of the desired palette and click save. You can save multiple palettes to the palette
#' box buy hitting 'save' for each palette individually. Upon closing the app, palettes can be retrieved by \code{palette_box()}.
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyalert useShinyalert shinyalert
#' @importFrom shinyWidgets pickerInput
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
#' Palettes will be saved here everytime the app is used with a session. Remember to save the palette box to disk before closing the
#' session to permanently save the palette.
#'
#' @return
#' @export
#'
#' @examples
palette_box <-  function(clear = FALSE) {
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
#' @importFrom snakecase to_title_case
#' @importFrom readr read_rds
#'
#' @examples
#' \dontrun{generate_palette_name(5)}
generate_palette_name <- function(n) {
  files <- list.files(system.file('extdata', package = 'evoPalette'), full.names = TRUE)
  adjective <- read_rds(grep("adjectives", files, value = TRUE))
  adjective <- sample(adjective$word[adjective$type == "adjective"], n)
  food <- read_rds(grep("food", files, value = TRUE))
  food <- sample(food$food_word, n)
  to_title_case(paste(adjective, food))
}
