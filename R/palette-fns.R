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
#' set.seed(200422)
#' pals <- random_palette(5, 2)
#' new_pals <- map(1:2, ~crossover(pals))
#' parents <- c(pals, new_pals)
#' names(parents) <- c("parent1", "parent2", "child1", "child2")
#' imap(parents, ~show_palette(.x, .y, labels = TRUE)) %>%
#'     wrap_plots()
#'}
crossover <- function(parents) {
  n <- length(parents[[1]])
  id <- which(round(runif(n)) == 0)
  child <- parents[[1]]
  child[id] <- parents[[2]][id]
  child <- sort_palette(child)
  child
}




#' Random palette
#'
#' Selects a random palette either from an exisiting set of palettes or completely random.
#'
#' @param n_palettes Number of palettes to generate. Numeric.
#' @param n_cols Number of colours in the palette. Numeric.
#' @param feeling_lucky If \code{TRUE} generates completely random colours for the palette. Default FALSE
#'
#' @details Palettes can be randomly selected from existing palettes from \code{evoPalette} or \code{paletteer},
#' or completely random colours if you're feeling lucky.
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
        rgb(r = runif(n_cols), g = runif(n_cols), b = runif(n_cols), alpha = runif(n_cols))
        }) %>%
        map(~sort_palette(.x)) %>%
        set_names(generate_palette_name(n_palettes))
    )
  }else{
    pal_df <- palette_data %>%
      filter(n_cols < 10) %>%
      sample_n(n_palettes)

    pal_df$palette %>%
      map(~colorRampPalette(.x)(n_cols)) %>%
      set_names(pal_df$name)
  }
}







#' Read palette data
#'
#' Pulls together evoPalette and paletteer palettes ready for sampling
#'
#' @return
#' @export
#'
#' @importFrom purrr map2
#' @import dplyr
#'
palette_data <- {
  # not using full tidyverse syntax in order to pass the R CMD checks
  evo_df <- readRDS(system.file("extdata/palettes.rds", package = "evoPalette"))
  pltr_df <- as_tibble(paletteer::palettes_d_names)
  pltr_df$name <- pltr_df$palette
  pltr_df$n_cols <- pltr_df$length
  pltr_df$category <- pltr_df$type
  evo_df %>%
    bind_rows(
      pltr_df %>%
        mutate(palette = purrr::map2(pltr_df$package, pltr_df$name, ~as.character(paletteer::paletteer_d(glue("{.x}::{.y}")))))
    ) %>%
    select(name, package, category, n_cols, palette)
}






#' Mutation
#'
#' Applies the mutation step in the evolutionary algorithm
#'
#' @param parents List of parents to apply the mutation step.
#' @param mutation_rate Mutation rate. Numeric value between 0-1 representing the probability of mutation of a single colour.
#' @param variation_parameter Numeric value between 0-1. Random variation applied to every gene.
#'
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
    map(~sort_palette(.x))
}








#' Show palette
#'
#' plots a given discrete and continuous colour palette
#'
#' @param pal Palette. Character vector.
#' @param title To add a title to the plot. Default \code{NULL}.
#' @param n Number of colours to show. Defaults to \code{c(length(pal), 200)}.
#' @param n_sat Number of saturation levels
#' @param labels Logical. Adds the hex label to the colour palette.
#' @param n_continuous Number of colours from the palette the continuous gradient uses. Default 3.
#' @param title_size Font size of title
#' @param show_continuous Show continuous scale
#'
#' @details By default the continuous scale is set by taking 3 equally spaced colours along the colour palette. Can set to more
#' or less if desired.
#'
#' @return
#' @export
#'
#' @import tidyr
#' @import ggplot2
#' @import snakecase
#' @import stringr
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' \dontrun{
#' random_palette(5, 2) %>%
#'     map(~show_palette(.x)) %>%
#'     wrap_plots()
#'     }
show_palette <- function(
  pal,
  title = NULL,
  n = NULL,
  n_sat = 1,
  labels = FALSE,
  n_continuous = 2,
  title_size = 18,
  show_continuous = FALSE,
  show_divergent = FALSE
  ){

  if(is.list(pal) & is.null(title)) {
    title <- names(pal)
    pal <- pal[[1]]
  }

  if(is.null(n)) n <- c(length(pal), 200)

  df1 <- map2_dfc(pal, 1:length(pal), ~{
    colour <- colorRampPalette(c("white", .x))(n_sat+1)[-1]
    # change the saturation in here
    tibble(colour = colour) %>%
      set_names(paste0("colour", .y))
    }) %>%
    mutate(saturation = 1:n_sat) %>%
    pivot_longer(cols = -saturation, names_to = "colour", values_to = "fill") %>%
    mutate(
      xmin = (as.numeric(str_extract(colour, "[:digit:]"))-1)/n[1],
      xmax = as.numeric(str_extract(colour, "[:digit:]"))/n[1] + 0.001,
      ymin = (saturation-1)/n_sat,
      ymax = ymin + 1/n_sat
    )

  x <- seq(0, 1, length = n[2]+1)
  y <- c(-1/n_sat, 0)
  type <- paste0("x", 1:(n[2]+1))
  df2 <- tibble(xmin = x[-(n[2]+1)], xmax = x[-1]+0.001, ymin = y[1], ymax = y[2], type = type[-1])

  x_text <- seq(0, 1, length = n[1]+1)
  d <- mean(df1$xmin[1:2])
  g <- ggplot() +
    theme_void() +
    annotate("rect", xmin = df1$xmin, xmax = df1$xmax, ymin = df1$ymin, ymax = df1$ymax, fill = df1$fill)

  if(show_continuous) g <- g + annotate("rect", xmin = df2$xmin, xmax = df2$xmax, ymin = df2$ymin, ymax = df2$ymax, fill = colorRampPalette(pal[seq(1, n[1], length = n_continuous)])(n[2]))
  if(show_divergent) g <- g + annotate("rect", xmin = df2$xmin, xmax = df2$xmax, ymin = df2$ymin-1/n_sat*show_continuous, ymax = df2$ymax-1/n_sat*show_continuous, fill = colorRampPalette(c(pal[1], "white", pal[length(pal)]))(n[2]))
  if(labels) g <- g + annotate("text", x = (df1$xmin + df1$xmax)/2, y = (df1$ymin + df1$ymax)/2, label = pal)
  if(!is.null(title)) {
    g <- g +
      labs(title = to_title_case(title)) +
      theme(
        plot.title = element_text(hjust = 0.5, size = title_size)
      )
  }

  g
}






#' Palette colour order
#'
#' Sorts colour palette by hue.
#'
#' @param pal Palette. Character vector of hex codes.
#' @param rgb_hsv Sort by either rgb, hsv, both, none or normal. See details.
#'
#' @details Sorting colours is near impossible to get it right everytime. This allows sorting by a few methods. Default is \code{both}.
#' \itemize{
#' \item{\code{rgb}}: Sorts by RGB by first finding the lightest, then it's nearest neighbour and so on.
#' \item{\code{hsv}}: Sorts by HSV by first finding the most saturated, then it's nearest neighbour and so on.
#' \item{\code{both}}: Both RGB and HSV.
#' \item{\code{normal}}: Simple character sort on hex codes.
#' \item{\code{none}}: Returns the same order. Can be convenient.
#' }
#'
#' @return
#'
#' @importFrom grDevices col2rgb rgb2hsv
#' @importFrom stats dist sd
#'
#' @examples
#' \dontrun{
#' pal <- c("#564862", "#EEFBFD", "#594543", "#8E4B3E", "#BF6856")
#' list(
#'     unsorted = pal,
#'     sorted = sort_palette(pal)
#'     ) %>%
#'     imap(~show_palette(.x, .y)) %>%
#'     wrap_plots()
#' }
dep_sort_palette <- function(pal, rgb_hsv = "choose"){

  if(!rgb_hsv %in% c("rgb", "hsv", "h", "both", "normal", "none", "choose")) stop("valid methods for rgb_hsv: rgb, hsv, h, both, normal, none.")

  .rgb <- t(col2rgb(pal))/255

  if(rgb_hsv == "choose") {
    x_array <- cbind(.rgb, t(rgb2hsv(.rgb[,1], .rgb[,2], .rgb[,3])), l = sqrt(0.241*.rgb[,1] + 0.691*.rgb[,2] + 0.068*.rgb[,3]))
    x_metrics <- apply(x_array, 2, sd)
    x_rgb <- sum(x_metrics[1:3])
    x_hsv <- sum(x_metrics[4:6])
    rgb_hsv <- ifelse(x_rgb > x_hsv, "rgb", "hsv")
  }else if(rgb_hsv == "none") {
    return(pal)
  }else if(rgb_hsv == "normal") {
    return(sort(pal))
  }

  browser()

  col_vector <- switch(rgb_hsv,
    rgb = .rgb,
    hsv = t(rgb2hsv(.rgb[,1], .rgb[,2], .rgb[,3])),
    both = cbind(.rgb, t(rgb2hsv(.rgb[,1], .rgb[,2], .rgb[,3])), l = sqrt(0.241*.rgb[,1] + 0.691*.rgb[,2] + 0.068*.rgb[,3])),
    h = t(rgb2hsv(.rgb[,1], .rgb[,2], .rgb[,3]))[,1]
  )

  d <- as.matrix(dist(col_vector))
  id <- sort(apply(col_vector, 1, min), decreasing = TRUE, index.return = TRUE)$ix
  begin <- id[1]
  d[d == 0] <- 99
  nm <- as.numeric(colnames(d))
  order <- rep(NA, length(pal))
  order[1] <- begin
  for(k in 1:(length(pal)-1)) {
    order[k+1] <- which.min(d[order[k],])
    d[,order[k]] <- 99
  }
  pal[order]
}




#' Palette colour order
#'
#' Sorts colour palette by hue.
#'
#' @param pal Palette. Character vector of hex codes.
#' @param rgb_hsv Sort by either rgb, hsv, both, none or normal. See details.
#'
#' @details Sorting colours is near impossible to get it right everytime. This allows sorting by a few methods. Default is \code{both}.
#' \itemize{
#' \item{\code{rgb}}: Sorts by RGB by first finding the lightest, then it's nearest neighbour and so on.
#' \item{\code{hsv}}: Sorts by HSV by first finding the most saturated, then it's nearest neighbour and so on.
#' \item{\code{both}}: Both RGB and HSV.
#' \item{\code{normal}}: Simple character sort on hex codes.
#' \item{\code{none}}: Returns the same order. Can be convenient.
#' }
#'
#' @return
#' @export
#'
#' @importFrom grDevices col2rgb rgb2hsv
#' @importFrom stats dist sd
#'
#' @examples
#' \dontrun{
#' pal <- c("#564862", "#EEFBFD", "#594543", "#8E4B3E", "#BF6856")
#' list(
#'     unsorted = pal,
#'     sorted = sort_palette(pal)
#'     ) %>%
#'     imap(~show_palette(.x, .y)) %>%
#'     wrap_plots()
#' }
sort_palette <- function(pal, method = "choose"){

  .rgb <- col2rgb(pal)/255
  x_array <- t(rbind(
    .rgb,
    rgb2hsv(.rgb),
    l = sqrt(0.241*.rgb[,1] + 0.691*.rgb[,2] + 0.068*.rgb[,3]))
    )
  colnames(x_array) <- c("r", "g", "b", "h", "s", "v", "l")
  x_metrics <- apply(x_array, 2, sd)
  x_rgb <- sum(x_metrics[1:3])
  x_hsv <- sum(x_metrics[4:6])
  if(method == "choose") {
    method <- ifelse(x_rgb > x_hsv, "rgb", "hsv")
  }else if(method == "none") {
    return(pal)
  }else if(method == "normal") {
    return(sort(pal))
  }

  if(!method %in% c("choose", "none", "normal")) method <- str_split(method, "")[[1]]

  col_vector <- x_array[, method, drop = FALSE]

  d <- as.matrix(dist(col_vector))
  id <- sort(apply(col_vector, 1, min), decreasing = TRUE, index.return = TRUE)$ix
  begin <- id[1]
  d[d == 0] <- 99
  nm <- as.numeric(colnames(d))
  order <- rep(NA, length(pal))
  order[1] <- begin
  for(k in 1:(length(pal)-1)) {
    order[k+1] <- which.min(d[order[k],])
    d[,order[k]] <- 99
  }

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
#' still occur and the \code{variation} parameter will generate slight variations of the parent. Useful for tweaking a chosen palette.
#'
#' If selecting 3 or more parents, two are chosen at random to spawn a single child. This is repeated \code{n_children} times.
#'
#' @importFrom purrr map
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples \dontrun{}
evolve <- function(selected_parents, n_children, mutation_rate = 0.05, variation = 0.01) {
  n <- length(selected_parents)
  evolved <- map(1:n_children, ~{
    parents <- sample(1:n, 2, replace = TRUE)
    crossover(selected_parents[parents])
    }) %>%
    mutation(mutation_rate = mutation_rate, variation_parameter = variation) %>%
    map(~sort_palette(.x))
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
#' @examples \dontrun{}
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
#' @examples \dontrun{}
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
#' @details This function randomly draws an adjective and a food related word from a dictionary The name is assigned to a newly generated
#' palette. Palette name can be changed when saving.
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
  adjective <- read_rds(system.file('extdata/adjectives.rds', package = 'evoPalette'))
  adjective <- sample(adjective$word[adjective$type == "adjective"], n)
  food <- read_rds(system.file('extdata/food-words.rds', package = 'evoPalette'))
  food <- sample(food$food_word, n)
  to_title_case(paste(adjective, food))
}
