

# examples
example1 <- function(n_cols, n_parents, n_children, nrow, ncol) {

  parent_pals <- map(1:n_parents, ~{
    rgb(r = runif(n_cols), g = runif(n_cols), b = runif(n_cols))
  })

  parent_pals <- map(parent_pals, ~get_pal_order(.x))

  show_palettes <- map(parent_pals, ~show_palette(.x))
  print(wrap_plots(show_palettes))

  again <- "y"
  children <- parent_pals
  while(again == "y") {

    mum <- as.numeric(readline("choose mum "))
    dad <- as.numeric(readline("choose dad "))

    children <- map(1:n_children, ~get_pal_order(crossover(children[c(mum, dad)])))

    show_palettes <- map(children, ~show_palette(.x))
    print(wrap_plots(show_palettes))


    again <- NA
    message("again? ")
    while(!again %in% c("y", "n")) {
      message("please choose y / n")
      again <- readline("")
    }
  }
  pal <- as.numeric(readline("select palette: "))
  print(show_palette(children[[pal]]) / plot_palette(children[[pal]]))
  children[[pal]]
}

