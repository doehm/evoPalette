

# examples
example1 <- function(n_cols, n_parents, n_children) {

  parent_pals <- random_palette(n_cols, n_parents)
  show_palettes <- map(parent_pals, ~show_palette(.x))
  print(wrap_plots(show_palettes))

  again <- "y"
  children <- parent_pals
  while(again == "y") {

    mum <- as.numeric(readline("choose mum "))
    dad <- as.numeric(readline("choose dad "))

    children <- map(1:n_children, ~crossover(children[c(mum, dad)])) %>% mutation()
    children <- map(children, ~get_pal_order(.x))
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

