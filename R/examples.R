# examples
example <- function() {

  rgb_grid <- expand_grid(r = 0:25, g = 0:25, b = 0:25)

  parents <- list(
    mum = sample(1:nrow(rgb_grid), 5),
    dad = sample(1:nrow(rgb_grid), 5)
  )

  parents_b <- map(parents, ~chromosome(.x))

  mum <- show_palette(pal = make_palette(parents_b$mum))
  dad <- show_palette(pal = make_palette(parents_b$dad))


  again <- "y"
  while(again == "y") {

    mum <- show_palette(pal = make_palette(parents_b$mum))
    dad <- show_palette(pal = make_palette(parents_b$dad))

    children <- crossover(parents_b)

    g1 <- show_palette(pal = make_palette(children$child1))
    g2 <- show_palette(pal = make_palette(children$child2))

    print((mum + dad) / (g1 + g2))

    parents_b$mum <- children$child1
    parents_b$dad <- children$child2

    again <- readline("again?  ")

  }
}

