library(ggplot2)
library(shinydashboard)
library(shinyalert)
library(paletteer)
library(purrr)
library(patchwork)
library(snakecase)
library(shinyWidgets)

if(!exists("gallery")) {
  gallery <- new.env()
  gallery$palette_box <- list()
}
gallery$current_palette <- NULL
gallery$random <- TRUE
gallery$generation <- 0
gallery$history <- list()
gallery$parent_history <- list()
gallery$n_parents <- 0

plot_palette <- function(pal, aesthetic = "fill") {
  if(is.character(pal)) pal <- list(pal)
  if(aesthetic == "fill"){
    g <- ggplot(ggplot2::mpg, aes(x = ggplot2::mpg$displ, fill = class)) + geom_histogram(bins = 30)
    imap(pal, ~{
      if(length(.x) < 7) {
        cols <- colorRampPalette(.x)(7)
      }else{
        cols <- .x[1:7]
      }
      g +
        scale_fill_manual(values = cols) +
        labs(
          title = to_title_case(.y),
          x = "Engine size (L)",
          fill = "Vehicle\nclass"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 18)
        )}) %>%
      wrap_plots()
  }else{
    g <- ggplot(ggplot2::mpg, aes(x = ggplot2::mpg$displ, y = ggplot2::mpg$hwy, colour = ggplot2::mpg$displ)) + geom_point(size = 7)
    imap(pal, ~g +
           scale_colour_gradientn(colours = colorRampPalette(.x[seq(1, length(.x), length = 3)])(200)) +
           labs(
             title = to_title_case(.y),
             x = "Engine size (L)",
             y = "MPG on highway",
             colour = "Engine\nsize"
           ) +
           theme_minimal() +
           theme(
             plot.title = element_text(hjust = 0.5, size = 18)
           )) %>%
      wrap_plots()
  }
}

