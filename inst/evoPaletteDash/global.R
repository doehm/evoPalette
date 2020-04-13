library(ggplot2)
library(shinydashboard)
library(shinyalert)
library(paletteer)
library(purrr)
library(patchwork)
library(snakecase)

if(!exists("gallery")) {
  gallery <- new.env()
  gallery$palette_box <- list()
}
gallery$current_palette <- NULL
gallery$random <- TRUE
