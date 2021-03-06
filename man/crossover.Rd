% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/palette-fns.R
\name{crossover}
\alias{crossover}
\title{Crossover}
\usage{
crossover(parents)
}
\arguments{
\item{parents}{List of two palettes of the same length.}
}
\value{

}
\description{
Implements the crossover step in the evolutionary algorithm.
}
\details{
A random number of genes are selected. The child gets the selected genes from one parent and the remaining from the other
parent. The output is a new palette of the same length as the parents.
}
\examples{
\dontrun{
set.seed(200422)
pals <- random_palette(5, 2)
new_pals <- map(1:2, ~crossover(pals))
parents <- c(pals, new_pals)
names(parents) <- c("parent1", "parent2", "child1", "child2")
imap(parents, ~show_palette(.x, .y, labels = TRUE)) \%>\%
    wrap_plots()
}
}
