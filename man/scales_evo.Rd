% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scales.R
\name{evo_pal}
\alias{evo_pal}
\alias{scale_fill_evo}
\alias{scale_colour_evo}
\title{evoPalette palette function}
\usage{
evo_pal(
  name,
  scale_type = "d",
  divergent = FALSE,
  sequential = FALSE,
  lum = 1,
  reverse = FALSE,
  ...
)

scale_fill_evo(
  name = NULL,
  scale_type = "d",
  lum = 1,
  reverse = FALSE,
  divergent = FALSE,
  sequential = FALSE,
  ...
)

scale_colour_evo(
  name,
  scale_type = "d",
  lum = 1,
  reverse = FALSE,
  divergent = FALSE,
  sequential = FALSE,
  ...
)
}
\arguments{
\item{name}{Name of palette in the palette box}

\item{scale_type}{Discrete or continuous. Input \code{d} / \code{c}.}

\item{divergent}{Use divergent scale? Use a two element vector to customise the scale e.g. \code{c(2, 5)}}

\item{sequential}{Use sequential scale? Use an integer to customise the scale e.g. \code{2}}

\item{lum}{Luminance value level (0-1) to dial down the intensity of the generated palette.}

\item{reverse}{Logical. Reverse the palette?}

\item{...}{Dots}
}
\value{





}
\description{
Generates a colour palette function for a discrete or continuous scale
}
\details{
The evoPalette app needs to be run and at least one palette saved to use the scale functions. The scales
refer to the palette by name.
}
\examples{
\dontrun{}
\dontrun{}
\dontrun{}
}
