
<!-- README.md is generated from README.Rmd. Please edit that file -->

# evoPalette

evoPalette allows for the discovery and generation of new colour
palettes for R. Taking user input it uses a genetic algorithm to spawn
new palettes. This can be repeated as many times as needed to generate
the desired palette.

## Installation

``` r
devtools::install_github("doehm/evoPalette")
```

## Getting started

To jump straight into it launch the Shiny app with

``` r
launch_evo_palette()
```

To generate the first set of palettes, click ‘evolve’.

Select one or more palettes that you like and think may work well
together from the checkbox and click ‘evolve’. The next generation of
palettes will be created. Continue until you are happy with a palette.

To save a palette, select the desired palette from the checkbox and
click ‘save’. The palette is now accessible from
<code>open\_palette\_box()</code> once the app is closed. You can save
multiple palettes which will be collected in the palette box.

## Control

Select the parameters from the drop down in the menu.

  - Number of colours to generate for each palette
  - Number of palettes to generate at each evolution
  - Mutation rate - Each colour in the palette has a probability of
    random mutation equal to this value (default p = 0.05)
  - Mutation variation - Each colour will vary slightly around it’s
    original value e.g. like a child not being the same height as either
    of their parents.
  - Load an existing palette from the global environment

When you are happy with your selections, click ‘evolve’
