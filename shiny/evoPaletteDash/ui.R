library(shinydashboard)
library(paletteer)
library(purrr)


dashboardPage(skin = "green",
              dashboardHeader(title = "evoPalette"),
              dashboardSidebar(
                  sidebarMenu(
                      HTML(
                          "<p>To begin select the number<br />
                          of colours to generate for<br />
                          each palette and the number<br />
                          of palettes to generate.<br />
                          Select the palettes you like the<br />
                          most using the checkbox<br />
                          and click evolve to spawn new<br />
                          palettes.</p>
                          <p>To begin again uncheck all<br />
                          boxes and click evolve.</p>"
                           ),
                      menuItem("Palettes", tabName = "dash", icon = icon("fill-drip")),
                      menuItem("Example: Fill aesthetic", tabName = "palette_examples_d", icon = icon("chevron-circle-right")),
                      menuItem("Example: Colour aesthetic", tabName = "palette_examples_c", icon = icon("chevron-circle-right")),
                      numericInput("n_cols", "Number of colours", value = 5, min = 3, max = 20),
                      numericInput("n_palettes", "Number of Palettes to generate", value = 4, min = 2, max = 12),
                      numericInput("mutation_rate", "Mutation rate", 0.05),
                      numericInput("variation", "Variation", 5),
                      uiOutput("selected_parents_ui"),
                      actionButton("evolve_button", "Evolve"),
                      HTML(
                        "<p>The saved palette can be retrieved with  once the<br />
                        app is closed</p>"
                      ),
                      useShinyalert(),
                      actionButton("save_palette", "Save"),
                      plotOutput("saved_pal", height = 100, width = 160)
                  )
              ),
              dashboardBody(
                  tabItems(
                      tabItem(tabName = "dash",
                              fluidRow(
                                  plotOutput("palette_plots", height = 1180)
                              )
                      ),
                      tabItem(tabName = "palette_examples_d",
                              fluidRow(
                                  plotOutput("palette_examples", height = 1180)
                              )
                      ),
                      tabItem(tabName = "palette_examples_c",
                              fluidRow(
                                plotOutput("palette_examples_colour", height = 1180)
                              )
                      )
                  )
              )
)
