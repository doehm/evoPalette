
dashboardPage(skin = "yellow",
              dashboardHeader(title = "evoPalette"),
              dashboardSidebar(
                  sidebarMenu(
                      HTML(
                          "<p><br /1>
                          To begin select the number of colours<br />
                          for each palette and the number of <br />
                          palettes to generate. Click evolve.<br />
                          </p>
                          <p>
                          Select the palettes you like the most<br />
                          using the checkbox. Click evolve to<br />
                          spawn the next generation of <br />
                          palettes.
                          </p>
                          <p>
                          To begin again uncheck all boxes and <br />
                          click evolve.<br /></p>"
                           ),
                      menuItem("Palettes", tabName = "dash", icon = icon("fill-drip")),
                      menuItem("Example: Fill aesthetic", tabName = "palette_examples_d", icon = icon("chevron-circle-right")),
                      menuItem("Example: Colour aesthetic", tabName = "palette_examples_c", icon = icon("chevron-circle-right")),
                      menuItem("Parameters", tabName = "config",
                               numericInput("n_cols", "Number of colours", value = 6, min = 3, max = 20),
                               numericInput("n_palettes", "Number of Palettes", value = 6, min = 2, max = 20),
                               numericInput("mutation_rate", "Mutation rate (0 - 1)", 0.05),
                               numericInput("variation", "Variation (0 - 1)", 0.01),
                               pickerInput("sortby", "Sort by ...", sort_methods, "Both"),
                               textInput("load_palette", "Load palettes (name of list)"),
                               checkboxInput("feeling_lucky", "Feeling lucky", FALSE)
                               ),
                      uiOutput("selected_parents_ui"),
                      actionButton("evolve_button", "Evolve"),
                      HTML(
                        "<p> <br />
                        The saved palettes can be retrieved<br />
                        with palette_box(). Save one palette<br />
                        at a time.
                        </p>"
                      ),
                      useShinyalert(),
                      actionButton("save_palette", "Save"),
                      plotOutput("saved_pal", height = 100, width = '100%')
                  )
              ),
              dashboardBody(
                tags$style(type = "text/css", "#palette_plots {height: calc(100vh - 80px) !important;}"),
                tags$style(type = "text/css", "#palette_examples {height: calc(100vh - 80px) !important;}"),
                tags$style(type = "text/css", "#palette_examples_colour {height: calc(100vh - 80px) !important;}"),
                  tabItems(
                      tabItem(tabName = "dash",
                              fluidRow(
                                  plotOutput("palette_plots")
                              )
                      ),
                      tabItem(tabName = "palette_examples_d",
                              fluidRow(
                                  plotOutput("palette_examples")
                              )
                      ),
                      tabItem(tabName = "palette_examples_c",
                              fluidRow(
                                plotOutput("palette_examples_colour")
                              )
                      )
                  )
              )
)
