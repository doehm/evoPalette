
function(input, output) {

    # generate palettes either
    pals_gen <- eventReactive(input$evolve_button, {

        # reset gallery if no parent selected
        if(is.null(input$selected_parents_ui)) {
            gallery$random <- TRUE
            gallery$current_palette <- NULL
            gallery$random <- TRUE
            gallery$generation <- 0
            gallery$history <- list()
            gallery$parent_history <- list()
        }

        # randomise palettes
        if(gallery$random) {
            gallery$random <- FALSE
            gallery$current_palette <- random_palette(input$n_cols, input$n_palettes, input$feeling_lucky) %>%
                map(sort_palette)
        }else{
            # evolved palettes
            gallery$current_palette <- evolve(
                gallery$current_palette[input$selected_parents_ui],
                n_children = input$n_palettes,
                mutation_rate = input$mutation_rate,
                variation = input$variation + 0.0001
                )
        }
        if(input$load_palette != "") {
            p0 <- get(input$load_palette, envir = .GlobalEnv)
            n <- length(p0)
            gallery$current_palette <- c(p0, gallery$current_palette[1:(input$n_palettes-n)])
        }

        # naming the current palette
        names(gallery$current_palette) <- tolower(to_parsed_case(names(gallery$current_palette)))

        # update generation count
        gallery$generation <- gallery$generation + 1

        # store palette hstory
        gallery$history[[gallery$generation]] <-gallery$current_palette

        # store parent history
        gallery$parent_history[[gallery$generation]] <- input$selected_parents_ui

        # output current palette
        gallery$current_palette
    })

    pals <- reactive({
        map(pals_gen(), ~sort_palette(.x))
    })


    palettes <- reactive({
        show_palettes <- imap(pals(), ~show_palette(.x, title = .y))
        wrap_plots(show_palettes)
    })

    palette_examples_fill <- reactive({
        plot_palette(pals())
    })

    palette_examples_colour <- reactive({
        plot_palette(pals(), aes = "colour")
    })

    observeEvent(input$save_palette, {
        shinyalert("Palette name", "Select a name for your palette...", type = "input", inputValue = to_title_case(input$selected_parents_ui[1]))
    })

    save_pal_name_input <- eventReactive(input$shinyalert, {
        name <- tolower(to_parsed_case(input$shinyalert))
        gallery$palette_box[[name]] <- gallery$current_palette[input$selected_parents_ui][[1]]
        show_palette(gallery$palette_box[[name]], title = input$shinyalert, title_size = 10)
    })

    parents <- eventReactive(input$evolve_button, {
        if(gallery$generation > 1) {
            t <- gallery$generation-1
            parents <- gallery$parent_history[[t+1]]
            gallery$n_parents <- length(parents)
            gallery$history[[t]][parents] %>%
                imap(~show_palette(.x, title = .y, title_size = 10)) %>%
                wrap_plots(ncol = 1)
        }else{
            a <- rgb(35/255, 45/255, 50/255)
            ggplot() +
                geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1), fill = a) +
                theme_void() +
                theme(plot.background = element_rect(fill = a, colour = a))
        }
    })

    output$parents_title <- renderText({
        input$selected_parents_ui
        if(gallery$generation > 1) {
            "Parents of generation"
        }
    })


    # render palettes
    output$palette_plots <- renderPlot({
        palettes()
    })

    # render example plots
    output$palette_examples <- renderPlot({
        palette_examples_fill()
    })

    output$palette_examples_colour <- renderPlot({
        palette_examples_colour()
    })






    # render checkbox ui
    output$selected_parents_ui <- renderUI({
        checkboxGroupInput("selected_parents_ui", "Select parents", choiceNames = to_title_case(names(pals())), choiceValues = names(pals()), inline = FALSE)
    })

    output$saved_pal <- renderPlot({
        save_pal_name_input()
    })

    output$parents_of <- renderPlot({
        parents()
    })

}

