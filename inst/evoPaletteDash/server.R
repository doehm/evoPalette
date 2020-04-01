
function(input, output) {

    # generate palettes either
    pals <- eventReactive(input$evolve_button, {
        if(is.null(input$selected_parents_ui)) {
            gallery$random <- TRUE
        }
        if(gallery$random) {
            gallery$random <- FALSE
            gallery$current_palette <- random_palette(input$n_cols, input$n_palettes)
        }else{
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
        gallery$current_palette
    })

    palettes <- reactive({
        show_palettes <- imap(pals(), ~show_palette(.x, title = .y))
        wrap_plots(show_palettes)
    })

    palette_examples <- reactive({
        plot_palette(pals())
    })

    palette_examples_colour <- reactive({
        plot_palette(pals(), aes = "colour")
    })

    observeEvent(input$save_palette, {
        shinyalert("Palette name", "Select a name for your palette...", type = "input", inputValue = input$selected_parents_ui[1])
    })

    save_pal_name_input <- eventReactive(input$shinyalert, {
        gallery$palette_box[[input$shinyalert]] <- gallery$current_palette[input$selected_parents_ui][[1]]
        show_palette(gallery$palette_box[[input$shinyalert]], title = input$shinyalert)
    })

    # render palettes
    output$palette_plots <- renderPlot({
        palettes()
    })



    # render example plots
    output$palette_examples <- renderPlot({
        palette_examples()
    })

    output$palette_examples_colour <- renderPlot({
        palette_examples_colour()
    })



    # render checkbox ui
    output$selected_parents_ui <- renderUI({
        checkboxGroupInput("selected_parents_ui", "Select parents", names(pals()), inline = FALSE)
    })

    output$saved_pal <- renderPlot({
        save_pal_name_input()
    })

}

