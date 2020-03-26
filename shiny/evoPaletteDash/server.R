
function(input, output) {


    # generate palettes either
    pals <- eventReactive(input$evolve_button, {
        if(is.null(input$selected_parents_ui)) gallery$random <- TRUE
        if(gallery$random) {
            gallery$random <- FALSE
            gallery$current_palette <- random_palette(input$n_cols, input$n_palettes)
        }else{
            gallery$current_palette <- evolve(gallery$current_palette[as.numeric(input$selected_parents_ui)], input$n_palettes, mutation_rate = 0.05, variation = input$variation)
        }
        gallery$current_palette
    })

    palettes <- reactive({
        show_palettes <- map(pals(), ~show_palette(.x))
        wrap_plots(show_palettes)
    })

    palette_examples <- reactive({
        plot_palette(pals())
    })

    palette_examples_colour <- reactive({
        plot_palette(pals(), aes = "colour")
    })

    observeEvent(input$save_palette, {
        shinyalert("Palette name", "Select a name for your palette...", type = "input")
    })

    save_pal_name_input <- eventReactive(input$shinyalert, {
        print(input$shinyalert)
        gallery$palette_box[[input$shinyalert]] <- gallery$current_palette[as.numeric(input$selected_parents_ui)][[1]]
        show_palette(gallery$palette_box[[input$shinyalert]])
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
        checkboxGroupInput("selected_parents_ui", "Select parents", 1:input$n_palettes, inline = FALSE)
    })

    output$saved_pal <- renderPlot({
        save_pal_name_input()
    })

}

