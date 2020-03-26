
function(input, output) {


    # generate palettes either
    pals <- eventReactive(input$evolve_button, {
        if(is.null(input$selected_parents_ui)) random <<- TRUE
        if(random) {
            random <<- FALSE
            current_pal <<- random_palette(input$n_cols, input$n_palettes)
        }else{
            current_pal <<- evolve(current_pal[as.numeric(input$selected_parents_ui)], input$n_palettes, mutation_rate = 0.05, variation = input$variation)
        }
        current_pal
    })

    palettes <- reactive({
        show_palettes <- map(pals(), ~show_palette(.x))
        wrap_plots(show_palettes)
    })

    palette_examples <- reactive({
        wrap_plots(
            map(pals(), ~plot_palette(.x))
        )
    })

    palette_examples_colour <- reactive({
        wrap_plots(
            map(pals(), ~plot_palette(.x, aes = "colour"))
        )
    })

    observeEvent(input$save_palette, {
        shinyalert("Palette name", "Select a name for your palette...", type = "input")
    })

    save_pal_name_input <- eventReactive(input$shinyalert, {
        gallery$palette_box[[input$shinyalert]] <- current_pal[as.numeric(input$selected_parents_ui)][[1]]
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

