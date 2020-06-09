shinyServer(function(session,input, output) {
    observe({
        val <- results_3 %>%
            filter(year == input$YearSelect) %>%
            group_by(year) %>%
            summarize(MaxRound = max(round))
        
        # Control the value, min, max, and step.
        # Step size is 2 when input value is even; 1 when value is odd.
        updateSliderInput(session, "RoundSelect", value = input$RoundSelect,
                          min = 1, max = val$MaxRound, step = 1)
    }
    )
    
    observe({
        driver_list_updated <- list(drivers_constructors_unique %>%
                                        filter(year == input$YearSelect) %>%
                                        select(forename, surname) %>%
                                        mutate(name = paste(forename, surname, sep = " ")) %>%
                                        select(name) %>% 
                                        unique() %>%
                                        arrange(name))
        names(driver_list_updated) <- "Driver"
        
        # Control the value, min, max, and step.
        # Step size is 2 when input value is even; 1 when value is odd.
        updateSelectInput(session, "DriverSelect", choices = driver_list_updated$Driver)
    }
    )
    
    
    output$seasonPlot <- renderPlot({
        input$goButton
        plot_title <- isolate(paste("Points accumulated during Season", input$YearSelect, sep = ' '))
        results_4 <- isolate(results_3 %>% 
                                 mutate(name = paste(forename, surname, sep = " ")) %>%
                                 dplyr::filter(name %in% input$DriverSelect)%>% 
                                 filter(year== input$YearSelect, round >= input$RoundSelect[1], round <= input$RoundSelect[2]) %>% 
                                 select(round,name,points, position, nameConstructor) %>% mutate(winRace = ifelse(position==1,'yes','no')) %>%
                                 group_by(name, nameConstructor) %>% 
                                 mutate(current = cumsum(points))
        )
        ggplot(data = results_4, aes(x=round,y=current,color=name)) +
            geom_line(size=2,alpha=.5) + 
            geom_point(aes(shape=winRace),color='black',size=2) +
            theme_bw() + 
            labs(title=plot_title,
                 subtitle = 'triangle indicates a race win, circle otherwise', 
                 x = 'Round', 
                 y = 'Points accumulated') + 
            theme(legend.position='right',legend.direction='vertical',
                  legend.text = element_text(size = 10))
    })
})
