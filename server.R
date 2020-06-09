results<-read_csv('https://www.dropbox.com/s/d0pgi1que6j4xo4/results.csv?raw=1')

races<-read_csv('https://www.dropbox.com/s/57e98c1ere5iqbj/races.csv?raw=1')

races$date<-as.Date(races$date,"%Y-%m-%d")

races$name<-gsub(" Grand Prix","",races$name)

circuits<-read_csv('https://www.dropbox.com/s/a27tb8napuaq232/circuits.csv?raw=1')

races<-left_join(races %>% select(-url), circuits %>% select(-name) %>% select(-url), by='circuitId')

results_2 <- left_join(
    results %>% dplyr::select(-time, -fastestLapTime), 
    races %>% dplyr::select(-time), 
    by='raceId')

drivers<-read_csv('https://www.dropbox.com/s/fp12tntuydbyg69/drivers.csv?raw=1')

driversStandings<-read_csv('https://www.dropbox.com/s/dlnh75jh6ecu4ds/driver_standings.csv?raw=1')

results_3 <- results_2 %>% 
    left_join(
        drivers %>% dplyr::select(-url, -number), 
        by = 'driverId') %>%
    left_join(
        driversStandings %>% 
            dplyr::rename(standingsPosition = position, standingPoints = points) %>% 
            dplyr::select(-positionText), 
        by = c('driverId', 'raceId'))


constructors<-read_csv('https://www.dropbox.com/s/utbrsl7jyenrudt/constructors.csv?raw=1')

constructorStandings<-read_csv('https://www.dropbox.com/s/ex0bn2wcn12a3id/constructor_standings.csv?raw=1')

constructorResults<-read_csv('https://www.dropbox.com/s/0cbikmzli0mwv84/constructor_results.csv?raw=1')

constructorResults<-left_join(
    constructorResults, 
    races %>% rename(name_races = name), 
    by='raceId')

constructorResults <- left_join(
    constructorResults,
    constructors %>% select(-url) %>% rename(name_constructor = name),
    by='constructorId')

constructorResults <- left_join(
    constructorResults, 
    constructorStandings %>% rename(point_constructor = points), 
    by=c('constructorId','raceId'))


results_3 <- results_3 %>% 
    left_join(
        constructors %>% 
            rename(nameConstructor = name) %>% 
            select(constructorId, nameConstructor, constructorRef), 
        by = 'constructorId') %>%
    mutate(driver_full_name = paste(forename, surname, sep = " "))


drivers_constructors_unique <- results_3 %>% 
    select(driverId, driverRef, forename, surname, nameConstructor, year) %>%
    distinct() %>%
    mutate(name = paste(forename, surname, sep = ' '))


constructors_list <- list(drivers_constructors_unique %>% 
                              select(nameConstructor) %>% 
                              unique() %>%
                              arrange(nameConstructor))

names(constructors_list) <- "Constructor"


drivers_list <- list(drivers_constructors_unique %>%
                         filter(year == 2019) %>%
                         select(forename, surname) %>%
                         mutate(name = paste(forename, surname, sep = " ")) %>%
                         select(name) %>% 
                         unique() %>%
                         arrange(name))

names(drivers_list) <- "Driver"


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
