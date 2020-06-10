library(tidyverse)
library(shiny)
library(shinythemes)

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





shinyUI(fluidPage(
    navbarPage("Reproducible Research (2020)",
        tabPanel("Home", fluid = TRUE, icon = icon("globe-americas"),
            sidebarLayout(
                sidebarPanel(
                    titlePanel("Reproducible Research (2020) Final Project"),
                ),
                
                mainPanel(
                    HTML(
                        paste(
                            h1("Reproducible Research (2020) - Final Project"),'<br/>',
                            h2("Bartlomiej Wieczorek (345498)"),'<br/>',
                            a("GitHub repo of this project", href = "https://github.com/bwieczorek4/ReprodResearchF1", target = "_blank")
                        )
                    )
                )
                            
            )
        ),
        
# Second Tab  

        tabPanel("F1 - season analysis", fluid = TRUE, icon = icon("globe-americas"),
            sidebarLayout(
                sidebarPanel(
                    titlePanel("Reproducible Research (2020) Final Project"),
                    numericInput("YearSelect", 
                                "Select a year : ", 
                                value = 2019, min = 1950, max = 2019),
                    
                    selectInput("DriverSelect", 
                                "Select driver(s) : ",
                                choices = drivers_list$Driver,
                                multiple = TRUE),
                                
                    sliderInput("RoundSelect", "Round:",
                                min = 1, max = NULL,
                                value = c(1,21), step = 1),
                                
                        
                    actionButton("goButton", "Go!"),
                ), 
                            
                mainPanel(
                    plotOutput("seasonPlot"),
                )
                            
            ),
        )
    )
))
