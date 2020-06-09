#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
        
    navbarPage("Reproducible Research (2020)",
        tabPanel("Home", fluid = TRUE, icon = icon("globe-americas"),
            sidebarLayout(
                sidebarPanel(
                    titlePanel("Reproducible Research (2020) Final Project")
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
        )
    )
))

