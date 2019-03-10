library(shiny)
library(shinydashboard)

# shinyUI(dashboardPage(skin = "black",

ui <- dashboardPage(skin = "black",
                    
                      dashboardHeader(title = h4("Tableau de bord des elections",style =      "color:navy"),
                                      titleWidth = 300
                      ),
                      dashboardSidebar(id="", 
                                       menuItem(h4(strong("Dashboard", align = "center")), tabName = "dashboard"),
                                       menuItem(h4(strong("Prediction")), tabName = "Prediction"),
                                       menuItem(h4(strong("Interpretation")), tabName = "Interpretation")),

                    
                      dashboardBody(
                        tabItems(
                          tabItem(tabName = "dashboard",h2("Analyse du comportement electoral  des citoyens tunisiens", align="center",style = "color:navy") ),
                          
                          tabItem(tabName = "Prediction", h2("Prediction du vote",    align="center",style = "color:blue")),
                          tabItem(tabName = "Interpretation", h2("Interpretation"))
                          
                        )
                      ))


server = function(input, output, session){}

shinyApp(ui, server)


