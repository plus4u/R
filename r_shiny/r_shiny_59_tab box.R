## 59 : add tab box in dashboard | tabBox()(7) 

library(shiny) 
library(shinydashboard)
library(plotly) 
library(datasets)
 
  
ui = dashboardPage(
        dashboardHeader(title = "Demo add tab box in dashboard | tabBox()(7)", titleWidth = 600), # disable = T
 
        dashboardSidebar(
            sidebarMenu(
                menuItem("tabbox", tabName = "charts", icon = icon("check"))
                )),
        
        dashboardBody(
            tabItems(
                
                tabItem(tabName = "charts",
                        # first row
                        fluidRow(
                            tabBox(id = "tabchart1", 
                                   tabPanel("Tab1", plotlyOutput("plot1")),
                                   tabPanel("Tab2", plotlyOutput("plot2")),
                                   tabPanel("Tab3", plotlyOutput("plot3"))),
                            
                           
                            tabBox(id = "tabchart2", side = "right",
                                   tabPanel("Tab1", "Tab 1 content"),
                                   tabPanel("Tab2", "Tab 2 content"),
                                   tabPanel("Tab3", "Tab 3 content"))
                        
                        ))
            )))
                                 
        
 
# 

server = function(input, output, session){

    ## ploly Scatter Plot
    
    output$plot1 <- renderPlotly( {
        plot_ly(data=mtcars,
                x=~wt, y=~mpg, type = 'scatter', mode="markers")
    })
    
    
    ## ploly Histogram
    
    output$plot2 <- renderPlotly( {
        plot_ly(data=mtcars, x=~mpg, type = 'histogram')
    })
    
    output$plot3 <- renderPlotly( {
        plot_ly(data=mtcars,
                x=~hp, y=~mpg, type = 'scatter', mode="markers")
    })
    
} 
        
  
 ## shinyApp
    
shinyApp(ui, server)
 