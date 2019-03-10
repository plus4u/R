## 57 : add box to dashboard body | box() (5) 

library(shiny) 
library(shinydashboard)
library(plotly) 
library(datasets)
 
  
ui = dashboardPage(
        dashboardHeader(title = "Demo shinydashboard package - add box to dashboard body | box() (5)    ", titleWidth = 800), # disable = T
 
        dashboardSidebar(
            sidebarMenu(
                menuItem("box", tabName = "box", icon = icon("check"))
                )),
        
        dashboardBody(
            tabItems(
                
                tabItem(tabName = "box",
                        
                        # first row
                        fluidRow(box(title = "Box with a plot", plotlyOutput("plot1", height = 250)),
                                 box(title = "Box with a plot", plotlyOutput("plot2", height = 250)),
                        
                        # second Row
                        fluidRow(box(title = "Box with data table", tableOutput("data"), width = 8),
                                 box(title = "Box with input widget", uiOutput("inputwidget"), width = 4))
                                 )
        )) ))
 
# 

server = function(input, output, session){

    output$plot1 <- renderPlotly( {
        plot_ly(data=mtcars,
                x=~wt, y=~mpg, type = 'scatter', mode="markers")
    })
    
    
    ## ploly Histogram
    
    output$plot2 <- renderPlotly( {
        plot_ly(data=mtcars,
                x=~mpg, type = 'histogram')
    })
    
    ## input widget
    
    output$inputwidget <- renderUI( {
        selectInput(inputId = "in","Select a variable", choices = names(mtcars))
               
    })
    
    output$data <- renderTable( {
        head(mtcars)
    })
    
    
} 
        
  
 ## shinyApp
    
shinyApp(ui, server)
 