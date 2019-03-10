## 58 : add style to box in dashboard body (6) 

install.packages("plotly")

library(shiny) 
library(shinydashboard)
library(plotly) 
library(datasets)
 
  
ui = dashboardPage(
        dashboardHeader(title = "Demo add style to box in dashboard body (6) ", titleWidth = 800), # disable = T
 
        dashboardSidebar(
            sidebarMenu(
                menuItem("box", tabName = "box", icon = icon("check"))
                )),
        
        dashboardBody(
            tabItems(
                
                tabItem(tabName = "box",
                        # first row
                        fluidRow(box(title = "Box with a plot", plotlyOutput("plot1", height = 250)),
                                 
                                 box(title = "Box with a plot", plotlyOutput("plot2", height = 250),
                                     status = "danger", solidHeader = T, collapsible = T)),
                                 
                                 # Valid status options as follows
                                 # https://rstudio.github.io/shinydashboard/appearance.html#statuses-and-colors
                                 
                        # second Row
                        fluidRow(box(title = "Box with data table", tableOutput("data"), width = 6,
                                     solidHeader = T, status = "success", background = "red"),
                                 
                                 box(title = "Box with input widget", uiOutput("inputwidget"), width = 2,
                                                                               background = "black", solidHeader = T))
                                 )
        )) )
 
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
 