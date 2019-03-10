## 65 : add icons to sidebar menu items and styling(13)  

install.packages("shinydashboard")

library(shiny)

library(shinydashboard) 
  
ui = dashboardPage(   # fluidPage(
        dashboardHeader(title = "add icons to sidebar menu items and styling(13)  ", titleWidth = 600), # disable = T
 
        dashboardSidebar("This is sidebar",
                         sidebarMenu(
                             menuItem("Dash Board", tabName = "dashboard",
                                      icon = icon("tachometer")),
                             
                             menuItem("Dash Board2", tabName = "dashboard2",
                                      icon = icon("tachometer", class = "fa-lg")),
                             
                             menuItem("Dash Board3", tabName = "dashboard3",icon = icon("fa-rotate-90")),
                             
                             menuItem("Setting1", tabName = "setting1",
                                      icon = icon("cog", class = "fa-spin")),
                             
                             tags$style(".fa-spin {text-align:center; }"),
                             
                             menuItem("Setting2", tabName = "setting2",
                                      icon = icon("cog", class = "fa-pulse")),
                             
                             menuItem("Setting3", tabName = "setting3",
                                      icon = icon("cog", class = "fa-border")),
                             
                             menuItem("HourGlass", tabName = "hourglass",
                                      icon = icon("hourglass", lib = "glyphicon")),
                             
                             menuItem("BoxStatus", tabName = "boxstatus", 
                                      icon = icon("square")),
                             
                             menuItem("BoxColor", tabName = "boxcolor",
                                      icon = icon("square", class = "mystyle")),
                             
                             tags$style(".mystyle {color:yellow;}")
                                                      ))
        ,
    
    dashboardBody()
    ) 

# 
 

server = function(input, output, session){

}
        
  
 ## shinyApp
    
shinyApp(ui, server)
 