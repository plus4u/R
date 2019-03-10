## 55 : add tabpages | tabItems (3) 

library(shiny) 
library(shinydashboard)
 
  
ui = dashboardPage(
        dashboardHeader(title = "Demo shinydashboard package - add tabpages | tabItems (3)  ", titleWidth = 700), # disable = T
        
       # dashboardSidebar("This is sidebar and ~", collapsed = TRUE ), # disable = TRUE
       
        dashboardSidebar(
            sidebarMenu(
                menuItem(text = "About", tabName = "about"), # icon = ("clipboard")
                menuItem("Data", tabName = "data", icon = icon("database")), # badgeLabel = "new", badgeColor = "green"),
                menuItem("myGithub", href = "https://github.com/aagarw30/R-Shinyapp-Tutorial") # icon = icon("code")
                # https://fontawesome.com/icons?d=gallery
              
            )
        ),
       dashboardBody(
           tabItems(
               tabItem(tabName = "about", p("This example app demonstrates basic layout of "), p("We will be using mtcars dataset for the sake of demo")),
               tabItem(tabName = "data", dataTableOutput("mydatatable"))
           )
       )
            
        )
   
# 

server = function(input, output, session){
 output$mydatatable <- renderDataTable ({ 
     mtcars
     })
     
} 
        
  
 ## shinyApp
    
shinyApp(ui, server)
 