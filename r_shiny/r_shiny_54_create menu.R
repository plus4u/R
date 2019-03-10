## 54 : create menu items in sidebar (2) 

# install.packages("shinydashboard")

library(shiny) 
library(shinydashboard)
 
  
ui = fluidPage(
    dashboardPage(
        dashboardHeader(title = "Demo shinydashboard package - This is header", titleWidth = 500), # disable = T
        
        # Add a title using the title argument
       # dashboardSidebar("This is sidebar and ~", collapsed = TRUE ), # disable = TRUE
        dashboardSidebar(
            sidebarMenu(
                menuItem(text = "About", tabName = "about", icon = icon("clipboard")),
                menuItem(text = "Data", tabName = "data", icon = icon("database"), badgeLabel = "new", badgeColor = "green"),
                menuItem(text = "Link to code files", href = "https://google.com", icon = icon("code"))
            )
        ),
       
        dashboardBody("This is a dash body"))
       )

# 

server = function(input, output, session){
 
     
} 
        
  
 ## shinyApp
    
shinyApp(ui, server)
 