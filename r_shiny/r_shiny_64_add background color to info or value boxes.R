## 64 : add background color to info or value boxes(12) 

library(shiny) 
library(shinydashboard) 
  
ui = dashboardPage(skin = "purple", # blue
        dashboardHeader(title = "add background color to info or value boxes(12) ", titleWidth = 600), # disable = T
 
        dashboardSidebar("This is sidebar",
                         
  
                         menuItem("boxcolor", tabName = "boxcolor")),
             
        dashboardBody(
            tabItems(
                tabItem(tabName = "boxcolor",
                        fluidRow(
                            infoBox(title = "Red", value = "008", color = "red", width = 3),
                            infoBox(title = "Yellow", value = "008", color = "yellow", width = 3),
                            infoBox(title = "Aqua", value = "008", color = "aqua", width = 3),
                            infoBox(title = "Blue", value = "008", color = "blue", width = 3)),
                         
                        fluidRow(
                            infoBox(title = "Navy", value = "009", color = "navy", width = 3, fill = T),
                            infoBox(title = "Teal", value = "009", color = "teal", width = 3,fill = T),
                            infoBox(title = "Olive", value = "009", color = "olive", width = 3, fill = T),
                            infoBox(title = "Lime", value = "009", color = "lime", width = 3, fill = T)),
                        
                        fluidRow(
                            valueBox(subtitle = "Maroon", value = "0109", color = "maroon", width = 3),
                            valueBox(subtitle = "Fuchsia", value = "0109", color = "fuchsia", width = 3),
                            valueBox(subtitle = "Light Blue", value = "009", color = "light-blue", width = 3),
                            valueBox(subtitle = "Green", value = "0109", color = "green", width = 3))
                            
                                 )))
 
)

# 
 
server = function(input, output, session){

}
        
  
 ## shinyApp
    
shinyApp(ui, server)
 