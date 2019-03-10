## 63 : add color to box header | status argument(11)  
# danger

library(shiny) 
library(shinydashboard) 
  
ui = dashboardPage(skin = "purple",
        dashboardHeader(title = "add color to box header | status argument(11) ", titleWidth = 600), # disable = T
 
        dashboardSidebar("This is sidebar",
                         
                         menuItem("valueBox", tabName = "boxstatus")),
             
        dashboardBody(
            tabItems(
                tabItem(tabName = "boxstatus",
                        fluidRow(box (title = "danger (red)", status = "danger", width = 2, solidHeader = T),
                                 box(title = "primary (blue)", status = "primary", width = 2, solidHeader = T),
                                 box(title = "success (green)", status = "success", width = 2,solidHeader = T),
                                 box(title = "info (blue)", status = "info", width = 2,solidHeader = T), 
                                 box(title = "warning (orange)", status = "warning", width = 2,solidHeader = T)  
                        ),
                        
                        # status decide color
                        fluidRow(box (title = "test", status = "danger", width = 2, solidHeader = T),
                                 box(title = "primary (blue)", status = "info", width = 2, solidHeader = T), 
                                 box(title = "warning (orange)", status = "warning", width = 2,solidHeader = T)  
                        )
                        
                        
                        )) ))
            
    # 
 

server = function(input, output, session){

    
}
        
  
 ## shinyApp
    
shinyApp(ui, server)
 