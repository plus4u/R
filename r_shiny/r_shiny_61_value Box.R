## 61 : value box in dashboard | valueBox()(9) 

library(shiny) 
library(shinydashboard) 
  
ui = dashboardPage(
        dashboardHeader(title = "Demo value box in dashboard | valueBox()(9) )", titleWidth = 600), # disable = T
 
        dashboardSidebar(
            sidebarMenu(
                menuItem("valueBox", tabName = "VB")
                )),
        
        dashboardBody(
            
            tabItems(
                
                tabItem(tabName = "VB",
                        # first row
                        fluidRow(
                            valueBoxOutput("min_", width = 3), valueBoxOutput("max_", width = 3), valueBoxOutput("sd_", width = 3), valueBoxOutput("mean_", width = 3)),
                        fluidRow(valueBoxOutput("median_", width = 6), valueBoxOutput("orders_", width = 3), valueBoxOutput("approved_", width = 3) ),
                        
                        fluidRow(valueBoxOutput("inline"), tags$style("#inline {height:75px; line-height:75px; padding-top:Opx; width:400px;}"))
                       
                        
            ))) )
                                 
        
# 
# generate 50 random numbers

s = sample(100:500, 50)

server = function(input, output, session){

     output$min_ <- renderValueBox ( {
         valueBox(value = min(s), subtitle = "minimum value in dataset")
     })
     
     output$max_ <- renderValueBox ( {
         valueBox(value = max(s), subtitle = "Maximum value in dataset",color = 'yellow')
     })
     
     output$sd_ <- renderValueBox ( {
         valueBox(value = round(sd(s),2), subtitle = "Standard Deviation", icon('arrow-up')) 
     })
     
     output$mean_ <- renderValueBox ( {
         valueBox(value = mean(s), subtitle = "Mean of dataset values", icon= icon('angle-double-right'), color = 'purple') 
     })
     
     output$median_ <- renderValueBox ( {
         valueBox(value = median(s), subtitle = tags$i("Median of dataset values"), icon= icon('angle-double-right')) 
     })  
     
     output$orders_ <- renderValueBox ( {
         valueBox(value = "20", subtitle = tags$i("New Ordes"), icon= icon('angle-double-right')) 
     })  
     
     output$approved_ <- renderValueBox ( {
         valueBox(value = "80 %", subtitle = tags$i("Proposition"), icon= icon('angle-double-right')) 
     })   
}
        
  
 ## shinyApp
    
shinyApp(ui, server)
 