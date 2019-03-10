## 35 : navbar layout & sub menu item # tags$a, a(herf=)   
 

library(shiny) 
     
ui <- navbarPage(
    
    title='Demo Shiny Navbar Layout ',
    tabPanel("About Page",
             
             h4("This app uses navbar for layout")),
    
    tabPanel("Data Page", 
             tableOutput("data")),
    
    
    
    tabPanel("Widgets & Sidebar",
             sidebarLayout(
                 sidebarPanel(
                     sliderInput("b", "Select no. of BINs", min = 5, max = 20, value = 10)
                 ),
                 mainPanel(
                     plotOutput("plot")
                 ))),
    
    navbarMenu("Menu Options",
               tabPanel("Menu item A - Summary stats", "verbatimTextOutput(summary)"),
               tabPanel("Menu item B - Link to code",
                        h4(HTML(paste("Thanks for watching the video. Reference code")),
                        tags$a(href="http://www.rstudio.com/", "Click here!"),
                        h4(HTML(paste("In case you have any question", a(href="mailto:gwihyeonseo@naver.com", "email me", ".")))))
    )))

     
    ##
     
     
server <- function(input, output, session){
    
    output$data <- renderTable( {
        mtcars
    })
    output$plot <- renderPlot( {
        hist(mtcars$mpg, col = "blue", breaks = input$b)
    })
    
}
    
    ## shinyApp
    
    shinyApp(ui, server)
