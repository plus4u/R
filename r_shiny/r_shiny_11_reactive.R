## 11: reactive  


library(shiny)

ui <- fluidPage(

# shinyUI(fluidPage(   
    
  sidebarLayout(
    
    sidebarPanel(("Select dataset and conditions"),
                 
                 selectInput("var","1. select input variables from dataset", 
                             choices = c("Sepal.Length"= 1,"Sepal.Width"= 2,"Petal.Length"= 3,"Petal.Width"= 4)),
                 
                 sliderInput("bins","2. select the number of bins",min = 1,max=100,value = 30),
                 radioButtons("color","3.select the color of the histogram", choices=c("Green","Red", "Yellow", selected="Green")
                              
                 )),
    
         mainPanel(
             tabsetPanel(type="tab",
                         tabPanel("Summary", verbatimTextOutput("sum")),
                         tabPanel("Structure", verbatimTextOutput("str")),
                         tabPanel("Data", tableOutput("data")),
                         tabPanel("Plot",
                                  plotOutput("myhist"))
        
        
              )
            )))



## server 

server <- function(input, output, session){

# shinyServer(  function(input, output) {
     
    colm <- reactive( {
        as.numeric(input$var)
        
    })
    
    
    output$sum <- renderPrint({
        summary(iris)
    }) 
    output$str <- renderPrint({
        str(iris)
    }) 
    output$data  <- renderTable({
    #    colm <- as.numeric(input$var)
        iris[colm()]
    })
    
    output$myhist <- renderPlot({
    #    colm <- as.numeric(input$var)
        hist(iris[,colm()],breaks=seq(0, max(iris[,colm() ]),l=input$bins +1),col=input$color, main="Histogram of Iris dataset", xlab=names(iris[,colm()]))
    
    })
}
    

## shinyApp

shinyApp(ui, server)
