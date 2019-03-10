## 21 : upload file


library(shiny)

ui <- fluidPage(

# shinyUI(fluidPage(   
    
    titlePanel(h4("upload files & summary data")),
    sidebarLayout(
        
        sidebarPanel(
            fileInput("file","upload the file"),
            helpText("default max. file size is 5 MB"),
            tags$hr(),
            
            checkboxInput(inputId = "header",label = "Header", value = FALSE),
            checkboxInput("stringsAsFactors","stringsAsFactors", FALSE),
            br(),
            radioButtons(inputId = "sep",label="Separator",choices = c(Comma=',',Semicoln=';',Tab='\t',Space=''),selected = ',')),
        
        mainPanel(
            uiOutput("tb")
            
    )
)) 

##

server <- function(input, output, session){
    
    data <- reactive({
        file1 <- input$file
        if(is.null(file1)){return()}
        read.table(file=file1$datapath,sep=input$sep,header = input$header,stringsAsFactors = input$stringsAsFactors)
    })
    
    output$filedf <- renderTable({
        if(is.null(data())){return()}
        input$file
    }) 
    
    output$sum <- renderTable({
        if(is.null(data())){return()}
        summary(data())
    })
    
    output$table <- renderTable({
        if(is.null(data())) {return()}
        data()
    })
    
    output$tb <- renderUI ({
        if(is.null(data())) {return()}
        # h5("Powered by", tags$img(src='RStudio-Ball.png', height=200, width=200))
        else
            x <- data()
            tabsetPanel(
                tabPanel("About file",tableOutput("filedf")),
                tabPanel("Data",tableOutput("table")),
                tabPanel("Summary",tableOutput("sum"))
            )
        })
}
        

## shinyApp

shinyApp(ui, server)


x

str(x)
data()
data
