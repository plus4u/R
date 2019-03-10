## 

library(shiny) 
library(shinydashboard) 
  
## 

title <- tags$a(href='https://plus4u.github.io/innovation/',
                icon("cloud"),
                'reference knowledge : plus4u.io/innovation', target="_blank")

# <i class="fab fa-github"></i>
# icon("diamond" , atias, church, cloud, book-reader, book)


## ui code starts here

ui <- dashboardPage(
    
        dashboardHeader(title = title, titleWidth = 600,
                        
                        tags$li(class="dropdown",tags$a(href="http://127.0.0.1:4765/", icon("youtube"), "My naver blog", target="_blank")),
                        
                        tags$li(class="dropdown",tags$a(href="https://blog.naver.com/gwihyeonseo", icon("youtube"), "My naver blog", target="_blank")),
                        tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/gwi-hyeon-seo-776600168/" ,icon("linkedin"), "My Profile", target="_blank")),
                        tags$li(class="dropdown",tags$a(href="https://github.com/plus4u", icon("github"), "Source Code", target="_blank"))
                        
                        
        ),
        
        
        ## 
        
        dashboardSidebar( width = 350,
            
            h3("Contents"),
                         sidebarMenu(
                             

                             ### start : 1. insert fileInput in menuItem  
                        
                             menuItem("Exploratory", tabName = "exploratory", icon = icon("bar-chart-o"),
                                      
                                      
                                      fileInput("file","upload the file"),
                                      helpText("default max. file size is 5 MB"),
                                      tags$hr(),
                                      
                                      checkboxInput(inputId = "header",label = "Header", value = FALSE),
                                      checkboxInput("stringsAsFactors","stringsAsFactors", FALSE),
                                      br(),
                                      radioButtons(inputId = "sep",label="Separator",choices = c(Comma=',',Semicoln=';',Tab='\t',Space=''),selected = ','),
                                                
                                      # Input directly under menuItem
                                      ### 2. insert selectInput in menuItem                                                                      
                                        
                                      selectInput("inputTest", "Input Test",
                                                  choices = c("a", "b", "c", "d"), multiple=TRUE, selectize=TRUE,
                                                  width = '98%'),
                                      
                                      # Input inside of menuSubItem
                                      menuSubItem(icon = NULL,
                                                  sliderInput("inputTest2", "Input test 2", min=0, max=10, value=5,
                                                              width = '95%')
                                      )
                             ),   
                             
                             
                             
    ### end
                           
                             menuItem("Dashboard", tabName = "dashboard",
                                      icon = icon("tachometer", class = "fa-lg")),
                             
                             menuItem("R", tabName = "r",
                                      icon = icon("registered", class = "fa-spin")),
                             
                             tags$style(".fa-spin {text-align:center; }"),
                             
                             menuItem("Python", tabName = "python",
                                      icon("python", class = "fa-spin")),
                             
                             tags$style(".fa-spin {text-align:center; }"),
                             
                             
                             menuItem("Setting", tabName = "Setting",
                                      icon = icon("cog", class = "fa-spin")),
                             
                             tags$style(".fa-spin {text-align:center; }"),
                             
                             
                             menuItem("BoxColor", tabName = "boxcolor",
                                      icon = icon("square", class = "mystyle")),
                             
                             tags$style(".mystyle {color:yellow;}")
                         )) ,
        
        
        ##
        
        dashboardBody( 
            
            tabItems(
                
                tabItem(tabName = "exploratory",  
                        fluidRow(valueBoxOutput("exploratory_v", width = 12) 
                         #       uiOutput("tb")
                        )),
                
                tabItem(tabName = "dashboard",
                        

                        # first row

                       # fluidRow(
                        #  valueBoxOutput("exploratory_", width = 12)),

                        
                        fluidRow(
                            valueBoxOutput("min_", width = 3), valueBoxOutput("max_", width = 3), valueBoxOutput("sd_", width = 3), valueBoxOutput("mean_", width = 3)),
                        
                        fluidRow(valueBoxOutput("median_", width = 6), valueBoxOutput("orders_", width = 3), valueBoxOutput("approved_", width = 3) ),
                        
                        fluidRow(valueBoxOutput("inline"), tags$style("#inline {height:75px; line-height:75px; padding-top:Opx; width:400px;}"))
                        
                        
                )),
            
            
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "custom1.css")
            )
        ))


# generate 50 random numbers

s = sample(100:500, 50)

server = function(input, output, session){
    
    ### start
    output$tb <- renderValueBox ( {
        valueBox(value = min(s), subtitle = "minimum value in dataset")
    })
    ### end
    
    
    output$exploratory_v <- renderValueBox ( {
      valueBox(value = min(s), subtitle = "exploratory  value in dataset", color = 'blue')
    })
    
    
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
    
    
    ### reactive 
    
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

    
    ### end 
    

shinyApp(ui, server)

# shinyApp(ui, server = function(input, output, session) {})


