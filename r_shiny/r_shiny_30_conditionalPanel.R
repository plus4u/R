## 30 : conditional Panel
 

library(shiny)
library(ggplot2)

ui <- pageWithSidebar(
    
    headerPanel("conditional Panel"),
    
    
    # sidebarLayout(
    
    sidebarPanel(
        conditionalPanel(condition = "input.tabselected==1", h4("Demo ..()")),
        conditionalPanel(condition = "input.tabselected==2",
                         selectInput("dataset", "select the desired dataset", choices = ls('package:datasets'), selected = "mtcars"),
                         radioButtons("choice", "choose an option", choices = c("Dataset"=1, "Structure"=2, "Summary"= 3)) 
        ),
        conditionalPanel(condition = "input.tabselected==3", uiOutput("varx"), uiOutput("vary"))
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("About", value = 1, helpText("conditionalPanel(condition, ...) creates ")),
                tabPanel("Data", value = 2, conditionalPanel(condition = "input.choice==1", verbatimTextOutput("dataset")),conditionalPanel(condition = "input.choice==2", verbatimTextOutput("structure")),conditionalPanel(condition = "input.choice==3", verbatimTextOutput("summary")) ),
                tabPanel("Plot", value = 3, plotOutput("plot")),
                id = "tabselected" )
        
    )
    )


##


server <- function(input, output, session){
  
    var <- reactive({
        switch(input$dataset,
               "iris" = names(iris),
               "mtcars" = names(mtcars),
               "trees" = names(trees)
        )
    })
    
    output$summary <- renderPrint({
        summary(mtcars)
    }) 
    output$structure <- renderPrint({
        str(mtcars)
    }) 
    output$dataset  <- renderTable({
              mtcars
    })
    
    ##
    output$varx <- renderUI({
        selectInput("variablex", "Select the First (X) variable", choices = var())
    })
    
    output$vary <- renderUI({
        selectInput("variabley", "Select the Second (Y) variable", choices = var())
    })
    
    # renderPlot is used to plot the ggplot and the plot output will be stored in the output variable p which could be used in ui.r to display the plot
    output$plot <- renderPlot({
        
    #    attach(get(input$dataset))
        
        plot(x= get(input$variablex), y= get(input$variabley), xlab=input$variablex, ylab=input$variabley)
        })
    
}
    

## shinyApp

shinyApp(ui, server)

 