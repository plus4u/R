## 16 : choose dataset and # of observation 


library(shiny)

ui <- fluidPage(

# shinyUI(fluidPage(   
    
    # Application title.
    titlePanel("Demonstration of submitButton()"),
    
    sidebarPanel(
        # selectInput widget for the selection of dataset
        selectInput("dataset", "Choose a dataset:", 
                    choices = c("iris", "pressure", "mtcars")),
        
        # numericInput for selection of the number of observation that user wants to view
        numericInput("obs", "Number of observations:", 6),
        # submitButton to create dependency of the reactivity on the event of pressing of the submitbutton. 
        submitButton("Update!"),
        p("In this example, changing the user input (datset or number of observation) will not reflect in the output until the Update button is clicked"),
        p("submitButton is used to control the reactiveness of the change in the user input")
    ),
    mainPanel(
        # just a header for the heading
        h4(textOutput("dataname")),
        # display the structure of the selected dataset is dependent of the submit button
        verbatimTextOutput("structure"),
        
        # just a header for the heading
        h4(textOutput("observation")),
        # display the observations of the selected dataset is dependent of the submit button
        tableOutput("view")
    )
) 

##
 
server <- function(input, output, session){
    

    # to print the heading in the main panel for structure of the data set. Dependency on the update button.
    output$dataname <- renderText({
        paste("Structure of the dataset", input$dataset)
        
    })
    
    # to print the heading in the main panel for observation of the data set. Dependency on the update button.
    output$observation <- renderText({
        paste("First", input$obs, "observations of the dataset", input$dataset)
        
    })
    
    # Return  the dataset structure. We have used the str() function to get the structure of the dataset. # this reactive function would show its effect only after the update button is pressed
    
    output$structure <- renderPrint({
        str(get(input$dataset)) 
    })
    
    # for dataset observations -  "n" observations as defined by user's input. The value of number of observation will come from the input$obs
    output$view <- renderTable({
        head(get(input$dataset), n = input$obs)
        
    })
}
    

## shinyApp

shinyApp(ui, server)
