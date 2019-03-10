
library(shiny)
library(shinydashboard)



sidebar <- dashboardSidebar(
  tags$head(
    tags$script(
      HTML(
        "
        $(document).ready(function(){
        // Bind classes to menu items, easiet to fill in manually
        var ids = ['subItemOne','subItemTwo','subItemThree','subItemFour'];
        for(i=0; i<ids.length; i++){
        $('a[data-value='+ids[i]+']').addClass('my_subitem_class');
        }
        
        // Register click handeler
        $('.my_subitem_class').on('click',function(){
        // Unactive menuSubItems
        $('.my_subitem_class').parent().removeClass('active');
        })
        })
        "
      )
    )
    ),
  width = 290,
  
  sidebarMenu(
    menuItem('Menu One', tabName = 'menuOne', icon = icon('line-chart'),
             collapsible = 
               menuSubItem('Sub-Item One', tabName = 'subItemOne'),
             menuSubItem('Sub-Item Two', tabName = 'subItemTwo')
    )
  ),
  
  sidebarMenu(
    menuItem('Menu Two', tabName = 'menuTwo', icon = icon('users'), 
             collapsible = 
               menuSubItem('Sub-Item Three', tabName = 'subItemThree'),
             menuSubItem('Sub-Item Four', tabName = 'subItemFour')
    )
  )
  
    )
# Body #############################
body <- dashboardBody(
  
  tabItems(
    tabItem(tabName = 'subItemOne',
            h2('Selected Sub-Item One')
    ),
    tabItem(tabName = 'subItemTwo',
            h2('Selected Sub-Item Two')
    ),
    tabItem(tabName = 'subItemThree',
            h2('Selected Sub-Item Three')
    ),
    tabItem(tabName = 'subItemFour',
            h2('Selected Sub-Item Four')
    )
  )
)
# UI #############################
ui <- dashboardPage(
  dashboardHeader(title = 'Test', titleWidth = 290),
  sidebar,
  body
)
# Server #############################
server <- function(input, output){
  
}

shinyApp(ui, server)
 