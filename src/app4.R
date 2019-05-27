
## app 4   ##

# User Interface

header <- dashboardHeader(
  title = "Header", 
  
  # Incorpora Logo desde la web:
  tags$li(a(href = 'https://rladies.org/',
            img(src = "https://rladies.org/wp-content/uploads/2016/12/R-LadiesGlobal.png",
                height="30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown")
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    
    # Se incorporan íconos a cada menuItem (https://glyphsearch.com/)
    menuItem("Item 1", tabName = "item1", icon = icon("arrow-right", lib = "glyphicon"),                                         # Glyphicon
             menuSubItem("SubItem 1", tabName = "subitem1", icon = icon("arrow-circle-right", lib = "font-awesome")),            # Font Awesome
             startExpanded = TRUE),
    menuItem("Item 2", tabName = "item2", icon = icon("arrow-right", lib = "glyphicon")),
    
    # Se incorpora un slider para seleccionar años
    sliderInput(inputId = "slider",                                                                                              # ?shiny::sliderInput
                label = "", 
                min = 2010, 
                max = 2018, 
                value = 2014, 
                sep = NULL)
  )
)

body <- dashboardBody(
)

ui <- dashboardPage(header, sidebar, body)

# Server
server <- function(input, output) { }

shinyApp(ui, server)

