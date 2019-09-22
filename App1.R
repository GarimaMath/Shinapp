## App1 : Create an app that allows users to choose a random number of wines 
##        and display relevant information about each of the wines in a tabular format.

library(shiny)
library(dplyr)
library(DT)
library(data.table)
library(shinyWidgets)

Wines <- read.csv("winequality-red.csv")
n_wines <- nrow(Wines)

# Define UI for application that plots features of movies
ui <- fluidPage(
  #Set Background colour
  setBackgroundColor(
    color = c("#F7FBFF", "#ff6666"),
    gradient = "linear",
    direction = "bottom"
  ),
  
  #App Heading
  headerPanel(
    h1("Red Wine Quality Analysis", 
       style = "font-family: 'Lobster', cursive;
       font-weight: 500; line-height: 1.1; 
       color: #b30000;")),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs
    sidebarPanel(
      
    
      # Numeric input for sample size
      numericInput(inputId = "nwines",
                   label = "Wine Types:",
                   value = 15,
                   min = 1, max = n_wines,
                   step = 1),
      
      # Text instructions
      HTML(paste("Note: ",
                 "Enter a value between 1 and", n_wines,"to select multiple wine types.")),
      
      helpText("Based on Sensory data, quality is assesed on a scale of 1 to 10 with 10 being the best."),
      helpText("PH describes how acidic a wine is on a scale from 0 (highly acidic) to 14 (least).")
    
    ),
    
    # Output: Show data table
    mainPanel(
      DT::dataTableOutput(outputId = "winetable")
    )
  )
  
  
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create data table
  output$winetable <- DT::renderDataTable({
    req(input$nwines)
    Wines_Table <- Wines %>%
      sample_n(input$nwines) %>%
        select(volatile_acidity:quality)
    DT::datatable(data = Wines_Table, 
                  options = list(pageLength = 20), 
                  rownames = FALSE)
  })
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
