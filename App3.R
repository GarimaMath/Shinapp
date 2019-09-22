## App3 : Create an interactive app that would allow a user to generate a scatterplot 
##        by choosing two input variables, as above.  Additionally, allow a user to hover 
##        over data points in the scatterplot and see information about that point underneath the scatterplot.

# Loading packages
library(shiny)
library(tidyverse)
library(DT)

# Load data
Wines <- read.csv("winequality-red.csv")

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  # Title of the App
  title = "Red Wine Quality",
  
  br(),
  # Set background colour
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
      # Select variable for y-axis
      selectInput(inputId = "y", label = "Vertical Axis:",
                  choices = c("fixed acidity",
                              "volatile acidity",
                              "citric acid",
                              "residual sugar",
                              "chlorides",
                              "free sulfur dioxide",
                              "total sulfur dioxide",
                              "density",
                              "pH",
                              "sulphates",
                              "alcohol",
                              "quality"
                  ), 
                  selected = "alcohol"),
      # Select variable for x-axis
      selectInput(inputId = "x", label = "Horizontal Axis:",
                  choices = c("fixed acidity",
                              "volatile acidity",
                              "citric acid",
                              "residual sugar",
                              "chlorides",
                              "free sulfur dioxide",
                              "total sulfur dioxide",
                              "density",
                              "pH",
                              "sulphates",
                              "alcohol",
                              "quality"
                  ), 
                  selected = "quality")
    ),
    
    # Output:
    mainPanel(
      # Shows scatterplot with brushing capability
      plotOutput(outputId = "scatterplot", hover = "plot_hover"),
      # Show data table
      dataTableOutput(outputId = "Winetable"),
      br()
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = Wines, aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  
  # Create data table
  output$Winetable <- DT::renderDataTable({
    nearPoints(Wines, input$plot_hover) %>% 
      select(fixed_acidity:quality)
  })
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
