## App2 : Create an app that would allow a user to generate a scatterplot in the mainPanel after choosing two input variables from two dropdown menus in sidebarPanel.
##        Also, add a box below the scatterplot that prints the correlation between the two variables. 

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(data.table)
library(dplyr)

Wines <- read.csv("winequality-red.csv")

# UI
ui <- fluidPage(
  
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
  
  sidebarLayout(
    
    # Input(s)
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Vertical Axis:",
                  choices = c("fixed_acidity",
                              "volatile_acidity",
                              "citric_acid",
                              "residual_sugar",
                              "chlorides",
                              "free_sulfur_dioxide",
                              "total_sulfur_dioxide",
                              "density",
                              "pH",
                              "sulphates",
                              "alcohol",
                              "quality"
                            ), 
                  selected = "alcohol"),
      
      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "Horizontal Axis:",
                  choices = c("fixed_acidity",
                              "volatile_acidity",
                              "citric_acid",
                              "residual_sugar",
                              "chlorides",
                              "free_sulfur_dioxide",
                              "total_sulfur_dioxide",
                              "density",
                              "pH",
                              "sulphates",
                              "alcohol",
                              "quality"
                  ), 
                  selected = "chlorides")
    ),
    
    # Outputs
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      textOutput(outputId = "correlation")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = Wines, aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  
  # Create text output stating the correlation between the two plotted variables
  output$correlation <- renderText({
    r <- round(cor(Wines[, input$x], Wines[, input$y], use = "pairwise"), 3)
    r_interpretion <- case_when(r == -1 ~ "Perfect negtative linear relationship",
                                r > -0.75 && r< -0.50 ~ "strong negtative linear relationship",
                                r > -0.50 && r< -0.30 ~ "moderate downhill linear relationship",
                                r== 0 ~ "no linear relationship",
                                r > 0.30 && r< 0.50 ~ "moderate positive linear relationship",
                                r > 0.50 && r< 0.70 ~ "strong positive linear relationship",
                                r == 1 ~ "perfect positive linear relationship",
                                TRUE ~ "weak/no linear relationship")
    paste0("Correlation = ", r,".The correlation cofficient indicates a ",r_interpretion," between ",input$x ," and ",input$y, ". Note: Correlation coefficients rounded to three decimal places.")
                                   })

}
# Create a Shiny app object
shinyApp(ui = ui, server = server)
