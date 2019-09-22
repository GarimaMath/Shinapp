library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(shinythemes)

#Data Preprocessing 

vad <- read.csv("Vehicle_Accident_Data.csv")

df <- tidyr::separate(data=vad,
                      col=Location,
                      into=c("Latitude", "Longitude"),
                      sep=",",
                      remove=FALSE)

df <- unite_(df, "Street", c("Street.Prefix","Street.Name","Street.Suffix"),sep =" ")
df <- unite_(df, "IntersectingStreet", c("Intersecting.Street.Prefix","Intersecting.Street.Name","Intersecting.Street.Suffix"),sep =" ")
 
df$Latitude <- stringr::str_replace_all(df$Latitude, "[(]", "")
df$Longitude <- stringr::str_replace_all(df$Longitude, "[)]", "")


df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)
df$DateOfAccident <- as.Date(df$Crash.Date.Time,format="%m/%d/%Y")
items <- as.character(df[[10]])

head(df)

# UI
ui <- fluidPage(
  
  theme = shinytheme("united"),
  
  # Title of the App
  titlePanel("Vehicle Accident Data"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
  
      selectInput(inputId = "Fatality", 
              label = "Type of Fatality:",
              choices = items,
              selected = "TRUE",
              multiple = TRUE),
  
      selectInput(inputId = "HAR", 
              label = "Hit and Run Case :",
              choices = items,
              selected = "FALSE",
              multiple = TRUE),
  
      dateRangeInput("daterange", "Date Range:",
                 start  = min(df$DateOfAccident),
                 end    = max(df$DateOfAccident),
                 min    = min(df$DateOfAccident),
                 max    = max(df$DateOfAccident),
                 format = "mm/dd/yyyy",
                 separator = " to ")
          ),
    #Outputs
    mainPanel(
      leafletOutput("mymap",width = "100%", height = 500),
      br(),
      # Show data table
      dataTableOutput(outputId = "vadtable")
             )
    )
)



# Server
server <- function(input, output) {
  
  data <- reactive({
    x <- df %>%
      filter(Fatality == input$Fatality)%>%
      filter(Hit.And.Run == input$HAR)%>%
      filter(between(DateOfAccident, input$daterange[1], input$daterange[2]))
      
  })
  
  output$mymap <- renderLeaflet({
    
    df <- data()
    
    m <- leaflet(data = df) %>%
      addTiles() %>%
      addMarkers(lng = ~Longitude,
                 lat = ~Latitude,
                 popup = paste("Intersection", df$At.An.Intersection, "<br>",
                               "Street Name:", df$Street))#%>% 
      #setView(-93.65, 42.0285, zoom = 100)
    m
  })
  
  # Create data table
  output$vadtable <- DT::renderDataTable({
    
    df <- data()%>%
      select(Report.Number,Street,Street.Description,Intersecting.Block.Number,IntersectingStreet,Intersecting.Street.Description,Latitude,Longitude,Hit.And.Run,Fatality,At.An.Intersection,Crash.Date.Time)
    DT::datatable(data = df,
                  options = list(pageLength = 5),
                  rownames = FALSE) 
  })
  }
  
# Create a Shiny app object
shinyApp(ui = ui, server = server)


