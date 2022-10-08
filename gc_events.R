library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(maps)
library(maptools)
source("helper.R")

# Define UI
ui <- fluidPage (
  titlePanel(title = "Welcome to the Grinnell College Events Page!"),
  
  titlePanel(
    img(src = "gc_logo.png", height = 102, width = 119, align = "right")
  ),
  
  tabsetPanel(
    id = "tabs",
    
    tabPanel(
      title = "Events Screen",
      value = "events_screen",
      leafletOutput("grinnell_map"),
    ),
    
    tabPanel(
      title = "Weekly Event Stats", 
      value = "event_stats",
      "weekly event stats"
    ), 
    
    tabPanel(
      title = "Add Event",
      value = "add_event",
     
       textInput(inputId = "event_address",
                label = "Enter the Address"),
     
       textInput(inputId = "description",
                label = "Event Description:"),
      
      selectInput(inputId = "select_category",
                  label = "Select Event Category",
                  choices = c("Party", "Academic", "Sports", "Casual")),
     
       "You can find the exact latitude and longitude values by entering the event address to this link:",
      a("https://developers.google.com/maps/documentation/geocoding/overview", href= "https://developers.google.com/maps/documentation/geocoding/overview"),
      
      textInput(inputId = "latitude",
                label = "Enter the Latitude:"),
      
      textInput(inputId = "longitude",
                label = "Enter the longitude:"),
      
      actionButton(inputId= "submit_button",
                   label = "Submit the Event")
    ), 
    
    tabPanel(
      title = "My Events",
      value = "my_events",
      "my events"
    )
  ),
  
)

# Define server logic --
server <- function(input, output, session) {
  
  #The Events Screen Tab
  output$grinnell_map <- renderLeaflet({
    leaflet() %>% setView(lng = -92.718, lat = 41.749, zoom = 15) %>%
      addTiles() 
  })
  
  #The Add Event Tab
  observeEvent(input$submit_button, {
          updateTextInput(session, "event_address", value="")
          updateTextInput(session, "description", value="")
          updateTextInput(session, "latitude", value="")
          updateTextInput(session, "longitude", value="")
          updateSelectInput(session, "select_category", selected="Party")
    })
  
}

# Run the app
shinyApp(ui = ui, server = server)