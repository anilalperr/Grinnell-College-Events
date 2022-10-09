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
      
      selectInput(inputId = "category",
                  label = "Select Event Category",
                  choices = c("Party", "Academic", "Sports", "Casual")),
     
       "You can find the exact latitude and longitude values by entering the event address to this link:",
      a("https://developers.google.com/maps/documentation/geocoding/overview", href= "https://developers.google.com/maps/documentation/geocoding/overview"),
      
      textInput(inputId = "latitude",
                label = "Enter the Latitude:"),
      
      textInput(inputId = "longitude",
                label = "Enter the longitude:"),
      
      actionButton(inputId= "submit_button",
                   label = "Submit the Event"),
      
      span(textOutput("message"), style="color:red")
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
          if (validateAddress(input$event_address) && validateDesc(input$description) && validateLat(input$latitude) && validateLon(input$longitude)) {
              
              event_df[nrow(event_df)+1,] <- c(input$event_address, input$description, input$category, input$latitude, input$longitude)
              
              event_df$lat <- as.numeric(event_df$lat)
              event_df$long <- as.numeric(event_df$long)
              
              leafletProxy("grinnell_map", session) %>%
                addMarkers(lng = ~long, lat = ~lat,
                           label = ~paste(address),
                           popup = ~paste("Description:", descriptions),
                           data = event_df)
              
              updateTextInput(session, "event_address", value="")
              updateTextInput(session, "description", value="")
              updateTextInput(session, "latitude", value="")
              updateTextInput(session, "longitude", value="")
              updateSelectInput(session, "category", selected="Party")
              
              output$message <- renderText("")
          } else {
            if (!validateAddress(input$event_address)) {
              
              output$message <- renderText("Please enter an address.")
              
            } else if (!validateDesc(input$description)) {
              
              output$message <- renderText("Please provide a description.")
              
            } else if (!validateLat(input$latitude)) {
              
              output$message <- renderText("Please make sure that the latitude is numeric and between -90 and 90 (inclusive)")
              
            } else {
              
              output$message <- renderText("Please make sure that the longitude is numeric and between -180 and 180 (inclusive)")
              
            }
          }
    })
  
}

# Run the app
shinyApp(ui = ui, server = server)