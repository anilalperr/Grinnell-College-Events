library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
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
      title = "Delete Event",
      value = "delete_event",
      selectInput(inputId = "delete_input",
                  label = "Select an event to delete",
                  choices = event_df$address),
      actionButton(inputId = "delete_button",
                   label = "Delete the selected event")
    ),
    
    tabPanel(
      title = "Event Stats",
      value = "event_stats",
      selectInput(inputId = "value_comp",
                  label = "Select Statistics to Compare",
                  choices = c())
    ),
    
    tabPanel(
      title = "About",
      value = "about_page",
      h1("What is the goal of this app?"),
      p("This app helps the Grinnell students see the events happening on campus. 
        The goal is to bring people with similar interests together! Add events, invite people, and have fun.
        Go to the add events page to add an event. Once you add an event, you will see a marker showing up
        where the event is on the events screen page. You can delete an event from the delete event page. This app is worth
        your time if you are bored and do not know what is happening on campus."),
      h1("Design Process"),
      h1("Acknowledgements"),
      p("I would like to thank Professor Fernanda Elliot and our class mentor Jung for their support and
        mentorship.")
    )
  ),
  
)

# Define server logic --
server <- function(input, output, session) {
  #The Events Screen Tab
  output$grinnell_map <- renderLeaflet({
    if (nrow(event_df) == 0) {
      leaflet() %>% setView(lng = -92.718, lat = 41.749, zoom = 15) %>%
        addTiles() 
    } else {
      leaflet(data=event_df) %>% setView(lng = -92.718, lat = 41.749, zoom = 15) %>%
        addTiles() %>% addMarkers(lng = ~long, lat = ~lat,
                                  layerId = id,
                                  label = ~paste(address),
                                  popup = ~paste("Description:", description))
    }
  })
  
  #The Add Event Tab
  observeEvent(input$submit_button, {
          if (validateAddress(input$event_address) && validateDesc(input$description) && validateLat(input$latitude) && validateLon(input$longitude)) {
              
              event_df[nrow(event_df)+1,] <- c(input$event_address, input$description, input$category, input$latitude, input$longitude, nrow(event_df)+1)
              
              event_df$lat <- as.numeric(event_df$lat)
              event_df$long <- as.numeric(event_df$long)
              
              write.csv(event_df, "events.csv", row.names=FALSE)
              
              leafletProxy("grinnell_map", session) %>%
                addMarkers(lng = ~long, lat = ~lat,
                           label = ~paste(address),
                           layerId = id,
                           popup = ~paste("Description:", description),
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
  
  observeEvent(input$delete_button, {
          row_id = filter(event_df, address == input$delete_input)$id[1]
          event_df = filter(event_df, address != input$delete_input)
          write.csv(event_df, "events.csv", row.names=FALSE)
          
          leafletProxy("grinnell_map", session) %>%
            removeMarker(row_id)
        })
}

# Run the app
shinyApp(ui = ui, server = server)