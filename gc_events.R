library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(maps)
library(maptools)
data("us.cities")

# Define UI
ui <- fluidPage (
  titlePanel(title = "Welcome to the Grinnell College Events Page!"),
  
  titlePanel(
    img(src = "gc_logo.png", height = 102, width = 119, align = "right")
  ),
  
  fluidRow(
    column(2,
           actionButton("events_screen", "Events Screen")
           ),
    
    column(3,
           actionButton("event_stats", "Weekly Event Stats")
    ),
    
    column(2,
           actionButton("add_event", "Add an Event")
    ),
    
    column(2,
           actionButton("my_events", "My Events")
    )
  ),
  
  mainPanel(
    textOutput("screen_output"),
    leafletOutput("grinnell_map")
  )
  
)

# Define server logic --
server <- function(input, output, session) {
  observeEvent(input$events_screen, { 
    output$screen_output <- renderText({
      "You have hit the Events Screen button."}
      )
    output$grinnell_map <- renderLeaflet({
      leaflet() %>% setView(lng = -92.718, lat = 41.749, zoom = 15) %>%
        addTiles()
      })
    })
  
  observeEvent(input$event_stats, { 
    output$screen_output <- renderText({
      "You have hit the Weekly Event Stats button."}
    )
  })
  
  observeEvent(input$add_event, { 
    output$screen_output <- renderText({
      "You have hit the Add Event button."}
    )
  })
  
  observeEvent(input$my_events, { 
    output$screen_output <- renderText({
      "You have hit the My Events button."}
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)