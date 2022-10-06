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
      "add event"
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
  output$grinnell_map <- renderLeaflet({
    leaflet() %>% setView(lng = -92.718, lat = 41.749, zoom = 15) %>%
      addTiles()
  })
}

# Run the app
shinyApp(ui = ui, server = server)