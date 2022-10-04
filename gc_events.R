library(shiny)
#library(leaflet)

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
    textOutput("screen_output")
  )
  
)

# Define server logic --
server <- function(input, output, session) {
  observeEvent(input$events_screen, { 
    output$screen_output <- renderText({
      "You have hit the Events Screen button."}
      )
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