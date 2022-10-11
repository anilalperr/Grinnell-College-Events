# values to store the location information
event_df <- read.csv("events.csv")

validateLat <- function(input) {
  numeric_input = as.numeric(input)
  if (!is.na(numeric_input)) {
    return((-90 <= numeric_input) && (numeric_input <= 90))
  }
  return (FALSE)
}

validateLon <- function(input) {
  numeric_input = as.numeric(input)
  if (!is.na(numeric_input)) {
    return((-180 <= numeric_input) && (numeric_input <= 180))
  } 
  return (FALSE)
}

validateAddress <- function(input) {
  return(input != "")
}

validateDesc <- function(input) {
  return(input != "")
}
