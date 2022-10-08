# values to store the location information
event_df <- data.frame(address = c(),
                       latitude = c(),
                       longitude = c(),
                       descriptions = c(),
                       category = c())

validateLat <- function(input) {
  numeric_input = as.numeric(input)
  return (is.numeric(numeric_input) && (-90 <= numeric_input) && (numeric_input <= 90))
}

validateLon <- function(input) {
  numeric_input = as.numeric(input)
  return (is.numeric(numeric_input) && (-180 <= numeric_input) && (numeric_input <= 180))
}

validateAddress <- function(input) {
  return(input != "")
}

validateDesc <- function(input) {
  return(input != "")
}
