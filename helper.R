# values to store the location information
event_df <- data.frame(matrix(ncol = 5, nrow = 0))
c_names <- c("address", "descriptions", "category", "lat", "long")
colnames(event_df) <- c_names

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
