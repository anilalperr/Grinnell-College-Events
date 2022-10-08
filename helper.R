# values to store the location information
address = c()
latitude = c()
longitude = c()
descriptions = c()
category = c()

event_df <- data.frame(address = c(),
                       latitude = c(),
                       longitude = c(),
                       descriptions = c(),
                       category = c())

validateNumeric <- function(input, t) {
  numeric_input = as.numeric(input)
  if (t == "lat") {
    return (is.numeric(numeric_input) && (-90 <= numeric_input) && (numeric_input <= 90))
  } else {
    return (is.numeric(numeric_input) && (-180 <= numeric_input) && (numeric_input <= 180))
  }
}

validateAddress <- function(input) {
  return(input)
}