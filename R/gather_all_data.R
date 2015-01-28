gather_all_data <- function(years) {
  # Uses gather_data to gather all the data for years inputted as a parameter
  x <- NULL
  for(i in years) {
    
    y <- gather_data(i)
    x <- rbind(x, y)
  }
  
  return(x)
  x %>%
    # applies the monthly_return function to add a cum_return column to display the cumulative return
    monthly_return()
}