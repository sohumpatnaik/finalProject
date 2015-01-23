gather_all_data <- function(years) {
  #uses gather_data to gather all data for years inputted as a parameter
  x <- NULL
  for(i in years) {

    y <- gather_data(i)
    x <- rbind(x, y)
  }
  x %>%
    #applies monthly_return function to add cum_return column
    monthly_return()
}
