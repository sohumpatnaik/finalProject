get_correlation <- function(x, symbolList, lagnumber){
  
  # Gets the correlation between total return and the lagged total return of a specified time period
  
  x <- filter(x, symbol %in% (symbolList))
  
  x <- select(x, v.date, symbol, price, tret)
  x <- arrange(x, v.date)
  
  if(lagnumber == 1){
    # finds the correlation between the price of one day and the price of the next day
    x <- mutate(x,lag_tret = lag(tret))
    cor(x$tret, x$lag_tret, use = "p")
  } else if(lagnumber == 3) {
    x <- mutate(x, lag_tret = lag(lag(lag(tret))))
    } else if(lagnumber == 6) {
    x <- mutate(x, lag_tret = lag(lag(lag(lag(lag(lag(tret)))))))
  } else if(lagnumber == 12) {
    x <- mutate(x, lag_tret = lag(lag(lag(lag(lag(lag(lag(lag(lag(lag(lag(lag(tret)))))))))))))
    }else{
    stop()
  }
  
  # Returns the correlation between the total return and the lagged total return
  cor(x$tret, x$lag_tret, use = "p")
}