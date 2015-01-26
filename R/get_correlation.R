get_correlation <- function(x, symbolList, lagnumber){
  
  x <- filter(x, symbol %in% (symbolList))
  
  x <- select(x, v.date, symbol, price, tret)
  x <- arrange(x, v.date)
  
  # acf(x$tret, na.action = na.pass)
  
  if(lagnumber == 1){
    #finds correlation between price one day to price the next day
    x <- mutate(x,lag_tret = lag(tret))
    cor(x$tret, x$lag_tret, use = "p")
    #acf(x$tret, lag.max = 12, na.action = na.pass)$acf
  } else if(lagnumber == 3) {
    x <- mutate(x, lag_tret = lag(lag(lag(tret))))
    } else if(lagnumber == 6) {
    x <- mutate(x, lag_tret = lag(lag(lag(lag(lag(lag(tret)))))))
  } else if(lagnumber == 12) {
    x <- mutate(x, lag_tret = lag(lag(lag(lag(lag(lag(lag(lag(lag(lag(lag(lag(tret)))))))))))))
    }else{
    stop()
  }
  
  cor(x$tret, x$lag_tret, use = "p")
}