get_all_correlations <- function(x, number, period) {
  
  y <- NULL
  #alters data frame to include either all 12 months, all januaries, or all feb-dec
  if(period == "full"){
    x <- x
  } else if(period == "jan") {
    x <- gather_January(x)
  } else if( period == "fd") {
    x <- remove_January(x)
    }else{
    stop()
  }
  #correlations of 1, 3, 6, and 12 month returns
  if(number == 1 | 3 | 6 | 12){
    for(i in 1:5) {
      
      cor <- get_correlation(x, get_quintile(x, i), number)
      y <- rbind(y, cor)
    }
  }else{
    stop()
  }
  
  return(y)
}
