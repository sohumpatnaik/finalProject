gather_January <- function(df){
  # Gathers all the January data and removes data from other time periods
  newdf <- df[which(month(df$v.date)==1),]
  
}