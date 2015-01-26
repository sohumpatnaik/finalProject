gather_January <- function(df){
  #gathers all January data and removes other data
  newdf <- df[which(month(df$v.date)==1),]
  
}