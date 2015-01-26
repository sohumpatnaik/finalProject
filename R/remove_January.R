remove_January <- function(df){
  #removes all january data and keeps all feb-dec data
  newdf <- df[which(month(df$v.date)!=1),]
}