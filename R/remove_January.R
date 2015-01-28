remove_January <- function(df){
  #removes all January data and keeps all Feb-Dec data
  newdf <- df[which(month(df$v.date)!=1),]
}