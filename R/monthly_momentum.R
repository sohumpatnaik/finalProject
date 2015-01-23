monthly_momentum <- function(df){
  # M = V - Vx
  # Where V is the latest price, Vx is the closing price x number of days ago

  a <- df$price

  b <- lag(df$price)

  monthly_momentum = a-b

  df <- bind_cols(df, as.data.frame(monthly_momentum))

}
