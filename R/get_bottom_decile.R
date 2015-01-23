get_bottom_decile <- function(df){

# data should be grouped by symbol, and then date

# Gets bottom decile of a data frame based off compound cumulative return

  df$symbol -> a

  df$symbol %>% unique() -> u


  lchanges <- c()

  for (i in 1:length(u)){
    l <- which(a==u[i])
    change <- x$cum_comp_ret[l[length(l)]]
    lchanges <- c(lchanges, change)
  }

  n <- data.frame(u, lchanges) %>%
    arrange(lchanges)

  return(n[1:round(length(a)/10),])

}
