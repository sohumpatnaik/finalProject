monthly_return <- function(df){
  # Returns a single row per symbol per month (keeps the last trading day's data)
  
  df %>%
    mutate(month = paste(month(v.date, label = TRUE), year(v.date), sep = "-")) %>%
    group_by(month) %>%
    # orders the dates of each month group in descending order
    mutate(day_rank = row_number(desc(v.date))) %>%
    # finds the last day of each month
    filter(day_rank == 1) %>%
    ungroup() %>%
    select(v.date) -> dates
  
  df  %>%
    mutate(month = paste(month(v.date, label = TRUE), year(v.date), sep = "-")) %>%
    # arranges data frame by symbol
    group_by(symbol) %>%
    arrange(symbol, v.date)  %>%
    # creates new column that calculates cumulative monthly return
    mutate(cum_ret = cumsum(tret)) %>%
    # only displays dates that are in the dates dataframe (last date of each month)
    filter(v.date %in% dates$v.date)
  
}

