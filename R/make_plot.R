make_plot <- function(df) {
  #makes plot of data by standard deviation of total returns and date
  x <- group_by(df, v.date)
  x <- summarize(x, sd_ret = sd(tret, na.rm = TRUE))
  x <- ggplot(x, aes(v.date, sd_ret)) + geom_point()
  return(x)
}
