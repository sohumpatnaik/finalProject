get_quintile <- function(df, n){

  # Get stocks that are in the specifed bucket

  b <- unique(df$symbol[which(df$bucket==paste0("bucket", n))])
  return (b)
}
