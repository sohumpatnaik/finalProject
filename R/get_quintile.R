get_quintile <- function(df, n){

  # Get stocks that are in the specifed bucket

  symbol <- as.character(df$symbol)
  bucket <- as.character(df$bucket)
  if (n %in% (1:5)){
  b <- unique(df$symbol[which(df$bucket==paste0("bucket", n))])
  } else {
    stop()
  }  
  
  # Returns the stocks in bucket n as a character
  as.character(b)
}
