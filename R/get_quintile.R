get_quintile <- function(df, n){

  # Get stocks that are in the specifed bucket

  symbol <- as.character(df$symbol)
  bucket <- as.character(df$bucket)
  b <- unique(df$symbol[which(df$bucket==paste0("bucket", n))])
  
  as.character(b)
}
