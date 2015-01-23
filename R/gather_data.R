gather_data <- function(whichyear) {
  #gathers data one year at a time using data from ws.data, yearly, and secref
  file.name <- paste("daily", whichyear, sep = ".")
  data(list = file.name)
  data(yearly)
  data(secref)

  x <- left_join(eval(parse(text = file.name)), secref)

  ## view data by year
  y <- filter(yearly, year == whichyear)
  x <- left_join(x, y)
  x <- tbl_df(x)
  # Removes stocks that have never been part of the top 1500 largest cap between 1998 and 2007
  u <- unique(x$symbol)

  for (i in 1:length(u)){
    if (TRUE %in% yearly$top.1500[which(yearly$symbol==u[i])] == FALSE){
      x <- filter(x, x$symbol!=u[i])
    }
  }
  ## remove companies with erroneous data
  x <- filter(x, ! symbol %in% c("CHTM", "3STTCE", "3MFNF", "KCI", "LDIG", "3CBHDE", "3AUFI", "PTIH", "BRCMB", "POFR", "3PUSH"))
  ##do not need certain variables for our analysis
  x <- select(x, -id, -price.unadj, -name, -m.sec, -m.ind, -top.1500, -year, -volume, -volume.unadj)
  return(x)
}
