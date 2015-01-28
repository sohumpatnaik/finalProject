gather_data <- function(whichyear) {
  # Gets the yearly data for the inputted year
  data(yearly)
  x <- yearly
  x <- filter(yearly, year == whichyear) 
  
  # filters out the companies that weren't in the top 1500
  # filters out other companies we manually found to be outliers
  x %>%
    filter(top.1500 == "TRUE") %>%
    
    #removes data of outliers
    filter(!(symbol %in% c("CHTM", "3STTCE", "3MFNF", "KCI", "LDIG", "3CBHDE", "3AUFI", 
                           "PTIH", "BRCMB", "POFR", "3PUSH", "3BIGTQ", "3WCIIQ", "ACRUQ",
                           "CLRNE", "ENGA", "NPNTQ", "PGEXQ", "PTVI", "RTHMQ", "SCNTQ"))) %>%
    select(-top.1500)
  
  #gathers secref and daily data
  data(secref)
  file.name <- paste("daily", whichyear, sep = ".")
  data(list = file.name)
  secref_data <- select(secref, -name, -m.sec, -m.ind)
  y <- left_join(eval(parse(text = file.name)), secref_data)
  y <- select(y, -price.unadj, -volume.unadj) 
  
  ## joins all the data
  x <- left_join(x, y)
  x <- tbl_df(x)
  
  # removes unecessary variables for our analysis
  x <- select(x, -year)
  
  # removes all symbols that didn't exist for every year from 1998 to 2007
  u <- unique(x$symbol)
  rem <- c()
  for (i in 1:length(u)){
    a <- yearly[which(yearly$symbol==u[i]),]
    if (TRUE %in% is.na(a$cap.usd)){
      rem <- c(rem, u[i])
    }
  }
  x %>%
    filter(!(symbol %in% rem))
  
}