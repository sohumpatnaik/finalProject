gather_data <- function(whichyear) {
  #gets yearly data for the inputted year
  data(yearly)
  x <- yearly
  x <- filter(yearly, year == whichyear) 
  #gets all data from all companies in top.1500
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
  y <- select(y, -price.unadj, -volume, -volume.unadj) 
  
  ## join all data
  x <- left_join(x, y)
  x <- tbl_df(x)
  
  ##do not need certain variables for our analysis
  x <- select(x, -year)
  
  ## removes all symbols that didn't exist for every year
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
