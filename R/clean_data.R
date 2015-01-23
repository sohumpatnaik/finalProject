clean_data <- function(x) {
  #shows companies with highest total returns in descending order o identify outliers/data errors
  x %>%
    select(-name) %>%
    filter(row_number(desc(tret)) < 10) %>%
    arrange(desc(tret))
}

