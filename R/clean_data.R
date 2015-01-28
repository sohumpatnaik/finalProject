clean_data <- function(x) {
  # Shows the companies with highest total returns in descending order so we
  # can manually identify the outliers/data errors
  x %>%
    select(-name) %>%
    filter(row_number(desc(tret)) < 10) %>%
    arrange(desc(tret))
}

