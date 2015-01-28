split_buckets <- function(df){

  # Splits stocks into 5 buckets based off the cap

  df  %>%
    group_by(v.date) %>%
    # makes bucket column with 5 buckets split by ascending cap
    mutate(bucket = cut(cap.usd,
                        quantile(cap.usd, probs = 2*as.numeric(paste0(".", 0:5))),
                        labels = paste0("bucket",1:5))) %>%
    # removes any rows with bucket that is NA
    filter(! is.na(bucket)) %>%
    # displays data by ascending bucket and cap
    group_by(v.date, bucket) %>% arrange(bucket, cap.usd)
}
