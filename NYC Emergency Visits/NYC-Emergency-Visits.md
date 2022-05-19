NYC Emergency Visits
================
Matthew
5/19/2022

# Read Data

``` r
emergencyVisits <- 
fromJSON("https://data.cityofnewyork.us/resource/2nwg-uqyg.json?$limit=3000")
```

# EDA

## Alter date column and remove ‘extract\_date’

``` r
emergencyVisits <- emergencyVisits %>% 
  mutate(date = as.POSIXct(date)) %>% 
  select(-extract_date)
```

## Last 300 Visits

``` r
last300 <- emergencyVisits %>% 
  arrange(desc(date)) %>% 
  head(300)
```
