Title
================
Matthew
October 25, 2023

``` r
data <- read_csv("~/DataAnalysis/FilteredAccidents.csv") %>% 
  rename_with(tolower)
```

``` r
data %>% count(severity, sort = TRUE)
```

    ## # A tibble: 4 x 2
    ##   severity       n
    ##      <dbl>   <int>
    ## 1        2 1842918
    ## 2        3   77957
    ## 3        4   50335
    ## 4        1   37875
