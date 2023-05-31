Title
================
Matthew
May 30, 2023

``` r
start_to_may27_23 <- read_csv("~/DataAnalysis/Robinhood Investments/start_to_may27_23.csv")
```

``` r
start_to_may27_23 %>% 
  mutate(Amount = parse_number(Amount)) %>% 
  filter(`Trans Code` == "ACH") %>% 
  summarize(sum(Amount, na.rm = TRUE))
```

    ## # A tibble: 1 x 1
    ##   `sum(Amount, na.rm = TRUE)`
    ##                         <dbl>
    ## 1                       5116.
