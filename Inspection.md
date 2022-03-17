Inspections
================
Matthew
3/16/2022

``` r
inspections <- read_csv("https://data.cityofnewyork.us/resource/43nn-pn8j.csv")
```

    ## Rows: 1000 Columns: 26
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (15): dba, boro, building, street, phone, cuisine_description, action, ...
    ## dbl   (8): camis, zipcode, score, latitude, longitude, community_board, bin,...
    ## dttm  (3): inspection_date, grade_date, record_date
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
inspections <- inspections %>%
  rename(id = camis, name = dba)
```

# EDA

``` r
inspections %>%
  filter(!is.na(score)) %>%
  summarize(min(score), max(score))
```

    ## # A tibble: 1 x 2
    ##   `min(score)` `max(score)`
    ##          <dbl>        <dbl>
    ## 1            0           97

``` r
inspections %>%
  count(score, sort = TRUE)
```

    ## # A tibble: 76 x 2
    ##    score     n
    ##    <dbl> <int>
    ##  1    12    95
    ##  2    13    78
    ##  3    10    68
    ##  4     9    54
    ##  5    11    49
    ##  6    NA    45
    ##  7    21    31
    ##  8    19    29
    ##  9    22    27
    ## 10    20    26
    ## # ... with 66 more rows

## Analysis of Boroughs

``` r
inspections %>%
  filter(!is.na(score), boro != 0) %>%
  group_by(boro) %>%
  summarize(m = mean(score)) %>%
  ggplot(aes(m, fct_reorder(boro, m), fill = boro)) + geom_col() +
  labs(y = "", x = "Average Score", fill = "", title = "Average Score by Borough")
```

![](Inspection_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
inspections %>%
  filter(grade %in% c("A", "B", "C")) %>%
  group_by(boro) %>%
  count(grade, sort = TRUE) %>%
  ggplot(aes(n, fct_rev(grade), fill = boro)) + geom_col(position = "dodge") +
  labs(y = "", x = "Count", title = "The Number of Restaurants by Borough and Grade")
```

![](Inspection_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
(inspections %>%
  filter(grade %in% c("A", "B", "C")) %>%
  group_by(boro) %>%
  summarize(A = mean(grade == "A"),
            B = mean(grade == "B"),
            C = mean(grade == "C")) %>%
  pivot_longer(-boro) %>%
  ggplot(aes(value, fct_rev(name), fill = boro)) + geom_col(position = "dodge") +
  labs(y = "", x = "", fill = "") + 
  scale_x_continuous(breaks = seq(0,1,0.1),
                     labels = scales::percent_format())) /
  (inspections %>%
    filter(grade %in% c("A", "B", "C")) %>%
    group_by(boro) %>%
    summarize(A = mean(grade == "A"),
              B = mean(grade == "B"),
              C = mean(grade == "C")) %>%
    pivot_longer(-boro) %>%
    ggplot(aes(value, fct_reorder(boro, value, .fun = max), fill = fct_rev(name))) + geom_col() +
    labs(y = "", x = "", fill = "") +
    scale_x_continuous(labels = scales::percent_format())) + 
  plot_annotation(title = "Percent of Restaurants by Grade and Borough",
                  theme = theme(plot.title = element_text(hjust = 0.5)))
```

![](Inspection_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
borofun <- function(x){
  inspections %>%
    filter(boro == {{x}}, !is.na(score)) %>%
    group_by(cuisine_description) %>%
    summarize(max_score = max(score)) %>%
    arrange(-max_score) 
}


tablefun <- function(x) {
  borofun({{x}}) %>% 
    head(5) %>%
    mutate(Borough = paste({{x}}, " (Top)")) %>%
    bind_rows(borofun({{x}}) %>%
    tail(5) %>%
    mutate(Borough = paste({{x}}, " (Bottom)")))
}
table <- tablefun("Bronx") %>%
  bind_rows(tablefun("Brooklyn")) %>%
  bind_rows(tablefun("Manhattan")) %>%
  bind_rows(tablefun("Queens")) %>%
  bind_rows(tablefun("Staten Island"))
```

### Table of Top 5 and Bottom 5 Cuisine Types for each Borough

``` r
table[,c(3,1,2)] %>% knitr::kable()
```

| Borough                | cuisine\_description           | max\_score |
|:-----------------------|:-------------------------------|-----------:|
| Bronx (Top)            | Donuts                         |         69 |
| Bronx (Top)            | Coffee/Tea                     |         68 |
| Bronx (Top)            | Chinese                        |         63 |
| Bronx (Top)            | Latin American                 |         52 |
| Bronx (Top)            | Pizza                          |         47 |
| Bronx (Bottom)         | Steakhouse                     |         18 |
| Bronx (Bottom)         | Bakery Products/Desserts       |         17 |
| Bronx (Bottom)         | Hamburgers                     |         13 |
| Bronx (Bottom)         | Japanese                       |         13 |
| Bronx (Bottom)         | Frozen Desserts                |          7 |
| Brooklyn (Top)         | Bakery Products/Desserts       |         75 |
| Brooklyn (Top)         | Caribbean                      |         63 |
| Brooklyn (Top)         | American                       |         57 |
| Brooklyn (Top)         | Indian                         |         57 |
| Brooklyn (Top)         | Asian/Asian Fusion             |         54 |
| Brooklyn (Bottom)      | Hamburgers                     |         11 |
| Brooklyn (Bottom)      | African                        |         10 |
| Brooklyn (Bottom)      | French                         |         10 |
| Brooklyn (Bottom)      | Greek                          |         10 |
| Brooklyn (Bottom)      | Soul Food                      |          7 |
| Manhattan (Top)        | Pizza                          |         97 |
| Manhattan (Top)        | Chinese                        |         85 |
| Manhattan (Top)        | Italian                        |         78 |
| Manhattan (Top)        | American                       |         70 |
| Manhattan (Top)        | African                        |         67 |
| Manhattan (Bottom)     | Vegetarian                     |         12 |
| Manhattan (Bottom)     | Hamburgers                     |         11 |
| Manhattan (Bottom)     | Peruvian                       |         11 |
| Manhattan (Bottom)     | Frozen Desserts                |          9 |
| Manhattan (Bottom)     | Donuts                         |          6 |
| Queens (Top)           | Sandwiches/Salads/Mixed Buffet |         91 |
| Queens (Top)           | Chinese                        |         88 |
| Queens (Top)           | Latin American                 |         83 |
| Queens (Top)           | Sandwiches                     |         83 |
| Queens (Top)           | American                       |         79 |
| Queens (Bottom)        | Southeast Asian                |         12 |
| Queens (Bottom)        | Continental                    |         11 |
| Queens (Bottom)        | Frozen Desserts                |         11 |
| Queens (Bottom)        | Seafood                        |          9 |
| Queens (Bottom)        | Donuts                         |          8 |
| Staten Island (Top)    | American                       |         76 |
| Staten Island (Top)    | Chinese                        |         44 |
| Staten Island (Top)    | Mexican                        |         36 |
| Staten Island (Top)    | Russian                        |         32 |
| Staten Island (Top)    | Chicken                        |         28 |
| Staten Island (Bottom) | Hamburgers                     |         13 |
| Staten Island (Bottom) | Sandwiches                     |         12 |
| Staten Island (Bottom) | Japanese                       |          9 |
| Staten Island (Bottom) | Spanish                        |          9 |
| Staten Island (Bottom) | Coffee/Tea                     |          7 |
