NYC House Prices
================
Matthew
4/24/2022

``` r
house <- read_csv("C:/Users/Matthew Hondrakis/OneDrive/Documents/data_lat_long2.csv")
```

    ## New names:
    ## * `` -> ...1

    ## Rows: 6163 Columns: 28
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (23): bath, bed, sqft, address, estimated mortage, school information, c...
    ## dbl  (5): ...1, Price, assessment year, Lat, Lon
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
house <- house[,-1]
```

# Explore and Clean

``` r
house[1:20,] %>% 
  View()

skim(house)
```

|                                                  |       |
|:-------------------------------------------------|:------|
| Name                                             | house |
| Number of rows                                   | 6163  |
| Number of columns                                | 27    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |       |
| Column type frequency:                           |       |
| character                                        | 23    |
| numeric                                          | 4     |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |       |
| Group variables                                  | None  |

Data summary

**Variable type: character**

| skim\_variable                               | n\_missing | complete\_rate | min |  max | empty | n\_unique | whitespace |
|:---------------------------------------------|-----------:|---------------:|----:|-----:|------:|----------:|-----------:|
| bath                                         |        270 |           0.96 |   6 |    9 |     0 |        26 |          0 |
| bed                                          |        210 |           0.97 |   5 |    7 |     0 |        25 |          0 |
| sqft                                         |        364 |           0.94 |   6 |   12 |     0 |      2274 |          0 |
| address                                      |          0 |           1.00 |  25 |   53 |     0 |      5370 |          0 |
| estimated mortage                            |        201 |           0.97 |  19 |   26 |     0 |      4189 |          0 |
| school information                           |          0 |           1.00 |  21 |   61 |     0 |        50 |          0 |
| crime information                            |          0 |           1.00 |  37 |   62 |     0 |        33 |          0 |
| commute information                          |          0 |           1.00 |  40 |   47 |     0 |        65 |          0 |
| shop and eat information                     |          0 |           1.00 |  36 |   56 |     0 |      5027 |          0 |
| description                                  |         14 |           1.00 |   1 | 3003 |     0 |      5751 |          0 |
| home details                                 |          1 |           1.00 |   8 |  882 |     0 |      5975 |          0 |
| price details                                |       1581 |           0.74 |  11 | 8859 |     0 |      4183 |          0 |
| tax                                          |       1629 |           0.74 |   3 |  363 |     0 |      3076 |          0 |
| land assessment cost                         |       1629 |           0.74 |   3 |  138 |     0 |      3517 |          0 |
| improvement cost                             |       1630 |           0.74 |   2 |  171 |     0 |      3824 |          0 |
| total cost                                   |       1630 |           0.74 |   4 |  693 |     0 |      3095 |          0 |
| neighborhood name                            |         40 |           0.99 |   4 |   35 |     0 |       248 |          0 |
| what local say about the neighborhood        |         67 |           0.99 |   6 |  454 |     0 |       528 |          0 |
| comments of residents and previous residents |         58 |           0.99 |   7 | 8260 |     0 |       619 |          0 |
| comparable properties                        |       1567 |           0.75 |   7 | 6722 |     0 |      4511 |          0 |
| provider info                                |        392 |           0.94 |   7 | 4027 |     0 |      3960 |          0 |
| seo description                              |          0 |           1.00 |  14 | 1163 |     0 |      5812 |          0 |
| linktoproperty                               |          0 |           1.00 |  68 | 4952 |     0 |      5812 |          0 |

**Variable type: numeric**

| skim\_variable  | n\_missing | complete\_rate |       mean |         sd |       p0 |       p25 |       p50 |        p75 |       p100 | hist  |
|:----------------|-----------:|---------------:|-----------:|-----------:|---------:|----------:|----------:|-----------:|-----------:|:------|
| Price           |       2065 |           0.66 | 2324633.85 | 5814353.04 | 40000.00 | 528952.00 | 785000.00 | 1325000.00 |  7.900e+07 | ▇▁▁▁▁ |
| assessment year |       1635 |           0.73 |    2017.84 |       0.37 |  2014.00 |   2018.00 |   2018.00 |    2018.00 |  2.018e+03 | ▁▁▁▂▇ |
| Lat             |       1079 |           0.82 |      40.71 |       0.12 |    40.50 |     40.62 |     40.71 |      40.77 |  4.317e+01 | ▇▁▁▁▁ |
| Lon             |       1079 |           0.82 |     -73.96 |       0.14 |   -77.59 |    -74.00 |    -73.96 |     -73.90 | -7.265e+01 | ▁▁▁▇▁ |

``` r
house <- 
  house %>% rename_with(tolower) %>% 
  rename_with( ~ gsub(" information", "", .x)) %>% 
  rename_with( ~ gsub(" ", "_", .x))
```

``` r
house <- house %>% 
  mutate(across(c(bath:sqft, tax:total_cost), ~ parse_number(.x)))

house <- house %>% 
  rename(by_car = commute) %>% 
  mutate(by_car = parse_number(by_car))


house <- house %>% 
  mutate(type = sub(",.*","", home_details))
```

``` r
house %>% 
  keep(is.numeric) %>% 
  select(-lon, -lat) %>% 
  gather() %>% 
  ggplot(aes(value)) + geom_histogram() + facet_wrap(~key, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](NYC-House-Prices_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
house %>% 
  keep(is.numeric) %>% 
  select(-lon, -lat) %>% 
  pivot_longer(-price) %>% 
  ggplot(aes(value, price)) + geom_point() +
  scale_y_log10() + scale_x_log10() +
  facet_wrap(~name)
```

![](NYC-House-Prices_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
house %>% 
  select_if(is.numeric) %>% 
  select(-lon, -lat) %>% 
  drop_na() %>% 
  pivot_longer(-price) %>% 
  group_by(name) %>% 
  summarize(corr = cor(price, value)) %>% 
  arrange(-abs(corr))
```

    ## # A tibble: 9 x 2
    ##   name                    corr
    ##   <chr>                  <dbl>
    ## 1 bath                  0.428 
    ## 2 by_car               -0.245 
    ## 3 bed                   0.214 
    ## 4 land_assessment_cost  0.145 
    ## 5 tax                   0.133 
    ## 6 total_cost            0.130 
    ## 7 improvement_cost      0.124 
    ## 8 sqft                  0.100 
    ## 9 assessment_year       0.0955

``` r
tidy_model <- house %>% 
  select_if(is.numeric) %>% 
  select(-lon, -lat) %>% 
  drop_na() %>% 
  pivot_longer(-price) %>% 
  nest(-name) %>% 
  mutate(mod = map(data, ~ lm(log(price) ~ value, .x)),
         tidy = map(mod, broom::tidy)) %>% 
  unnest(tidy)
```

``` r
tidy_model %>% 
  filter(term == "value") %>% 
  arrange(-abs(estimate))
```

    ## # A tibble: 9 x 8
    ##   name               data     mod   term  estimate std.error statistic   p.value
    ##   <chr>              <list>   <lis> <chr>    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 assessment_year    <tibble> <lm>  value  3.00e-1   3.92e-2      7.65 2.80e- 14
    ## 2 bath               <tibble> <lm>  value  2.75e-1   6.90e-3     39.8  1.71e-276
    ## 3 bed                <tibble> <lm>  value  1.80e-1   7.00e-3     25.8  6.10e-132
    ## 4 by_car             <tibble> <lm>  value -1.24e-2   7.21e-4    -17.2  2.53e- 63
    ## 5 sqft               <tibble> <lm>  value  3.87e-7   2.93e-7      1.32 1.86e-  1
    ## 6 tax                <tibble> <lm>  value  9.22e-8   2.38e-8      3.88 1.09e-  4
    ## 7 land_assessment_c~ <tibble> <lm>  value  7.16e-8   1.33e-8      5.39 7.70e-  8
    ## 8 improvement_cost   <tibble> <lm>  value  1.09e-8   3.26e-9      3.35 8.11e-  4
    ## 9 total_cost         <tibble> <lm>  value  1.00e-8   2.65e-9      3.80 1.50e-  4

## Checking correlated variables

``` r
gplot <- function(x){
  (house %>% 
    filter(!is.na({{x}}), !is.na(price)) %>% 
    ggplot(aes({{x}}, price, group = {{x}})) + geom_boxplot() + scale_y_log10()) +
  (house %>% 
     filter(!is.na({{x}}), !is.na(price)) %>% 
     group_by({{x}}) %>% 
     summarize(m = mean(price, na.rm = TRUE)) %>% 
     ggplot(aes({{x}}, m)) + geom_line() + scale_y_log10())
}

gplot(bath)
```

![](NYC-House-Prices_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
gplot(bed)
```

![](NYC-House-Prices_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
gplot(assessment_year)
```

![](NYC-House-Prices_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
house %>% 
  filter(price != 0, !is.na(price)) %>% 
  mutate(price = price/1e3,
         type = fct_lump(type, 7),
         type = fct_reorder(type, price, median)) %>% 
  ggplot(aes(price, type)) + geom_boxplot() +
  scale_x_log10(labels = scales::comma) + labs(x = "", title = "Price in thousands") +
  theme(plot.margin = margin(10,50,10,0))
```

![](NYC-House-Prices_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->


## Plots of Price by numerics
```{r}
gplot2 <- function(x){
  house %>% 
    ggplot(aes({{x}}, price, color = fct_lump(type, 6))) + 
    geom_point() +
    scale_x_log10() + scale_y_log10()
}
(gplot2(tax) +
gplot2(total_cost) +
gplot2(land_assessment_cost)) /
(gplot2(sqft) +
gplot2(improvement_cost)) + plot_layout(guides = 'collect')
```

# Model
## Preprocess
```{r}
house <- house %>% 
  mutate(type_mod = fct_lump(type, 6))

house_mod <- house %>% 
  select(price, tax, total_cost, sqft, land_assessment_cost,
         improvement_cost, type_mod, bath, bed) %>% 
  drop_na() %>% 
  filter_if(is.numeric, all_vars(. > 0))

house_mod %>% 
  keep(is.numeric) %>% 
  cor()
```


```{r}
house_mod %>% 
  keep(is.numeric) %>% 
  pivot_longer(-tax) %>% 
  ggplot(aes(tax, value)) + geom_point() +
  facet_wrap(~name, scales = "free")
```


```{r}
updated_model <- 
  lm(log(price) ~ (log(tax) + log(sqft) + 
                     bath) * type_mod, 
   house_mod)

anova(updated_model)
summary(updated_model)

augment(updated_model) %>% 
  ggplot(aes(`log(price)`,.fitted, color = type_mod)) + 
  geom_point() + geom_abline()

augment(updated_model) %>% 
  mutate(residual = .fitted - `log(price)`) %>% 
  ggplot(aes(.fitted, residual, color = type_mod)) + 
  geom_point() +
  geom_hline(yintercept = 0)

hist(residuals(updated_model))
```


