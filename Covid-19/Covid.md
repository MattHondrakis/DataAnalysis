Covid
================
Matthew
2/24/2022

- <a href="#eda" id="toc-eda">EDA</a>
  - <a href="#case-count-per-borough" id="toc-case-count-per-borough">Case
    Count per Borough</a>
  - <a href="#line-plot-of-case-count-and-hospitalization-over-time"
    id="toc-line-plot-of-case-count-and-hospitalization-over-time">Line plot
    of case count and hospitalization over time</a>
  - <a href="#proportion-of-dead-over-hospitalized-per-year"
    id="toc-proportion-of-dead-over-hospitalized-per-year">Proportion of
    dead over hospitalized per year</a>
    - <a href="#death-count" id="toc-death-count">Death count</a>
  - <a href="#death-count-by-borough" id="toc-death-count-by-borough">Death
    count by borough</a>

``` r
df <- read_csv("https://data.cityofnewyork.us/resource/rc75-m7u3.csv")
```

    ## Rows: 1000 Columns: 67
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## dbl  (66): case_count, probable_case_count, hospitalized_count, death_count,...
    ## dttm  (1): date_of_interest
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df <- df %>%
  rename(date = date_of_interest)
```

# EDA

## Case Count per Borough

``` r
df %>%
  select(date, matches("^[[:alpha:]]{2}_case_count$")) %>%
  pivot_longer(names_to = "Borough",-date) %>%
  mutate(Borough = toupper(str_sub(Borough,1,2)),
         Borough = fct_reorder(Borough, -value, mean)) %>%
  ggplot(aes(date, value, color = Borough)) + geom_line() +
  labs(y = "", x = "", title = "NYC Case Count", subtitle = "From 2/29/20 to 2/20/22")
```

![](Covid_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
df %>%
  tail(93) %>% 
  select(date, matches("^[[:alpha:]]{2}_case_count$")) %>%
  pivot_longer(names_to = "Borough",-date) %>%
  mutate(Borough = toupper(str_sub(Borough,1,2)),
         Borough = fct_reorder(Borough, -value, mean)) %>%
  ggplot(aes(date, value, color = Borough)) + geom_line() +
  labs(y = "", x = "", title = "NYC Case Count", subtitle = "From 11/20/21 to 2/20/22")
```

![](Covid_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

## Line plot of case count and hospitalization over time

``` r
df %>%
  pivot_longer(names_to = "Metric", 
               c("hospitalized_count", "death_count")) %>%
  ggplot(aes(date, value, color = Metric)) + geom_line()
```

![](Covid_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Proportion of dead over hospitalized per year

``` r
df %>%
  filter(hospitalized_count > 0) %>%
  mutate(proportion = death_count/hospitalized_count) %>% 
  filter(proportion <= 1) %>%
  ggplot(aes(date, proportion)) + geom_line() +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  theme(panel.grid.minor = element_blank())
```

![](Covid_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
df %>%
  select(date, death_count, hospitalized_count) %>%
  filter(hospitalized_count > 0) %>%
  mutate(proportion = death_count/hospitalized_count) %>%
  filter(proportion > 1) 
```

    ## # A tibble: 0 x 4
    ## # ... with 4 variables: date <dttm>, death_count <dbl>,
    ## #   hospitalized_count <dbl>, proportion <dbl>

### Death count

``` r
df %>%
  head(60) %>%
  ggplot(aes(date, death_count)) + geom_line() +
  labs(title = "Last 90 days", y = "Deaths", x = "")
```

![](Covid_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Death count by borough

``` r
df %>%
  select(date, bk_death_count, bx_death_count,
         mn_death_count, si_death_count, qn_death_count) %>%
  pivot_longer(!date) %>%
  ggplot(aes(date, value, color = name)) + geom_line()
```

![](Covid_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
df %>%
  mutate(dow = weekdays(date),
         dow = fct_relevel(dow, "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) %>%
  filter(!is.na(dow)) %>%
  group_by(dow) %>%
  ggplot(aes(dow, case_count, fill = dow)) + geom_col() + 
  labs(y = "", x = "", title = "Cases by day of the week") +
  theme(legend.position = "", plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels = scales::comma)
```

![](Covid_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
