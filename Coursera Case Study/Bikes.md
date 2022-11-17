Coursera Case Study: Bikes
================
Matthew
2022-11-10

-   <a href="#google-analytics-case-study-cyclistic"
    id="toc-google-analytics-case-study-cyclistic">Google Analytics Case
    Study: Cyclistic</a>
-   <a href="#data-validation" id="toc-data-validation">Data Validation</a>
-   <a href="#exploratory-data-analysis"
    id="toc-exploratory-data-analysis">Exploratory Data Analysis</a>
    -   <a href="#casuals-vs-members" id="toc-casuals-vs-members">Casuals vs
        Members</a>
        -   <a href="#ride-duration-distributions"
            id="toc-ride-duration-distributions">Ride Duration Distributions</a>
        -   <a href="#weekdays-vs-weekends" id="toc-weekdays-vs-weekends">Weekdays
            vs Weekends</a>
        -   <a href="#type-of-bikes-used" id="toc-type-of-bikes-used">Type of Bikes
            Used</a>
        -   <a href="#popular-start-stations"
            id="toc-popular-start-stations">Popular Start Stations</a>

# Google Analytics Case Study: Cyclistic

The purpose of this case study is the dive into the data and find
differences between members and casuals, with the hopes of converting
casuals to members. Members are individuals that buy an annual
subscription service, while casuals are individuals that buy single ride
passes.

Multiple data sets (13) were downloaded from a link provided by
[Coursera](https://divvy-tripdata.s3.amazonaws.com/index.html) and then
was merged into one csv, which was subsequently read using the code
chunk below. The first chunk uses a for-loop to iterate the *read_csv*
(read data files) and *rbind* (combine data rowwise) functions till all
data files are read; which is then saved as a csv of its own. Now the
csv can be read only once using the following code chunk without going
through the iteration process.

``` r
files <- list.files("D:/Downloads/Case Study")
bikes <- read_csv("D:/Downloads/Case Study/202110-divvy-tripdata.csv")

for(i in 2:length(files)) {
  bikes <- bikes %>% rbind(read_csv(paste0("D:/Downloads/Case Study/",
                   files[i])))
}

write_csv(bikes, "bikes.csv")
```

``` r
bikes <- read_csv("D:/Downloads/Case Study/Full Data/bikes.csv")
```

    ## Rows: 6386920 Columns: 13
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

Create columns that are the *length* of bike ride (in minutes) and
*day_of_week* (1 = Sunday, 7 = Saturday).

``` r
bikes$length <- difftime(bikes$ended_at, bikes$started_at, unit = "mins")
bikes$day_of_week <- wday(bikes$started_at)
bikes$day_label <- wday(bikes$started_at, label = TRUE)
```

``` r
s_bikes <- bikes %>% sample_n(500000)  # sample data set only for the purpose of quicker computations during analysis
```

# Data Validation

There are some instances where the data suggests that an individual
started a ride after they ended it. This will be assumed to be faulty
and the values will be converted to their absolute values. The faulty
data consists of only **0.00175%** (112 out of 6.3m) of the total data.

``` r
bikes %>% 
  summarize(neg_time = mean(length < 0) * 100)
```

    ## # A tibble: 1 x 1
    ##   neg_time
    ##      <dbl>
    ## 1  0.00175

``` r
bikes %>% 
  filter(length < 0) %>% 
  select(started_at, ended_at)
```

    ## # A tibble: 112 x 2
    ##    started_at          ended_at           
    ##    <dttm>              <dttm>             
    ##  1 2021-11-07 01:40:02 2021-11-07 01:05:46
    ##  2 2021-11-07 01:52:53 2021-11-07 01:05:22
    ##  3 2021-11-07 01:40:13 2021-11-07 01:00:29
    ##  4 2021-11-07 01:34:03 2021-11-07 01:17:13
    ##  5 2021-11-07 01:54:25 2021-11-07 01:03:44
    ##  6 2021-11-07 01:54:04 2021-11-07 01:25:57
    ##  7 2021-11-07 01:51:52 2021-11-07 01:22:53
    ##  8 2021-11-07 01:54:12 2021-11-07 01:05:09
    ##  9 2021-11-07 01:54:36 2021-11-07 01:03:11
    ## 10 2021-11-07 01:51:21 2021-11-07 01:07:59
    ## # ... with 102 more rows

``` r
bikes$length <- abs(bikes$length)
s_bikes$length <- abs(s_bikes$length)
```

``` r
clrs <- c("blue", "green4") # vector of colors to be used
```

``` r
skimr::skim_without_charts(bikes %>% select(-matches("lng|lat")))
```

|                                                  |                             |
|:-------------------------------------------------|:----------------------------|
| Name                                             | bikes %\>% select(-matches… |
| Number of rows                                   | 6386920                     |
| Number of columns                                | 12                          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                             |
| Column type frequency:                           |                             |
| character                                        | 7                           |
| difftime                                         | 1                           |
| factor                                           | 1                           |
| numeric                                          | 1                           |
| POSIXct                                          | 2                           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                             |
| Group variables                                  | None                        |

Data summary

**Variable type: character**

| skim_variable      | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:-------------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| ride_id            |         0 |          1.00 |  16 |  16 |     0 |  6386920 |          0 |
| rideable_type      |         0 |          1.00 |  11 |  13 |     0 |        3 |          0 |
| start_station_name |    986387 |          0.85 |   7 |  64 |     0 |     1640 |          0 |
| start_station_id   |    986387 |          0.85 |   3 |  44 |     0 |     1308 |          0 |
| end_station_name   |   1054844 |          0.83 |   9 |  64 |     0 |     1663 |          0 |
| end_station_id     |   1054844 |          0.83 |   3 |  44 |     0 |     1315 |          0 |
| member_casual      |         0 |          1.00 |   6 |   6 |     0 |        2 |          0 |

**Variable type: difftime**

| skim_variable | n_missing | complete_rate | min    | max           | median     | n_unique |
|:--------------|----------:|--------------:|:-------|:--------------|:-----------|---------:|
| length        |         0 |             1 | 0 mins | 41387.25 mins | 10.35 mins |    23350 |

**Variable type: factor**

| skim_variable | n_missing | complete_rate | ordered | n_unique | top_counts                                          |
|:--------------|----------:|--------------:|:--------|---------:|:----------------------------------------------------|
| day_label     |         0 |             1 | TRUE    |        7 | Sat: 1068394, Fri: 914981, Thu: 909070, Sun: 893711 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean |   sd |  p0 | p25 | p50 | p75 | p100 |
|:--------------|----------:|--------------:|-----:|-----:|----:|----:|----:|----:|-----:|
| day_of_week   |         0 |             1 | 4.11 | 2.03 |   1 |   2 |   4 |   6 |    7 |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| started_at    |         0 |             1 | 2021-10-01 00:00:09 | 2022-10-31 23:59:33 | 2022-06-18 23:50:58 |  5349251 |
| ended_at      |         0 |             1 | 2021-10-01 00:03:11 | 2022-11-07 04:53:58 | 2022-06-19 00:17:08 |  5359703 |

# Exploratory Data Analysis

## Casuals vs Members

**Summary**

-   There are *3.77* million rides by members and *2.61* million rides
    by casuals, accounting for *59.1%* and *40.9%* of the data,
    respectively.
-   Members and casuals have approximately equal duration distributions,
    with casuals having a slightly longer rides.
-   Members tend to use rides during the weekday while casuals tend to
    use rides during the weekend.
-   Casuals disproportionately prefer specific bike stations.
-   Number of rides peak at both 8:00 am and 5:00 pm for members, while
    rides for casuals peak only at 5:00 pm.
-   Members used both *classic* and *electric* bikes approximately
    equally, while casuals disproportionately used *electric* bikes.
    Furthermore, there is no data of members using *docked* bikes in the
    last 12 months.
    -   Members: **51.7%** Classic, **48.3%** Electric

    -   Casual: **53.7%** Electric, **38.4%** Classic, **7.9%** Docked

``` r
bikes %>% 
  count(member_casual, sort = TRUE) %>% 
  mutate(percent = n/sum(n)*100)
```

    ## # A tibble: 2 x 3
    ##   member_casual       n percent
    ##   <chr>           <int>   <dbl>
    ## 1 member        3776645    59.1
    ## 2 casual        2610275    40.9

### Ride Duration Distributions

``` r
(bikes %>% 
  ggplot(aes(as.numeric(length), member_casual, fill = member_casual)) + 
  geom_boxplot() +
  scale_x_log10(label = comma_format()) + labs(y = "", x = "", fill = "") + scale_fill_manual(values = clrs))/
(bikes %>% 
   ggplot(aes(as.numeric(length), fill = member_casual)) + 
   geom_density(alpha = 0.5) + theme(legend.position = "none") +
   scale_x_log10(label = comma_format()) + labs(y = "", x = "") + scale_fill_manual(values = c("blue", "green4"))) +
  plot_annotation(title = "Ride Duration Distributions") + plot_layout(guides = "collect")
```

![](Bikes_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Weekdays vs Weekends

Members and casuals appear to use rides for different reasons.

-   Members tend to use rides on weekdays, which implies its used to
    commute to and from work; while casuals tend to use rides on the
    weekend, which implies that the main purpose of use is leisure.

``` r
(bikes %>% 
  ggplot(aes(fct_reorder(day_label, day_of_week), fill = member_casual)) + 
  geom_bar(position = position_dodge2()) + 
  scale_fill_manual(values = clrs) +
  scale_y_continuous(labels = comma_format()) +
  labs(y = "", x = "", 
       title = "Days Members and Casuals Use Rides",
       fill = "")) /
bikes %>% 
  mutate(wend = ifelse(day_label %in% c("Sat", "Sun"), "Weekend", "Weekday")) %>% 
  group_by(member_casual) %>% 
  count(wend, sort = TRUE) %>% 
  mutate(percent = n/sum(n)) %>% 
  ggplot(aes(x = member_casual, y = percent, fill = wend)) + 
  geom_bar(position="fill", stat = "identity") +
  geom_text(aes(y = ifelse(percent > 0.50, 0.65, 0.15), label = paste0(round(percent, 3)*100,"%"))) +
  scale_fill_brewer(palette = "Accent", direction = -1) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  scale_y_continuous(label = percent_format()) +
  labs(y = "", x = "", fill = "", title = "Percentage of Rides Used on Weekdays or Weekends")
```

![](Bikes_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Type of Bikes Used

``` r
bikes %>% 
  group_by(member_casual) %>% 
  count(rideable_type, sort = TRUE) %>% 
  mutate(total = sum(n), pct = n/total) %>% 
  ggplot(aes(pct, member_casual, fill = rideable_type)) + 
  geom_col(position = position_dodge2()) +
  scale_x_continuous(label = percent_format()) + 
  geom_text(aes(label = paste0(round(pct, 3)*100, "%")), position = position_dodge2(0.9), hjust = 1.1) +
  labs(title = "Proportion of Bikes Used by Membership", y = "", x = "", fill = "") +
  scale_fill_brewer(palette = "Set2", direction = -1)
```

![](Bikes_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### Popular Start Stations

``` r
(bikes %>% 
  filter(!is.na(start_station_name)) %>% 
  group_by(member_casual) %>% 
  count(start_station_name, sort = TRUE) %>% 
  top_n(5) %>% 
  ggplot(aes(n, fct_reorder(start_station_name, n), fill = member_casual)) + 
  geom_col() +
  geom_text(aes(label = n), color = "white", fontface = "bold", hjust = 1.2) +
  labs(y = "", x = "", fill = "", title = "Start Stations") + theme(plot.title = element_text(hjust = 0)) +
  scale_fill_manual(values = clrs)) /
bikes %>% 
  filter(!is.na(end_station_name)) %>% 
  group_by(member_casual) %>% 
  count(end_station_name, sort = TRUE) %>% 
  top_n(5) %>% 
  ggplot(aes(n, fct_reorder(end_station_name, n), fill = member_casual)) + 
  geom_col() +
  geom_text(aes(label = n), color = "white", fontface = "bold", hjust = 1.2) +
  labs(y = "", x = "", fill = "", title = "End Stations") +
  scale_fill_manual(values = clrs) + theme(plot.title = element_text(hjust = 0)) +
  plot_layout(guides = "collect") + plot_annotation(title = "Top 10 Stations")
```

    ## Selecting by n
    ## Selecting by n

![](Bikes_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

The number of rides started, locally peak at 8:00 am and 5:00 pm for
members, which further reinforces the hypothesis that members use rides
to go and come back from work. This assumption is based on the fact that
the typical work hours are from 9am-5pm, which would lead people start
rides at 8am to get to work, and then pick up a ride again after work at
5pm. Furthermore, this pattern is not observed on *Weekends*.

``` r
bikes %>% 
  mutate(hour = hour(started_at)) %>% 
  group_by(member_casual, hour) %>% 
  count(hour, sort = TRUE) %>% 
  ggplot(aes(hour, n, color = member_casual)) + 
  geom_line() + scale_color_manual(values = clrs) +
  scale_x_continuous(breaks = seq(0,24,4), labels = function(x) paste0(x, ":00")) +
  labs(y = "", x = "", title = "Number of Rides per Hour", color = "")
```

![](Bikes_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
bikes %>% 
  filter(day_label %in% c("Sat", "Sun")) %>% 
  mutate(hour = hour(started_at)) %>% 
  group_by(member_casual, hour) %>% 
  count(hour, sort = TRUE) %>% 
  ggplot(aes(hour, n, color = member_casual)) + 
  geom_line() + scale_color_manual(values = clrs) +
  scale_x_continuous(breaks = seq(0,24,4), labels = function(x) paste0(x, ":00")) +
  labs(y = "", x = "", title = "Number of Rides per Hour only Weekends", 
       subtitle = "Saturday and Sunday", color = "") +
  theme(plot.subtitle = element_text(hjust = 0.5))
```

![](Bikes_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

The function below will provide a quick and easy way to join a data
frame to the original, containing information on which group found
certain stations more popular than others.

``` r
wide_fun <- function(data){
  data %>% 
    group_by(member_casual) %>% 
    filter(!is.na(start_station_name)) %>% 
    count(start_station_name) %>% 
    pivot_wider(names_from = member_casual, values_from = n) %>% 
    mutate(casual = ifelse(is.na(casual), 0, casual),
           member = ifelse(is.na(member), 0, member),
           pct_casual = casual/sum(casual),
           pct_member = member/sum(member),
           the_largest = ifelse(pct_casual > pct_member, pct_casual, pct_member),
           membership = ifelse(pct_casual > pct_member, "casual", "member")) %>% 
    select(start_station_name, the_largest, membership)
}
```

``` r
bikes %>% 
  inner_join(wide_fun(s_bikes)) %>% 
  count(membership, sort = TRUE)
```

    ## Joining, by = "start_station_name"

    ## # A tibble: 2 x 2
    ##   membership       n
    ##   <chr>        <int>
    ## 1 member     3143626
    ## 2 casual     2255066

The histogram plot below describes the popularity of stations for each
group. The vertical axis represents the amount of stations and the
horizontal axis represents percentage use. For example, in the case for
members, we see a large peak towards the right portion of the x-axis.
This shows that the **majority** of stations are used about evenly
(between 0.1% - 1%). The peak on the left side for members is a natural
consequence of the fact that the remainder of stations are not used very
often. For casuals, most stations are located on the left portion of the
graph, implying that in fact most stations are not used very often.

``` r
wide_fun(bikes) %>% 
  ggplot(aes(the_largest, fill = membership)) + 
  geom_histogram(alpha = 0.5, position = "identity", bins = 30, color = "grey20") +
  scale_fill_manual(values = clrs) +
  scale_x_log10(labels = percent_format()) + 
  scale_y_continuous() +
  labs(y = "Proportion of Stations", x = "Percent Use (log-scale)", 
                         fill = "", 
                         title = "Proportion of Stations by Percent Use")
```

![](Bikes_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
wide_fun(bikes) %>% 
  group_by(membership) %>% 
  summarize(max = max(the_largest),
            `95%` = quantile(the_largest, 0.95),
            `80%` = quantile(the_largest, 0.8),
            `60%` = quantile(the_largest, 0.6),
            median = median(the_largest),
            `40%` = quantile(the_largest, 0.4),
            `20%` = quantile(the_largest, 0.2),
            min = min(the_largest)) %>% 
  mutate(across(where(is.numeric), ~ paste0(round(.x * 100, 4), "%"))) %>% 
  knitr::kable()
```

| membership | max     | 95%     | 80%     | 60%     | median  | 40%     | 20%    | min |
|:-----------|:--------|:--------|:--------|:--------|:--------|:--------|:-------|:----|
| casual     | 2.8683% | 0.3058% | 0.0208% | 0.0044% | 0.002%  | 7e-04%  | 1e-04% | 0%  |
| member     | 0.892%  | 0.4635% | 0.2561% | 0.1217% | 0.0572% | 0.0114% | 1e-04% | 0%  |
