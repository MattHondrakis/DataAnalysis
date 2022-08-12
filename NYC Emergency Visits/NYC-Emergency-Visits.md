NYC Emergency Visits
================
Matthew
5/19/2022

-   <a href="#read-data" id="toc-read-data">Read Data</a>
-   <a href="#part-1" id="toc-part-1">Part 1</a>
    -   <a href="#alter-date-column-and-remove-extract_date"
        id="toc-alter-date-column-and-remove-extract_date">Alter date column and
        remove ‘extract_date’</a>
    -   <a href="#last-300-visits" id="toc-last-300-visits">Last 300 Visits</a>
    -   <a href="#convert-appropriate-columns-to-numeric"
        id="toc-convert-appropriate-columns-to-numeric">Convert appropriate
        columns to numeric</a>
    -   <a href="#number-of-visits-per-zip-code"
        id="toc-number-of-visits-per-zip-code">Number of visits per zip code</a>
-   <a href="#part-2" id="toc-part-2">Part 2</a>
    -   <a href="#plot-empty-map" id="toc-plot-empty-map">Plot empty map</a>
-   <a href="#part-3" id="toc-part-3">Part 3</a>
    -   <a href="#join-data" id="toc-join-data">Join data</a>
    -   <a href="#colored-map-by-visits" id="toc-colored-map-by-visits">Colored
        Map by Visits</a>
-   <a href="#part-4" id="toc-part-4">Part 4</a>
    -   <a href="#census-data" id="toc-census-data">Census Data</a>
    -   <a href="#join-population-data-with-emergency-visits"
        id="toc-join-population-data-with-emergency-visits">Join population data
        with emergency visits</a>
    -   <a href="#visits-per-inhabitant-by-zip-code"
        id="toc-visits-per-inhabitant-by-zip-code">Visits per inhabitant by zip
        code</a>
-   <a href="#part-5" id="toc-part-5">Part 5</a>
    -   <a href="#poverty-dat" id="toc-poverty-dat">Poverty Dat</a>

# Read Data

``` r
emergencyVisits <- 
fromJSON("https://data.cityofnewyork.us/resource/2nwg-uqyg.json?$limit=3000")
```

# Part 1

## Alter date column and remove ‘extract_date’

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

## Convert appropriate columns to numeric

``` r
last300 <- last300 %>% 
  rename(ZIPCODE = mod_zcta) %>% 
  mutate(across(.cols = -c(date, ZIPCODE), as.numeric))
```

## Number of visits per zip code

``` r
last300 %>% 
  group_by(ZIPCODE) %>% 
  summarize(total = sum(total_ed_visits)) %>% 
  arrange(-total)
```

    ## # A tibble: 146 x 2
    ##    ZIPCODE total
    ##    <chr>   <dbl>
    ##  1 10457     595
    ##  2 11207     564
    ##  3 11368     558
    ##  4 10029     541
    ##  5 10461     500
    ##  6 11432     440
    ##  7 10456     419
    ##  8 10304     415
    ##  9 11235     399
    ## 10 10002     393
    ## # ... with 136 more rows

# Part 2

``` r
NYCmap <- st_read("C:/Users/Matthew Hondrakis/Downloads/ZIP_CODE_040114", layer = "ZIP_CODE_040114")
```

    ## Reading layer `ZIP_CODE_040114' from data source 
    ##   `C:\Users\Matthew Hondrakis\Downloads\ZIP_CODE_040114' using driver `ESRI Shapefile'
    ## Simple feature collection with 263 features and 12 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 913129 ymin: 120020.9 xmax: 1067494 ymax: 272710.9
    ## Projected CRS: NAD83 / New York Long Island (ftUS)

## Plot empty map

``` r
NYCmap %>% 
  ggplot() + geom_sf() 
```

![](NYC-Emergency-Visits_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Part 3

## Join data

``` r
joined_data <- last300 %>% 
  inner_join(NYCmap)
```

    ## Joining, by = "ZIPCODE"

## Colored Map by Visits

``` r
NYCmap %>% 
  left_join(last300) %>% 
  group_by(ZIPCODE) %>% 
  mutate(total = sum(total_ed_visits)) %>% 
  ggplot(aes(fill = total)) + geom_sf() +
  scale_fill_viridis_c() + 
  labs(title = "Map of total visits", caption = "Grey shade = NA from left joining on last 300 visits") +
  theme(plot.title = element_text(hjust = 0.5))
```

    ## Joining, by = "ZIPCODE"

![](NYC-Emergency-Visits_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

# Part 4

## Census Data

``` r
census_api_key("aa512886c5449a582d837da8d3a07af66a043fe5", install=TRUE, overwrite=T)
```

    ## Your original .Renviron will be backed up and stored in your R HOME directory if needed.

    ## Your API key has been stored in your .Renviron and can be accessed by Sys.getenv("CENSUS_API_KEY"). 
    ## To use now, restart R or run `readRenviron("~/.Renviron")`

    ## [1] "aa512886c5449a582d837da8d3a07af66a043fe5"

``` r
readRenviron("~/.Renviron")

censusData <- load_variables(2018, "acs5", cache=T)

populationData <- get_acs(geography = "zcta", variables = 'B01003_001', geometry = FALSE)
```

    ## Getting data from the 2016-2020 5-year ACS

``` r
populationData <- populationData %>% 
  mutate(ZIPCODE = sub("ZCTA5 ", "", NAME)) %>% 
  rename(population = estimate)
```

## Join population data with emergency visits

``` r
pop_ed_visits <- last300 %>% 
  inner_join(populationData)
```

    ## Joining, by = "ZIPCODE"

## Visits per inhabitant by zip code

``` r
pop_ed_visits <- pop_ed_visits %>% 
  group_by(ZIPCODE) %>% 
  mutate(total = sum(total_ed_visits),
         ed_ratio = total_ed_visits/population)
```

# Part 5

## Poverty Dat

``` r
vars <- c(poverty = 'B17001_002')
povertyData <- get_acs(geography = "zcta", variables = vars, geometry = FALSE)
```

    ## Getting data from the 2016-2020 5-year ACS
