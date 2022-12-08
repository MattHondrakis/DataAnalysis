Masters Project: Job Placement
================
Matthew
2022-12-07

-   <a href="#intro" id="toc-intro">Intro</a>
    -   <a href="#data-dictionary" id="toc-data-dictionary">Data Dictionary</a>
-   <a href="#cleanse" id="toc-cleanse">Cleanse</a>
-   <a href="#outliers" id="toc-outliers">Outliers</a>

``` r
fall <- read_csv("C:/Users/Matthew Hondrakis/OneDrive/Documents/DataAnalysis/Masters Project Fall Placement/fall2022Placement.csv")
```

    ## Rows: 215 Columns: 15
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (8): gender, ssc_b, hsc_b, hsc_s, degree_t, workex, specialisation, status
    ## dbl (7): sl_no, ssc_p, hsc_p, degree_p, etest_p, mba_p, salary
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Intro

This is a structured analysis guided by the assignment provided by my
friends professor. The task is as follows.

-   **Cleanse**: Look for null data in the table, and use the
    appropriate strategy to handle null data for each column. Explain
    why this was the strategy that you used.

-   **Outliers**: Search the data for outliers and remove them. Use the
    appropriate outlier method(s). Show all work.

-   **Who are these graduates?**: Create 4-5 visualizations providing
    useful and explanatory information about these graduates. The
    visualizations should show trends, correlations, and other useful
    patterns that the data provides.  
    Remember, we are dealing with senior management. The charts must be
    readable, meaningful and at a summary level.  
    Use at least one group/by or bin in your analysis.

-   **Who are most likely to get placed?**: Create visualizations to
    show management qualities specific to those who are likely to get
    placed at jobs. Show three meaningful visualizations.  
    Use at least one group/by or bin in your analysis.

-   **Prepare the data for a regression analysis**: The XYZ management
    company team has heard that it might be possible to use their data
    to make predictions. They don’t know much about data analytics. We
    are hoping that an example of what is possible will help them
    understand better.  
    Prepare the data file to run with a regression analysis. Use the
    techniques and methods discussed in class. (*I’m not a student, so
    I’m unfamiliar with what methods were discussed in class*)

## Data Dictionary

``` r
knitr::kable(readxl::read_excel("C:/Users/Matthew Hondrakis/OneDrive/Documents/DataAnalysis/Masters Project Fall Placement/Data dictionary.xlsx"))
```

| Column         | Explanation                                  |
|:---------------|:---------------------------------------------|
| sl_no          | Serial Number                                |
| gender         | Gender (M/F)                                 |
| ssc_p          | Secondary Education percentile               |
| ssc_b          | Board of Education                           |
| hsc_p          | Higher Secondary Education percentile        |
| hsc_b          | Board of Education- Central/ Others          |
| hsc_s          | Specialization in Higher Secondary Education |
| degree_p       | Degree Percentile                            |
| degree_t       | Under Graduation(Degree type)                |
| workex         | Work Experience                              |
| etest_p        | Employability test percentile                |
| specialisation | Area of speciality                           |
| mba_p          | MBA percentile                               |
| Salary         | Salary of job offered                        |
| status         | placed or not placed (target variable)       |

# Cleanse

``` r
skimr::skim(fall) %>% select(skim_type, skim_variable, n_missing)
```

|                                                  |      |
|:-------------------------------------------------|:-----|
| Name                                             | fall |
| Number of rows                                   | 215  |
| Number of columns                                | 15   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |      |
| Column type frequency:                           |      |
| character                                        | 8    |
| numeric                                          | 7    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |      |
| Group variables                                  | None |

Data summary

**Variable type: character**

| skim_variable  | n_missing |
|:---------------|----------:|
| gender         |         0 |
| ssc_b          |         0 |
| hsc_b          |         0 |
| hsc_s          |         1 |
| degree_t       |         0 |
| workex         |         0 |
| specialisation |         1 |
| status         |         0 |

**Variable type: numeric**

| skim_variable | n_missing |
|:--------------|----------:|
| sl_no         |         0 |
| ssc_p         |         0 |
| hsc_p         |         2 |
| degree_p      |         0 |
| etest_p       |         0 |
| mba_p         |         0 |
| salary        |        67 |

As we can see, we have *N/A* values for:

-   *hsc_s* (Specialization in Higher Secondary Education): **1**

-   *specialisation* (Area of speciality): **1**

-   *salary* (Salary of job offered): **67**

``` r
fall %>% 
  count(hsc_s, sort = TRUE)
```

    ## # A tibble: 4 x 2
    ##   hsc_s        n
    ##   <chr>    <int>
    ## 1 Commerce   112
    ## 2 Science     91
    ## 3 Arts        11
    ## 4 <NA>         1

``` r
fall %>% 
  count(specialisation, sort = TRUE)
```

    ## # A tibble: 4 x 2
    ##   specialisation     n
    ##   <chr>          <int>
    ## 1 Mkt&Fin          119
    ## 2 Mkt&HR            94
    ## 3 Mt&HR              1
    ## 4 <NA>               1

I am going to make a few assumptions. I will take “Mkt” to mean
“Marketing”, “Fin” to mean “Finance”, “HR” to mean “Human Resources”. We
see only one variable having the value of “Mt”, which we may assume is a
typo or just an outlier. For now, I will consider this a valid data
point, assume it means something like “Management or Math” and wont
change it to “Mkt”.

``` r
fall %>% 
  filter(is.na(hsc_s)) %>% 
  select(salary)
```

    ## # A tibble: 1 x 1
    ##   salary
    ##    <dbl>
    ## 1     NA

``` r
fall %>% 
  filter(is.na(specialisation)) %>% 
  select(salary)
```

    ## # A tibble: 1 x 1
    ##   salary
    ##    <dbl>
    ## 1 250000

The row that has a missing value for *hsc_s* (Specialization in Higher
Secondary Education) also contains a missing value for *salary*; this is
not the case for when specialisation is NA.

``` r
fall %>% 
  group_by(specialisation) %>% 
  count(hsc_s) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(specialisation, -n)
```

    ## # A tibble: 9 x 4
    ## # Groups:   specialisation [4]
    ##   specialisation hsc_s        n   prop
    ##   <chr>          <chr>    <int>  <dbl>
    ## 1 Mkt&Fin        Commerce    69 0.580 
    ## 2 Mkt&Fin        Science     42 0.353 
    ## 3 Mkt&Fin        Arts         8 0.0672
    ## 4 Mkt&HR         Science     48 0.511 
    ## 5 Mkt&HR         Commerce    42 0.447 
    ## 6 Mkt&HR         Arts         3 0.0319
    ## 7 Mkt&HR         <NA>         1 0.0106
    ## 8 Mt&HR          Science      1 1     
    ## 9 <NA>           Commerce     1 1

``` r
fall %>% 
  group_by(hsc_b) %>% 
  count(hsc_s) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(hsc_b, -n)
```

    ## # A tibble: 7 x 4
    ## # Groups:   hsc_b [2]
    ##   hsc_b   hsc_s        n   prop
    ##   <chr>   <chr>    <int>  <dbl>
    ## 1 Central Commerce    47 0.560 
    ## 2 Central Science     29 0.345 
    ## 3 Central Arts         7 0.0833
    ## 4 Central <NA>         1 0.0119
    ## 5 Others  Commerce    65 0.496 
    ## 6 Others  Science     62 0.473 
    ## 7 Others  Arts         4 0.0305

Most of “Mkt&HR” are evenly distributed between **Science** and
**Commerce**. Thus its not possible at the moment to decide which one of
the 2 should be used to fill in the missing value. With regards to
*hsc_b* (Board of Education), the majority of **Central** are in
**Commerce**. This may provide a clue later on, but it is still too
early to make an assumption.

``` r
fall %>% 
  group_by(missing = is.na(salary)) %>% 
  count(status) %>% 
  arrange(status)
```

    ## # A tibble: 2 x 3
    ## # Groups:   missing [2]
    ##   missing status         n
    ##   <lgl>   <chr>      <int>
    ## 1 TRUE    Not Placed    67
    ## 2 FALSE   Placed       148

As we can see, all missing values for *salary* are from individuals that
were not offered a job. Since the Data Dictionary defines *Salary* as
“Salary of job offered”, individuals that were not offered a job, do not
have a salary from an offer. Therefore, *salary* will be 0 for
individuals that were not offered a job.

``` r
clean_fall <- fall %>% 
  mutate(salary = ifelse(is.na(salary), 0, salary))
```

# Outliers

``` r
(fall %>% 
  ggplot(aes(salary)) +
  geom_boxplot() +
  scale_x_log10(labels = comma_format()))/
(fall %>% 
  ggplot(aes(salary)) +
  geom_histogram() +
  scale_x_log10(labels = comma_format()))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Masters-Project-Fall-Placement_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
