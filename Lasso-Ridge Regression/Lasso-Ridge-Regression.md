Lasso-Ridge Regression
================
Matthew
May 17, 2023

- <a href="#read-data" id="toc-read-data">Read Data</a>

# Read Data

``` r
col_names <- c(
 "CRIM", 
 "ZN", 
 "INDUS",
 "CHAS",
 "NOX",
 "RM",
 "AGE",
 "DIS",
 "RAD",
 "TAX",
 "PTRATIO",
 "B",
 "LSTAT",
 "MEDV")

data <- read.table("C:/Users/Matthew Hondrakis/OneDrive/Documents/data.txt", col.names = col_names)
```

``` r
data_description <- data.frame(
  Variable = c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV"),
  Description = c(
    "per capita crime rate by town",
    "proportion of residential land zoned for lots over 25,000 sq.ft.",
    "proportion of non-retail business acres per town",
    "Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)",
    "nitric oxides concentration (parts per 10 million)",
    "average number of rooms per dwelling",
    "proportion of owner-occupied units built prior to 1940",
    "weighted distances to five Boston employment centres",
    "index of accessibility to radial highways",
    "full-value property-tax rate per $10,000",
    "pupil-teacher ratio by town",
    "1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town",
    "% lower status of the population",
    "Median value of owner-occupied homes in $1000's")
)

knitr::kable(data_description)
```

| Variable | Description                                                           |
|:---------|:----------------------------------------------------------------------|
| CRIM     | per capita crime rate by town                                         |
| ZN       | proportion of residential land zoned for lots over 25,000 sq.ft.      |
| INDUS    | proportion of non-retail business acres per town                      |
| CHAS     | Charles River dummy variable (= 1 if tract bounds river; 0 otherwise) |
| NOX      | nitric oxides concentration (parts per 10 million)                    |
| RM       | average number of rooms per dwelling                                  |
| AGE      | proportion of owner-occupied units built prior to 1940                |
| DIS      | weighted distances to five Boston employment centres                  |
| RAD      | index of accessibility to radial highways                             |
| TAX      | full-value property-tax rate per \$10,000                             |
| PTRATIO  | pupil-teacher ratio by town                                           |
| B        | 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town        |
| LSTAT    | % lower status of the population                                      |
| MEDV     | Median value of owner-occupied homes in \$1000’s                      |
