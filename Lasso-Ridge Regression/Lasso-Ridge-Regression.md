Lasso-Ridge Regression
================
Matthew
May 17, 2023

- <a href="#read-data" id="toc-read-data">Read Data</a>
- <a href="#explore" id="toc-explore">Explore</a>
- <a href="#split-data" id="toc-split-data">Split Data</a>
- <a href="#linear-regression" id="toc-linear-regression">Linear
  Regression</a>
- <a href="#ridge-regression" id="toc-ridge-regression">Ridge
  Regression</a>
- <a href="#lasso-regression" id="toc-lasso-regression">Lasso
  Regression</a>
- <a href="#mse" id="toc-mse">MSE</a>

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

data <- read.table("~/DataAnalysis/Lasso-Ridge Regression/data.txt", col.names = col_names)
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

# Explore

``` r
data %>% 
  pivot_longer(-MEDV) %>% 
  group_by(name) %>% 
  summarize(corr = cor(log(MEDV), value)) %>% 
  arrange(-abs(corr)) %>% 
  ggplot(aes(abs(corr), fct_reorder(name, abs(corr)), fill = corr>0)) +
  geom_col(color = "black") +
  geom_text(aes(label = round(abs(corr), 2)), hjust = 1.2) +
  labs(y = "Variable", x = "Correlation",
       title = "Variables Correlated with Log(Price)")
```

![](Lasso-Ridge-Regression_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
data %>% 
  mutate(logmdev = log(MEDV)) %>% 
  select(logmdev, LSTAT, RM, TAX) %>% 
  pivot_longer(-logmdev) %>% 
  ggplot(aes(value, logmdev)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~name, scales = "free") +
  labs(title = "Log Median Price by 3 Most Correlated") 
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Lasso-Ridge-Regression_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

# Split Data

``` r
library(tidymodels)
```

    ## Registered S3 method overwritten by 'tune':
    ##   method                   from   
    ##   required_pkgs.model_spec parsnip

    ## -- Attaching packages -------------------------------------- tidymodels 0.1.4 --

    ## v broom        1.0.1     v rsample      0.1.1
    ## v dials        0.1.0     v tune         0.1.6
    ## v infer        1.0.4     v workflows    0.2.4
    ## v modeldata    1.0.1     v workflowsets 0.2.1
    ## v parsnip      0.2.0     v yardstick    0.0.9
    ## v recipes      0.2.0

    ## -- Conflicts ----------------------------------------- tidymodels_conflicts() --
    ## x scales::discard() masks purrr::discard()
    ## x dplyr::filter()   masks stats::filter()
    ## x recipes::fixed()  masks stringr::fixed()
    ## x dplyr::lag()      masks stats::lag()
    ## x yardstick::spec() masks readr::spec()
    ## x recipes::step()   masks stats::step()
    ## x tune::tune()      masks parsnip::tune()
    ## * Use suppressPackageStartupMessages() to eliminate package startup messages

``` r
set.seed(123)
data_split <- initial_split(data, 1/2)
data_train <- training(data_split)
data_testing <- testing(data_split)
```

# Linear Regression

``` r
rec <- recipe(MEDV ~ ., data_train) %>% 
  step_log(all_outcomes())

lm_spec <- linear_reg() %>% 
  set_mode("regression") %>% 
  set_engine("lm")

lm_mod <- workflow() %>% 
  add_model(lm_spec) %>% 
  add_recipe(rec) %>% 
  last_fit(data_split)

lm_mod %>% collect_metrics()
```

    ## # A tibble: 2 x 4
    ##   .metric .estimator .estimate .config             
    ##   <chr>   <chr>          <dbl> <chr>               
    ## 1 rmse    standard       0.206 Preprocessor1_Model1
    ## 2 rsq     standard       0.754 Preprocessor1_Model1

# Ridge Regression

``` r
ridge_spec <- linear_reg(mixture = 0, penalty = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")

penaltygrid <- grid_regular(penalty(range = c(-3,3)), levels = 30)

ridge_wkfl <- workflow() %>% 
  add_model(ridge_spec) %>% 
  add_recipe(rec)

tune_res_ridge <- tune_grid(
  ridge_wkfl,
  resamples = vfold_cv(data_train, v = 10), 
  grid = penaltygrid
)
```

    ## ! Fold01: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold02: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold03: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold04: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold05: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold06: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold07: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold08: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold09: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold10: internal: A correlation computation is required, but `estimate` is const...

``` r
autoplot(tune_res_ridge)
```

![](Lasso-Ridge-Regression_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
best_p_ridge <- select_best(tune_res_ridge, metric = "rsq")

ridge_wkfl_fit <- ridge_wkfl %>% 
  finalize_workflow(best_p_ridge) %>% 
  last_fit(data_split)

ridge_wkfl_fit %>% 
  collect_metrics()
```

    ## # A tibble: 2 x 4
    ##   .metric .estimator .estimate .config             
    ##   <chr>   <chr>          <dbl> <chr>               
    ## 1 rmse    standard       0.205 Preprocessor1_Model1
    ## 2 rsq     standard       0.753 Preprocessor1_Model1

# Lasso Regression

``` r
lasso_spec <- linear_reg(mixture = 1, penalty = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")

lasso_wkfl <- workflow() %>% 
  add_model(ridge_spec) %>% 
  add_recipe(rec)

tune_res_lasso <- tune_grid(
  ridge_wkfl,
  resamples = vfold_cv(data_train, v = 10), 
  grid = penaltygrid
)
```

    ## ! Fold01: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold02: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold03: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold04: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold05: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold06: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold07: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold08: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold09: internal: A correlation computation is required, but `estimate` is const...

    ## ! Fold10: internal: A correlation computation is required, but `estimate` is const...

``` r
autoplot(tune_res_lasso)
```

![](Lasso-Ridge-Regression_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
best_p_lasso <- select_best(tune_res_lasso, metric = "rsq")

lasso_wkfl_fit <- lasso_wkfl %>% 
  finalize_workflow(best_p_lasso) %>% 
  last_fit(data_split)

lasso_wkfl_fit %>% 
  collect_metrics()
```

    ## # A tibble: 2 x 4
    ##   .metric .estimator .estimate .config             
    ##   <chr>   <chr>          <dbl> <chr>               
    ## 1 rmse    standard       0.205 Preprocessor1_Model1
    ## 2 rsq     standard       0.753 Preprocessor1_Model1

# MSE

``` r
lasso_wkfl_fit %>% 
  collect_metrics() %>% 
  mutate(model = "lasso") %>% 
  bind_rows(ridge_wkfl_fit %>% 
  collect_metrics() %>% 
  mutate(model = "ridge")) %>% 
  bind_rows(lm_mod %>% 
  collect_metrics() %>% 
  mutate(model = "linear")) %>% 
  mutate(.estimate = .estimate^2) %>% 
  filter(.metric == "rmse") %>% 
  select(.estimate, model) %>% 
  arrange(.estimate)
```

    ## # A tibble: 3 x 2
    ##   .estimate model 
    ##       <dbl> <chr> 
    ## 1    0.0419 lasso 
    ## 2    0.0419 ridge 
    ## 3    0.0423 linear
