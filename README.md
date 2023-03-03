
<!-- README.md is generated from README.Rmd. Please edit that file -->

# caRlo

<!-- badges: start -->
<!-- badges: end -->

The goal of caRlo is to sample, generate statistics, and plot outputs

## Installation

You can install the development version of caRlo from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tgoodbody/caRlo")
```

## Example

This is a basic example which shows you how to solve a common problem:

    #--- load internal data ---#

    data("plots")
    data("samples")
    data("stats")

    #--- fake population mean and median ---#
    population <-  data.frame(median = 1.1, mean = 2.2)

    #--- piped & parallel ---#

    cores <- 5

    monte_carlo(plots, nSamp = c(50, 100, 150), iter = 10, cores = cores) %>%
      stats_nested(cores = cores) %>%
      sample_bootstrap(., population = population, cores = cores)

    #--- internal data for testing ---#
    samples <- monte_carlo(data = plots, nSamp = c(50, 100, 150), iter = 3, cores = cores) 

    nested <- stats_nested(data = samples, cores = cores)

    bootstraps <- sample_bootstrap(data = stats, population = population, cores = cores)

    #--- define functions for new stats ---#

## Specify user defined functions for statistics using `.f`

``` r
#--- function needs to have more than 1 metric to give proper outputs ---#


.f <- function(x){

  #--- calculate the mean and percent of points less than the mean ---#
  c(mean = mean(x), 
  perc_gr_mean = length(x[x < mean(x)])/length(x))

} 

s <- stats_nested(data = samples, cores = cores, .f = .f)

s$statistics[[1]]
#> # A tibble: 6 × 3
#>   statistic    name  value
#>   <chr>        <chr> <dbl>
#> 1 mean         zmean  8.92
#> 2 mean         zq90  17.9 
#> 3 mean         lai    2.30
#> 4 perc_gr_mean zmean  0.52
#> 5 perc_gr_mean zq90   0.46
#> 6 perc_gr_mean lai    0.52
```

## outputs

``` r
#--- using internal data ---#

monte_carlo(data = plots, nSamp = c(50, 100, 150), iter = 2, cores = cores) 
#> Simple feature collection with 1800 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 784680.6 ymin: 5266360 xmax: 804241.6 ymax: 5283495
#> Projected CRS: ETRS89 / UTM zone 32N
#> First 10 features:
#>    zmean  zq90  lai iter nSamp method                     geom
#> 1   7.89 15.06 2.89    1    50    lhs POINT (786560.8 5274454)
#> 2   1.79  9.87 0.33    1    50    lhs   POINT (802112 5275445)
#> 3   9.32 20.19 3.07    1    50    lhs POINT (787182.5 5275565)
#> 4  14.65 25.06 4.87    1    50    lhs   POINT (797791 5273284)
#> 5   5.47 14.44 1.51    1    50    lhs   POINT (803039 5274805)
#> 6   1.30  4.07 0.62    1    50    lhs POINT (801968.6 5276545)
#> 7   3.31  7.18 1.85    1    50    lhs POINT (788615.1 5277535)
#> 8  11.81 19.83 4.11    1    50    lhs POINT (801402.9 5273410)
#> 9  10.33 21.76 3.16    1    50    lhs   POINT (795133 5277281)
#> 10  3.84 11.73 1.05    1    50    lhs POINT (794704.1 5275741)

stats_nested(data = samples, cores = cores)
#> # A tibble: 15 × 5
#>     iter nSamp method data              statistics       
#>    <dbl> <dbl> <chr>  <list>            <list>           
#>  1     1    50 lhs    <tibble [50 × 3]> <tibble [42 × 3]>
#>  2     2    50 lhs    <tibble [50 × 3]> <tibble [42 × 3]>
#>  3     3    50 lhs    <tibble [50 × 3]> <tibble [42 × 3]>
#>  4     4    50 lhs    <tibble [50 × 3]> <tibble [42 × 3]>
#>  5     5    50 lhs    <tibble [50 × 3]> <tibble [42 × 3]>
#>  6     1    50 srs    <tibble [50 × 3]> <tibble [42 × 3]>
#>  7     2    50 srs    <tibble [50 × 3]> <tibble [42 × 3]>
#>  8     3    50 srs    <tibble [50 × 3]> <tibble [42 × 3]>
#>  9     4    50 srs    <tibble [50 × 3]> <tibble [42 × 3]>
#> 10     5    50 srs    <tibble [50 × 3]> <tibble [42 × 3]>
#> 11     1    50 lpm    <tibble [50 × 3]> <tibble [42 × 3]>
#> 12     2    50 lpm    <tibble [50 × 3]> <tibble [42 × 3]>
#> 13     3    50 lpm    <tibble [50 × 3]> <tibble [42 × 3]>
#> 14     4    50 lpm    <tibble [50 × 3]> <tibble [42 × 3]>
#> 15     5    50 lpm    <tibble [50 × 3]> <tibble [42 × 3]>

sample_bootstrap(data = stats, population = population, cores = cores)
#> # A tibble: 378 × 6
#> # Groups:   nSamp, method, statistic, name [378]
#>    nSamp method statistic name  data              bootstrap       
#>    <dbl> <chr>  <chr>     <chr> <list>            <list>          
#>  1   100 lhs    min       zmean <tibble [20 × 1]> <tibble [2 × 6]>
#>  2   100 lhs    min       zq90  <tibble [20 × 1]> <tibble [2 × 6]>
#>  3   100 lhs    min       lai   <tibble [20 × 1]> <tibble [2 × 6]>
#>  4   100 lhs    mean      zmean <tibble [20 × 1]> <tibble [2 × 6]>
#>  5   100 lhs    mean      zq90  <tibble [20 × 1]> <tibble [2 × 6]>
#>  6   100 lhs    mean      lai   <tibble [20 × 1]> <tibble [2 × 6]>
#>  7   100 lhs    max       zmean <tibble [20 × 1]> <tibble [2 × 6]>
#>  8   100 lhs    max       zq90  <tibble [20 × 1]> <tibble [2 × 6]>
#>  9   100 lhs    max       lai   <tibble [20 × 1]> <tibble [2 × 6]>
#> 10   100 lhs    var       zmean <tibble [20 × 1]> <tibble [2 × 6]>
#> # … with 368 more rows
```
