
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GEDIsamp

<!-- badges: start -->
<!-- badges: end -->

The goal of GEDIsamp is to sample, generate statistics, and plot outputs

## Installation

You can install the development version of GEDIsamp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tgoodbody/GEDIsamp")
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

stats_nested(data = samples, cores = cores, .f = .f)
#> # A tibble: 15 × 5
#>     iter nSamp method data              statistics      
#>    <dbl> <dbl> <chr>  <list>            <list>          
#>  1     1    50 lhs    <tibble [50 × 3]> <tibble [6 × 3]>
#>  2     2    50 lhs    <tibble [50 × 3]> <tibble [6 × 3]>
#>  3     3    50 lhs    <tibble [50 × 3]> <tibble [6 × 3]>
#>  4     4    50 lhs    <tibble [50 × 3]> <tibble [6 × 3]>
#>  5     5    50 lhs    <tibble [50 × 3]> <tibble [6 × 3]>
#>  6     1    50 srs    <tibble [50 × 3]> <tibble [6 × 3]>
#>  7     2    50 srs    <tibble [50 × 3]> <tibble [6 × 3]>
#>  8     3    50 srs    <tibble [50 × 3]> <tibble [6 × 3]>
#>  9     4    50 srs    <tibble [50 × 3]> <tibble [6 × 3]>
#> 10     5    50 srs    <tibble [50 × 3]> <tibble [6 × 3]>
#> 11     1    50 lpm    <tibble [50 × 3]> <tibble [6 × 3]>
#> 12     2    50 lpm    <tibble [50 × 3]> <tibble [6 × 3]>
#> 13     3    50 lpm    <tibble [50 × 3]> <tibble [6 × 3]>
#> 14     4    50 lpm    <tibble [50 × 3]> <tibble [6 × 3]>
#> 15     5    50 lpm    <tibble [50 × 3]> <tibble [6 × 3]>
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
#> 1   1.36  4.69 0.60    1    50    lhs POINT (792316.2 5272377)
#> 2   3.70 13.28 0.94    1    50    lhs POINT (787136.9 5274271)
#> 3   6.64 19.64 1.24    1    50    lhs POINT (787187.2 5283090)
#> 4   0.56  1.85 0.17    1    50    lhs POINT (792149.7 5276770)
#> 5   9.84 21.93 3.14    1    50    lhs   POINT (801748 5269030)
#> 6  13.12 23.00 3.53    1    50    lhs POINT (793360.3 5279120)
#> 7   9.92 18.95 2.82    1    50    lhs   POINT (789775 5278571)
#> 8   9.56 23.79 2.31    1    50    lhs POINT (801973.4 5276343)
#> 9   0.98  2.87 0.37    1    50    lhs POINT (792863.6 5273792)
#> 10  6.27 17.01 1.58    1    50    lhs POINT (792363.9 5274080)

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
