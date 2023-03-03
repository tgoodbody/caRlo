
<!-- README.md is generated from README.Rmd. Please edit that file -->

# caRlo

<!-- badges: start -->
<!-- badges: end -->

The goal of `caRlo` is to perform monte carlo sampling simulations,
calculate statistics on sample iterations, bootstrap statistics, and
visualize statistical outputs.

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

    plots
    stats
    bootstraps

    #--- fake population mean and median ---#
    population <-  data.frame(median = 1.1, mean = 2.2)

    #--- piped & parallel ---#

    cores <- 5 # number of cores
    iter <- 10 # number of monte carlo simulations per sample size

    #--- sample - generate stats - bootstrap stats ---#
    monte_carlo(plots, nSamp = c(50, 100, 150), iter = 10, cores = cores) %>%
      stats_nested(cores = cores) %>%
      bootstrap_stats(., population = population, cores = cores)

## Internal data for testing

``` r
#--- raw plots
plots <- caRlo:::plots
gedi <- caRlo:::gedi

#--- samples from monte_carlo() ---#
samples <- caRlo:::samples

#--- stats from stats_nested() ---#
stats <- caRlo:::stats

#--- boostraps from bootstrap_stats() ---#
bootstraps <- caRlo:::bootstraps
```

``` r
#--- sample ---#
monte_carlo(data = plots, nSamp = c(50, 100, 150), iter = 3, cores = cores)
#> Simple feature collection with 2700 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 784680.6 ymin: 5266360 xmax: 804241.6 ymax: 5283282
#> Projected CRS: ETRS89 / UTM zone 32N
#> First 10 features:
#>    zmean  zq90  lai iter nSamp method                     geom
#> 1   1.47  3.04 0.64    1    50    lhs POINT (796291.6 5268117)
#> 2   6.97 17.96 1.79    1    50    lhs POINT (789108.2 5277652)
#> 3  19.49 31.08 4.94    1    50    lhs   POINT (801354 5274227)
#> 4   7.32 16.66 2.07    1    50    lhs POINT (801516.4 5272321)
#> 5  15.01 21.62 4.51    1    50    lhs POINT (801134.4 5277518)
#> 6  10.06 17.20 3.65    1    50    lhs POINT (788651.8 5278835)
#> 7   3.56 10.17 1.38    1    50    lhs POINT (798963.3 5268920)
#> 8   2.81  9.86 0.84    1    50    lhs POINT (792573.6 5271174)
#> 9   2.13  6.88 0.91    1    50    lhs POINT (799508.8 5270341)
#> 10  0.32  0.71 0.04    1    50    lhs POINT (798180.6 5276603)

#--- generate stats ---#
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

#--- boostrap statistics ---#
bootstrap_stats(data = stats, population = population, cores = cores)
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

## Specify user defined functions for statistics using `.f`

``` r
#--- function needs to have more than 1 metric to give proper outputs ---#

.f <- function(x) {
  #--- calculate the mean and percent of points less than the mean ---#
  c(
    mean = mean(x),
    perc_gr_mean = length(x[x < mean(x)]) / length(x)
  )
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
