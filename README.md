
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

    #--- fake population mean and median ---#
    population <-  data.frame(median = 1.1, mean = 2.2)

    #--- piped & parallel ---#
    cores <- 5 # number of cores
    iter <- 10 # number of monte carlo simulations per sample size

    #--- sample - generate stats - bootstrap stats ---#
    monte_carlo(caRlo:::plots, nSamp = c(50, 100, 150), iter = 10, cores = cores) %>%
      stats_nested(cores = cores) %>%
      bootstrap_stats(., population = population, cores = cores)

## Internal data for testing

``` r
#--- raw plots ALS---#
caRlo:::plots
#> Simple feature collection with 1000 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 784680.6 ymin: 5266360 xmax: 804241.6 ymax: 5283495
#> Projected CRS: ETRS89 / UTM zone 32N
#> First 10 features:
#>    zmean  zq90  lai                     geom
#> 1   3.07 10.77 0.83 POINT (801105.8 5267898)
#> 2   1.69  6.04 0.68 POINT (804221.8 5275831)
#> 3   7.05 17.64 1.79 POINT (796966.2 5267670)
#> 4   1.72  3.05 0.83 POINT (785621.8 5277216)
#> 5  16.53 27.36 3.94 POINT (802962.2 5279089)
#> 6  10.06 17.20 3.65 POINT (788651.8 5278835)
#> 7   1.80  8.48 0.54 POINT (801023.9 5269900)
#> 8   0.82  1.63 0.17 POINT (802541.7 5275059)
#> 9  12.25 18.52 3.78 POINT (790559.1 5279026)
#> 10 15.18 28.00 3.46 POINT (787220.4 5275333)

#--- raw plots GEDI ---#
caRlo:::gedi
#> Simple feature collection with 1000 features and 13 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 45.93467 ymin: -77.61382 xmax: 46.01722 ymax: -77.38342
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>        beam   shot_number degrade_flag quality_flag power.beam sensitivity
#> 1  BEAM0101 2.780541e-298            0            1          1   0.9387031
#> 2  BEAM0110 5.381020e-303           70            1          1   0.9816445
#> 3  BEAM0110 1.909406e-305            0            1          1   0.9689672
#> 4  BEAM0110 5.381020e-303           70            1          1   0.9086881
#> 5  BEAM1011 1.909532e-305            0            1          1   0.9866748
#> 6  BEAM1011 1.272330e-298           80            0          1   0.8842500
#> 7  BEAM1011 1.272330e-298           80            1          1   0.9656126
#> 8  BEAM0110 6.564673e-300           70            1          1   0.9559135
#> 9  BEAM0101 1.272203e-298           80            1          1   0.9761789
#> 10 BEAM1000 8.087216e-302            0            0          1   0.9714817
#>    solar_elevation  rh25  rh75  rh90  rh95  rh98 rh100
#> 1        -10.16813  2.47 11.91 13.63 14.53 15.16 15.88
#> 2         37.60383  9.86 21.63 27.35 30.30 33.06 35.34
#> 3         24.02323 -0.85 10.13 18.09 19.70 21.16 23.36
#> 4         37.61970 -0.70  6.12 11.47 13.04 14.12 15.20
#> 5         24.02541  0.59 18.84 24.60 26.25 27.56 29.69
#> 6         59.33067 -1.19  0.82  1.64  2.09  2.54  3.22
#> 7         59.29658 -1.27  0.97  2.09  3.59  9.24 20.82
#> 8         46.88210  0.78 26.44 28.05 28.87 29.70 31.08
#> 9         59.31870  0.18 14.15 16.44 17.78 18.87 20.33
#> 10        28.16706  8.20 23.77 30.10 32.20 33.21 33.92
#>                      geometry
#> 1  POINT (45.98721 -77.39899)
#> 2  POINT (45.97034 -77.58892)
#> 3  POINT (45.98602 -77.41941)
#> 4  POINT (45.97878 -77.61164)
#> 5  POINT (45.95877 -77.45536)
#> 6  POINT (45.95481 -77.44483)
#> 7   POINT (45.97653 -77.3873)
#> 8  POINT (45.95471 -77.40883)
#> 9    POINT (45.97055 -77.457)
#> 10 POINT (46.01096 -77.41206)

#--- samples from monte_carlo() ---#
caRlo:::samples
#> Simple feature collection with 750 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 784680.6 ymin: 5266755 xmax: 804241.6 ymax: 5283282
#> Projected CRS: ETRS89 / UTM zone 32N
#> First 10 features:
#>         zmean    zq90       lai iter nSamp method                     geom
#> 1   2.6449966  6.3050 1.3959207    1    50    lhs   POINT (798043 5277098)
#> 2   0.9800176  2.7109 0.3343221    1    50    lhs POINT (801171.6 5271106)
#> 3  14.2686011 23.6160 3.8952262    1    50    lhs POINT (789860.5 5278887)
#> 4  13.5236161 29.1280 2.2707834    1    50    lhs POINT (799364.6 5277036)
#> 5   2.3044855  8.9140 0.5687744    1    50    lhs POINT (802086.4 5268547)
#> 6   0.4800423  1.7440 0.1784513    1    50    lhs POINT (797344.9 5269644)
#> 7  18.7409489 30.2830 3.4724960    1    50    lhs POINT (802453.4 5269550)
#> 8   8.5180578 16.2600 2.7465721    1    50    lhs POINT (802408.8 5275571)
#> 9   1.7178540  4.4290 0.8616541    1    50    lhs POINT (797210.2 5270152)
#> 10 12.4680616 22.8010 2.9334627    1    50    lhs POINT (791395.2 5278454)

#--- stats from stats_nested() / stats_summary() ---#
caRlo:::stats$statistics[1]
#> [[1]]
#> # A tibble: 42 × 3
#>    statistic name  value
#>    <chr>     <chr> <dbl>
#>  1 min       zmean  0.33
#>  2 min       zq90   0.2 
#>  3 min       lai    0.06
#>  4 mean      zmean  8.81
#>  5 mean      zq90  17.7 
#>  6 mean      lai    2.31
#>  7 max       zmean 21.8 
#>  8 max       zq90  34.6 
#>  9 max       lai    6.17
#> 10 var       zmean 28.9 
#> # … with 32 more rows

#--- boostraps from bootstrap_stats() ---#
caRlo:::bootstraps$bootstrap[1]
#> [[1]]
#> # A tibble: 2 × 6
#>   statistic      bias std.error conf.low conf.high bootstrap
#>       <dbl>     <dbl>     <dbl>    <dbl>     <dbl> <chr>    
#> 1     -0.77 -0.0218      0.0594    -0.94    -0.705 median   
#> 2     -1.92  0.000256    0.0419    -2.00    -1.84  mean
```

``` r
#--- sample ---#
monte_carlo(data = caRlo:::plots, nSamp = c(50, 100, 150), iter = 3, cores = cores)
#> Simple feature collection with 2700 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 784680.6 ymin: 5266360 xmax: 804241.6 ymax: 5283495
#> Projected CRS: ETRS89 / UTM zone 32N
#> First 10 features:
#>    zmean  zq90  lai iter nSamp method                     geom
#> 1  12.86 24.98 3.43    1    50    lhs POINT (796546.9 5276522)
#> 2  11.19 16.94 4.47    1    50    lhs POINT (788407.2 5277750)
#> 3   7.71 24.30 1.48    1    50    lhs   POINT (797137 5277165)
#> 4   4.67 15.09 1.08    1    50    lhs POINT (802821.2 5275178)
#> 5  11.71 24.19 2.62    1    50    lhs POINT (802309.5 5268149)
#> 6   6.71 19.09 1.55    1    50    lhs POINT (797316.4 5268755)
#> 7  15.29 26.68 5.49    1    50    lhs POINT (792986.5 5279298)
#> 8   9.66 19.80 2.25    1    50    lhs POINT (795740.7 5277110)
#> 9   2.76  8.21 0.97    1    50    lhs POINT (786850.2 5273753)
#> 10  5.80 25.86 0.65    1    50    lhs POINT (802294.1 5276058)

#--- generate stats ---#
stats_nested(data = caRlo:::samples, cores = cores)
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

#--- summary stats for a dataset - useful for population parameters ---#
stats_summary(data = caRlo:::plots)
#> # A tibble: 42 × 3
#>    statistic name  value
#>    <chr>     <chr> <dbl>
#>  1 min       zmean  0   
#>  2 min       zq90   0   
#>  3 min       lai    0   
#>  4 mean      zmean  8.71
#>  5 mean      zq90  17.9 
#>  6 mean      lai    2.30
#>  7 max       zmean 25.6 
#>  8 max       zq90  38.3 
#>  9 max       lai    7.09
#> 10 var       zmean 28.7 
#> # … with 32 more rows

#--- boostrap statistics ---#
bootstrap_stats(data = caRlo:::stats, population = population, cores = cores)
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

s <- stats_nested(data = caRlo:::samples, cores = cores, .f = .f)

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
