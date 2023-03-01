
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

``` r
library(GEDIsamp)

#--- load internal data ---#

data("plots")

#--- take 50 samples, 5 times for latin hypercube, simple random, balanced sampling methods ---#
samples <- iterate_samples(plots, nSamp = 50, iter = 5)
#> Sub-sampling based on ALL 'existing' metric distributions. Ensure only attributes of interest are included.
#> Sub-sampling based on ALL 'existing' metric distributions. Ensure only attributes of interest are included.
#> Sub-sampling based on ALL 'existing' metric distributions. Ensure only attributes of interest are included.
#> Sub-sampling based on ALL 'existing' metric distributions. Ensure only attributes of interest are included.
#> Sub-sampling based on ALL 'existing' metric distributions. Ensure only attributes of interest are included.
#> Warning: UNRELIABLE VALUE: Future ('<none>') unexpectedly generated random
#> numbers without specifying argument 'seed'. There is a risk that those random
#> numbers are not statistically sound and the overall results might be invalid.
#> To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe random
#> numbers are produced via the L'Ecuyer-CMRG method. To disable this check, use
#> 'seed=NULL', or set option 'future.rng.onMisuse' to "ignore".
#> Warning: UNRELIABLE VALUE: Future ('<none>') unexpectedly generated random
#> numbers without specifying argument 'seed'. There is a risk that those random
#> numbers are not statistically sound and the overall results might be invalid.
#> To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe random
#> numbers are produced via the L'Ecuyer-CMRG method. To disable this check, use
#> 'seed=NULL', or set option 'future.rng.onMisuse' to "ignore".
#> Warning: UNRELIABLE VALUE: Future ('<none>') unexpectedly generated random
#> numbers without specifying argument 'seed'. There is a risk that those random
#> numbers are not statistically sound and the overall results might be invalid.
#> To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe random
#> numbers are produced via the L'Ecuyer-CMRG method. To disable this check, use
#> 'seed=NULL', or set option 'future.rng.onMisuse' to "ignore".

#--- calculate statistics ---#
nested_stats <- stats_nested(samples)

#--- unnested stats ---#
nested_stats %>%
  tidyr::unnest(statistics)
#> # A tibble: 675 × 7
#>     iter nSamp method data              statistic name   value
#>    <dbl> <dbl> <chr>  <list>            <chr>     <chr>  <dbl>
#>  1     1    50 lhs    <tibble [50 × 3]> SE        zmean  0.769
#>  2     1    50 lhs    <tibble [50 × 3]> SE        zq90   1.14 
#>  3     1    50 lhs    <tibble [50 × 3]> SE        lai    0.189
#>  4     1    50 lhs    <tibble [50 × 3]> min       zmean  0.443
#>  5     1    50 lhs    <tibble [50 × 3]> min       zq90   1.23 
#>  6     1    50 lhs    <tibble [50 × 3]> min       lai    0.125
#>  7     1    50 lhs    <tibble [50 × 3]> mean      zmean  8.98 
#>  8     1    50 lhs    <tibble [50 × 3]> mean      zq90  17.9  
#>  9     1    50 lhs    <tibble [50 × 3]> mean      lai    2.36 
#> 10     1    50 lhs    <tibble [50 × 3]> max       zmean 21.8  
#> # … with 665 more rows
```
