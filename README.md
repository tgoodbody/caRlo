
<!-- README.md is generated from README.Rmd. Please edit that file -->

# caRlo

<!-- badges: start -->
<!-- badges: end -->

The goal of `caRlo` is to create a simple way to implement Monte Carlo
bootstrapping approaches. The package does the following:

1.  `monte_carlo()` - Use Monte Carlo sampling to create a large number
    of samples from the population.

2.  `stats_nested()` - For each of these samples, calculate a statistic
    of interest (e.g. mean, median, variance).

3.  `bootstrap_stats()` - Use bootstrapping on each set of sample
    statistics to estimate the distribution of the statistic of interest
    for the entire population.

4.  `stats_plot()` - Visualize statistical outputs

## Overview

To install `caRlo` from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("tgoodbody/caRlo")
```

### Bootstrapping

Monte Carlo bootstrapping involves creating a large number of random
samples from the population using Monte Carlo sampling, and then
applying bootstrapping to the set of sample statistics to estimate the
distribution of the statistic of interest for the entire population.

The first step in Monte Carlo bootstrapping is to generate a large
number of random samples from the population using Monte Carlo sampling.
For example, if you are interested in estimating the mean height of a
vegetation, you could generate a large number of random samples of
height data from lidar data that acts as the population using Monte
Carlo sampling.

### Sample statistics

Next, you would calculate the mean vegetation height for each of the
random samples. This gives you a set of sample statistics representing
the mean income for each of the random samples.You can then use
bootstrapping to estimate the distribution of the mean vegetation height
for the entire population based on these sample statistics.

### Bootstrap statistics

To estimate the distribution of the mean vegetation height, you would
randomly select a subset of the sample statistics (with replacement) to
create a new “bootstrap sample”. Then, you would calculate the mean (or
any statistic of your choosing) of this bootstrap sample, and repeat
this process many times to generate a large number of bootstrap means.
These bootstrap means can be used to estimate the distribution of the
mean vegetation height for the entire population, taking into account
the variability of the sample means.

By combining Monte Carlo sampling and bootstrapping in this way, we can
get a more accurate estimate of the statistic of interest and a better
sense of its variability. This can be particularly useful in situations
where we have limited data or need to make predictions about uncertain
outcomes.

## Monte Carlo sampling for forest inventory

Lets assume we want to estimate the average height of trees over a
landscape. Luckily, we have wall-to-wall lidar data over the forest,
which is a remote sensing technology that uses laser pulses to measure
the distance between the sensor and the ground surface, vegetation, or
other objects. By analyzing the lidar data, we can estimate the height
of trees in the forest. To do that we:

1.  Choose the lidar metrics we want to sample on. For vegetation height
    the logical choice is a lidar metric like the p99 (99th percentile
    of height).

2.  Choose a sampling method (e.g. simple random sampling, latin
    hypercube sampling etc.). You can choose multiple to compare
    estimates of height too!.

3.  Perform repeated sampling at a determined sample size (`nSamp` in
    `caRlo`) for a discrete number of iterations (`iter`).

Steps 1-3 are performed using the `monte_carlo()` function. After
samples have been selected we need to:

4.  Calculate the average of the p99 metric for each sample.

5.  Bootstrap the p99 average to estimate the distribution of the
    average tree height for the entire forest.

During bootstrapping, random subsets of the sample averages (with
replacement) are used to create a new “bootstrap sample”. Then, we
calculate the mean of this bootstrap sample ( the average of averages :)
), and repeat this process many times to generate a large number of
bootstrap means. These bootstrap means can be used to estimate the
distribution of the average tree height for the entire forest, taking
into account the variability of the sample means.

### Sample size optimization

Now, let’s assume we want to estimate the average height of trees using
increasingly larger sample sizes. We can repeat the process described
above for different sample sizes, ranging from small samples to large
samples. We could start with a monte carlo sample approach for 50 where
`nSamp = 50, iter = 100`. We could then repeat the process for larger
sample sizes, `nSamp = c(50, 100, 200, 400, 800)`. Calculating
bootstrapped statistics on these samples will give us a good sense of
how the estimated average tree height changes as the sample size
increases.

By using Monte Carlo sampling and bootstrapping at different sample
sizes, we can estimate the variability of the average tree height for
each sample size, and determine how many locations we need to sample in
order to get a reliable estimate of the true average tree height for the
entire forest. This can be useful information for forest management and
conservation planning, as it can help us improved estimation methods
track changes in forest health over time.

### Example

    #--- code in parallel ---#
    cores <- 5 # number of cores to use for processing

    #--- population mean and median ---#
    population <-  data.frame(sd = 6.2, mean = 18.7)

    #--- define parameters ---#
    data <- caRlo:::plots # internal test data with 1000 plots
    nSamp <- c(50, 100, 150) # Sample sizes for monte carlo simulations
    iter <- 10 # number of monte carlo simulations per sample size

    #--- sample - generate stats - bootstrap stats ---#
    samples <- monte_carlo(data = data, nSamp = nSamp, iter = tier, cores = cores)

    #--- calculate statistics from monte carlo samples ---#
    stats <- stats_nested(data = samples, cores = cores)

    #--- plot stats from monte carlo samples with standard error ribbons for all metrics ---#
    stats_plot(data = stats, type = "mean", scales = "free")

    #--- bootstrap statistics of monte carlo samples ---#
    bootstraps <- bootstrap_stats(data = stats, population = population, cores = cores)

    #--- plot stats from monte carlo samples with standard error ribbons for all metrics ---#
    bootstrap_plot(data = bootstraps, bootstat = "mean", scales = "free")

## Specify user defined functions for statistics using `.f`

`caRlo` allows users to define the statistics they wish to calculate for
monte carlo samples. This provides flexibility in what the user wants to
compare.

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

## Visualze

``` r
#--- population mean and median ---#
population <-  data.frame(sd = 1.1, mean = 2.2)

#--- bootstrap statistics of monte carlo samples ---#
bootstraps <- bootstrap_stats(data = caRlo:::stats, population = population, cores = 5)

#--- plot bootstrap means of monte carlo samples with standard error ribbons for all metrics ---#
bootstrap_plot(data = bootstraps,bootstat = "mean", type = "mean", scales = "free")
```
