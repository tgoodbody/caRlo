#' Standard summary
#' @inheritParams stdstats
#' @param population dataframe with population statistics
#' @param R Number of bootstrap iterations
#'
#' @keywords internal
#'
#' @importFrom boot boot
#' @importFrom broom tidy
#' @importFrom tibble tibble
stdboot <- function(x, R = 10000) {

  diff <- NULL

  # Try to compute bootstrapped median and confidence intervals
  tryCatch({
    bmed <- tidy(boot(x$samples, boot_median, R, pop = unique(x$population)), conf.int = TRUE) %>%
      mutate(bootstrap = "median",
             error = 0)
  }, error = function(e) {
    # If an error occurs, set the "conf.low" and "conf.high" columns to the median of "x$samples"
    diff <- median(x$samples) - unique(x$population)

    bmed <- tibble(statistic = diff,
                   conf.low = diff,
                   conf.high = diff,
                   bootstrap = "median",
                   error = 1)
  })

  # Try to compute bootstrapped median and confidence intervals
  tryCatch({
    bmean <- tidy(boot(x$samples, boot_mean, R, pop = unique(x$population)), conf.int = TRUE) %>%
      mutate(bootstrap = "mean",
             error = 0)
  }, error = function(e) {
    # If an error occurs, set the "conf.low" and "conf.high" columns to the mean of "x$samples"
    diff <- mean(x$samples) - mean(x$population)

    bmean <- tibble(statistic = diff,
                   conf.low = diff,
                   conf.high = diff,
                   bootstrap = "mean",
                   error = 1)
  })


  out <- bind_rows(bmed, bmean) %>%
    rename(bootstat = statistic)

  return(out)
}

#' Bootstrapped difference from population median
#'
#' This function computes the difference between the median of a given sample and the population median using bootstrapping.
#'
#' @param x A vector of numerical values.
#' @param i A vector of indices specifying the current bootstrap sample.
#' @param pop The population median to compare to.
#'
#' @keywords internal
#'
#' @return The difference between the median of the current bootstrap sample and the population median.
boot_median <- function(x, i, pop) {
  sampmed <- median(x[i])

  sampmed - pop
}

#' Bootstrapped difference from population mean
#'
#' This function computes the difference between the mean of a given sample and the population mean using bootstrapping.
#'
#' @param x A vector of numerical values.
#' @param i A vector of indices specifying the current bootstrap sample.
#' @param pop The population mean to compare to.
#'
#' @keywords internal
#'
#' @return The difference between the mean of the current bootstrap sample and the population mean.
boot_mean <- function(x, i, pop) {
  sampmed <- mean(x[i])

  sampmed - pop
}
