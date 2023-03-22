#' Standard summary
#' @inheritParams stdstats
#' @param population dataframe with population statistics
#' @param R Number of bootstrap iterations
#'
#' @keywords internal
#'
#' @return Tibble with the bootstrapped statistic, bias, std.error,
#' conf.low, conf/high, bootstrap statistic, whether no varion resulting in an error '1 or 0',
#' and the bootstrap data 't'.
#'
#' @importFrom boot boot
#' @importFrom broom tidy
#' @importFrom tibble tibble
stdboot <- function(x, R = 10000) {

  diff <- bdiffmed <- bdiffmean <- bmed <- bmean <- statistic <- NULL

  #--- differences between population ---#
  # Try to compute bootstrapped median and confidence intervals
  tryCatch({

    b <- boot(x$samples, boot_diffmedian, R, pop = unique(x$population))

    t <- list(round(b$t, digits = 3))

    bdiffmed <- tidy(b, conf.int = TRUE) %>%
      mutate(bootstrap = "diffmed",
             error = 0,
             t = t)
  }, error = function(e) {
    # If an error occurs, set the "conf.low" and "conf.high" columns to the median of "x$samples"
    diff <- median(x$samples) - unique(x$population)

    bdiffmed <- tibble(statistic = diff,
                   conf.low = diff,
                   conf.high = diff,
                   bootstrap = "diffmed",
                   error = 1,
                   t = t)
  })

  # Try to compute bootstrapped median and confidence intervals
  tryCatch({

    b <- boot(x$samples, boot_diffmean, R, pop = unique(x$population))

    t <- list(round(b$t, digits = 3))

    bdiffmean <- tidy(b, conf.int = TRUE) %>%
      mutate(bootstrap = "diffmean",
             error = 0,
             t = t)
  }, error = function(e) {
    # If an error occurs, set the "conf.low" and "conf.high" columns to the mean of "x$samples"
    diff <- mean(x$samples) - mean(x$population)

    bdiffmean <- tibble(statistic = diff,
                   conf.low = diff,
                   conf.high = diff,
                   bootstrap = "diffmean",
                   error = 1,
                   t = t)
  })

  #--- median and mean bootstraps ---#
  # Try to compute bootstrapped median and confidence intervals
  tryCatch({

    b <- boot(x$samples, boot_median, R)

    t <- list(round(b$t, digits = 3))

    bmed <- tidy(b, conf.int = TRUE) %>%
      mutate(bootstrap = "median",
             error = 0,
             t = t)
  }, error = function(e) {
    # If an error occurs, set the "conf.low" and "conf.high" columns to the median of "x$samples"
    smed <- median(x$samples)

    bmed <- tibble(statistic = smed,
                   conf.low = smed,
                   conf.high = smed,
                   bootstrap = "median",
                   error = 1,
                   t = t)
  })

  # Try to compute bootstrapped median and confidence intervals
  tryCatch({

    b <- boot(x$samples, boot_mean, R)

    t <- list(round(b$t, digits = 3))

    bmean <- tidy(b, conf.int = TRUE) %>%
      mutate(bootstrap = "mean",
             error = 0,
             t = t)
  }, error = function(e) {
    # If an error occurs, set the "conf.low" and "conf.high" columns to the mean of "x$samples"
    smean <- mean(x$samples)

    bmean <- tibble(statistic = smean,
                    conf.low = smean,
                    conf.high = smean,
                    bootstrap = "mean",
                    error = 1,
                    t = t)
  })


  out <- bind_rows(bdiffmed, bdiffmean, bmed, bmean) %>%
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
boot_diffmedian <- function(x, i, pop) {
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
boot_diffmean <- function(x, i, pop) {
  sampmed <- mean(x[i])

  sampmed - pop
}

#' Bootstrapped median
#'
#' This function computes the bootstrapped median of a given sample.
#'
#' @param x A vector of numerical values.
#' @param i A vector of indices specifying the current bootstrap sample.
#'
#' @keywords internal
#'
#' @return The bootstrapped median of a sample.
boot_median <- function(x, i) {
  median(x[i])

}

#' Bootstrapped mean
#'
#' This function computes the bootstrapped mean of a given sample.
#'
#' @param x A vector of numerical values.
#' @param i A vector of indices specifying the current bootstrap sample.
#'
#' @keywords internal
#'
#' @return The bootstrapped mean of a sample.
boot_mean <- function(x, i) {
  mean(x[i])
}
