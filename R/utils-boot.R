#' Standard summary
#' @inheritParams stdstats
#' @param population dataframe with population statistics
#' @param R Number of bootstrap iterations
#'
#' @keywords internal
#'
#' @importFrom boot boot
#' @importFrom broom tidy
stdboot <- function(x, population, R = 10000) {
  x <- dplyr::pull(x)

  bmed <- tidy(boot(x, boot_median, R, pop = population$median), conf.int = TRUE) %>%
    mutate(bootstrap = "median")

  bmean <- tidy(boot(x, boot_mean, R, pop = population$mean), conf.int = TRUE) %>%
    mutate(bootstrap = "mean")

  out <- bind_rows(bmed, bmean)

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
