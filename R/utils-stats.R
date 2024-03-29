#' Calculate descriptive statistics for each column in a data frame
#'
#' Given a data frame, calculates the standard error, minimum, mean, maximum, variance, interquartile range (IQR), and decile quantiles for each column.
#'
#' @param data A data frame with numeric columns to calculate statistics for.
#' @param population Logical defining whether \code{metrics} is a population. If \code{population = FALSE}
#' then a standard error will be calculated.
#' @param .f User-defined function with a single parameters. See \code{stdstats()} for example syntax.
#'
#' @keywords internal
#'
#' @return A data frame with columns for each calculated statistic, where each row represents a column in the input data frame.
#' @export
apply_stats <- function(data,
                        population = FALSE,
                        .f = NULL) {
  #--- globals ---#
  statistic <- . <- NULL

  #--- apply standard stats suite ---3
  if (is.null(.f)) {
    .f <- stdstats
  }

  #--- if population is true dont calculate standard error ---#
  # for each column calculate the following statistics: standard error, min, mean, max, variance, IQR, 10% quantiles from 10%-90%
  result <- apply(data, MARGIN = 2, FUN = .f, simplify = FALSE)

  #--- convert the result to a data frame ---#
  result <- as.data.frame(result) %>%
    mutate(statistic = rownames(.)) %>%
    tidyr::pivot_longer(-statistic)

  return(result)
}

#' Compute summary statistics for a numeric vector
#'
#' This function computes standard summary statistics for a numeric vector, including minimum, mean, maximum, variance, interquartile range (IQR), and quantiles at 10% intervals between 10% and 90%.
#'
#' @param x A numeric vector for which summary statistics are to be computed.
#'
#' @importFrom moments skewness kurtosis
#'
#' @keywords internal
#'
#' @return A named numeric vector with the following components:
#' \item{min}{The minimum value of \code{x}}
#' \item{mean}{The mean value of \code{x}}
#' \item{max}{The maximum value of \code{x}}
#' \item{var}{The variance of \code{x}}
#' \item{IQR}{The interquartile range of \code{x}}
#' \item{skew}{The skewness of \code{x}}
#' \item{kurt}{The kurtosis of \code{x}}
#' \item{10%}{The 10th percentile of \code{x}}
#' \item{20%}{The 20th percentile of \code{x}}
#' \item{30%}{The 30th percentile of \code{x}}
#' \item{40%}{The 40th percentile of \code{x}}
#' \item{50%}{The 50th percentile (median) of \code{x}}
#' \item{60%}{The 60th percentile of \code{x}}
#' \item{70%}{The 70th percentile of \code{x}}
#' \item{80%}{The 80th percentile of \code{x}}
#' \item{90%}{The 90th percentile of \code{x}}
#'
#' @export

stdstats <- function(x) {
  out <- c(
    min = min(x, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    var = var(x, na.rm = TRUE),
    IQR = IQR(x, na.rm = TRUE),
    skew = skewness(x, na.rm = TRUE),
    kurt = kurtosis(x, na.rm = TRUE),
    quantile(x, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
  )

  return(out)
}
