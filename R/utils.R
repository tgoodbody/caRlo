#' Sampling of Spatial Data
#'
#' This function samples `nSamp`points `iter` times using either Latin Hypercube Sampling (LHS), Simple Random Sampling (SRS), or Balanced Sampling.
#'
#' @param data A spatial dataset of class sf, SpatialPointsDataFrame, SpatialPixelsDataFrame or SpatialGridDataFrame.
#' @param nSamp The number of samples to take.
#' @param iter The iteration number.
#' @param method The method to use for sampling. Can be "lhs" for Latin Hypercube Sampling, "srs" for Simple Random Sampling, or anything else for Balanced Sampling.
#'
#' @importFrom furrr future_map2
#'
#' @return A data.frame object containing the sampled points and their attributes, as well as additional columns specifying the iteration number, the number of samples, and the sampling method used.
#' @export

utils_sample_applied <- function(data,
                         nSamp,
                         iter,
                         method){

  out <- future_map2(.x = nSamp,
                     .y = iter,
                     .f = ~utils_sample_methods(data = data,
                                              nSamp = .x,
                                              iter = .y,
                                              method = method)) %>%
    bind_rows()

  out

}

#' @importFrom sgsR sample_existing
#' @importFrom dplyr mutate slice_sample %>%
#' @importFrom sf st_coordinates st_drop_geometry st_coordinates

utils_sample_methods <- function(data,
                                 nSamp,
                                 iter,
                                 method){

  #--- determine method to use ---#

  if(method == "lhs"){

    suppressMessages(

    out <- sample_existing(existing = data, nSamp = nSamp) %>%
      mutate(iter = iter,
             nSamp = nSamp,
             method = method)

    )

  } else if(method == "srs"){

    out <- slice_sample(.data = data, n = nSamp) %>%
      mutate(iter = iter,
             nSamp = nSamp,
             method = method)

  } else if(method == "lpm"){

    out <- utils_sample_bal(data = data, nSamp = nSamp) %>%
      mutate(iter = iter,
             nSamp = nSamp,
             method = method)

  } else {

    stop("unknown sampling method provided.")

  }



  #--- extract coordinates and bind them to the sampled data ---#

  return(out)

}

#' Calculate descriptive statistics for each column in a data frame
#'
#' Given a data frame, calculates the standard error, minimum, mean, maximum, variance, interquartile range (IQR), and decile quantiles for each column.
#'
#' @param metrics A data frame with numeric columns to calculate statistics for.
#' @param population Logical defining whether \code{metrics} is a population. If \code{population = FALSE}
#' then a standard error will be calculated.
#' @return A data frame with columns for each calculated statistic, where each row represents a column in the input data frame.
#' @export
#'
#'
#' @examples
#' data(iris)
#' util_stats(iris[, 1:4])

util_stats <- function(metrics,
                       population = FALSE){

  statistic <- NULL

  #--- if population is true dont calculate standard error ---#
  if(isTRUE(population)){

    # for each column calculate the following statistics: standard error, min, mean, max, variance, IQR, 10% quantiles from 10%-90%
    result <- apply(metrics, MARGIN = 2, function(x) {
      c(min = min(x, na.rm = TRUE),
        mean = mean(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        var = var(x, na.rm = TRUE),
        IQR = IQR(x, na.rm = TRUE),
        quantile(x, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE))
    })

  } else {

    # for each column calculate the following statistics: standard error, min, mean, max, variance, IQR, 10% quantiles from 10%-90%
    result <- apply(metrics, MARGIN = 2, function(x) {
      c(SE = sd(x,na.rm = TRUE) / sqrt(length(x)),
        min = min(x, na.rm = TRUE),
        mean = mean(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        var = var(x, na.rm = TRUE),
        IQR = IQR(x, na.rm = TRUE),
        quantile(x, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE))
    })

  }


  # convert the result to a data frame
  result <- as.data.frame(result)

  #add rownames for pivoting
  result$statistic <- rownames(result)

  #resturn pivoted to long format
  result %>% tidyr::pivot_longer(-statistic)

}

#' Sample balanced subsets of a large dataset
#'
#' This function samples a balanced subset of a large dataset using the LPM2 method from the SamplingBigData package.
#' The method works by dividing the data into cells, and sampling from each cell in proportion to its size.
#' @param data A data.frame containing the dataset to be sampled
#' @param nSamp The number of samples to be taken
#' @param p A vector of probabilities to sample each row. If not provided, it defaults to uniform sampling.
#' @return A data.frame containing the sampled rows
#'
#' @importFrom SamplingBigData lpm2_kdtree
#' @export
utils_sample_bal <- function(data,
                             nSamp,
                             p = NULL)
{

  vals <- . <-  NULL

  vals_m <- data %>%
    sf::st_drop_geometry() %>%
    as.matrix(.)

  N <- nrow(vals_m)
  if (is.null(p)) {
    p <- rep(nSamp/N, N)
  } else {
    if (!is.numeric(p)) {
      stop("'p' must be type numeric.", call. = FALSE)
    }
    if (length(p) != N) {
      stop(paste0("'p' have a length of ", N, "."), call. = FALSE)
    }
  }

  sampled <- lpm2_kdtree(prob = p, x = vals_m)

  samples <- data[sampled, ]

  return(samples)
}

#' Create a vector by repeating values from a list
#'
#' This function takes a list of values and repeats each value a specified number of times to create a vector.
#'
#' @param nSamp Vector of desired sample sizes
#' @param iter Desired number of iterations
#' @return A vector for each \code{nSamp} of length \code{iter}

#--- list from vector for number of samples and iterations ---#
utils_list_to_vector <- function(nSamp, iter){

  return(unlist(lapply(nSamp, function(x) rep(x, iter))))

}
