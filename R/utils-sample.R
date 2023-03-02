#' Sample a data frame using different methods
#'
#' This function takes a data frame and performs sampling using three different methods:
#' latin hypercube sampling (LHS), simple random sampling (SRS),
#' and Latin pseudo-maximum sampling (LPM).
#' For each method, it applies the \code{\link{utils_apply_sample()}} function
#' to the data frame, with the specified number of samples and iterations.
#'
#' @param data A data frame to be sampled.
#' @param nSamp The number of samples to generate.
#' @param iter The number of iterations to perform.
#' @param method The method to use for sampling. Can be "lhs" for Latin Hypercube Sampling,
#' "srs" for Simple Random Sampling, or "lpm" else for Balanced Sampling.
#'
#' @importFrom furrr future_map
#' @return A data frame containing the sampled data using each of the three methods.
#'
#' @examples
#' # Generate some random data
#' set.seed(123)
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#'
#' # Sample the data using utils_sample()
#' sampled_data <- utils_sample(data, nSamp = 10, iter = 5)
#'
#' @export

utils_sample <- function(data, nSamp, iter, method = NULL) {
  #--- sample and return output ---#
  out <- future_map(
    .x = method,
    .f = ~utils_apply_sample(
      data = data,
      nSamp = nSamp,
      iter = iter,
      method = .x
    )
  ) %>%
    bind_rows()

  return(out)
}

#' Sampling of Spatial Data
#'
#' This function samples `nSamp`points `iter` times using either Latin Hypercube Sampling (LHS), Simple Random Sampling (SRS), or Balanced Sampling.
#'
#' @inheritParams utils_sample
#' @importFrom furrr future_map2
#'
#' @return A data.frame object containing the sampled points and their attributes, as well as additional columns specifying the iteration number, the number of samples, and the sampling method used.
#' @export

utils_apply_sample <- function(data,
                                 nSamp,
                                 iter,
                                 method) {

  out <- furrr::future_map2(
    .x = nSamp,
    .y = iter,
    .f = ~ utils_sample_methods(
      data = data,
      nSamp = .x,
      iter = .y,
      method = method
    )
  ) %>%
    bind_rows()

  return(out)
}

#' @inheritParams utils_sample
#' @importFrom sgsR sample_existing
#' @importFrom dplyr mutate slice_sample %>%
#' @importFrom sf st_coordinates st_drop_geometry st_coordinates

utils_sample_methods <- function(data,
                                 nSamp,
                                 iter,
                                 method) {
  #--- determine method to use ---#

  if (method == "lhs") {
    suppressMessages(
      out <- sample_existing(existing = data, nSamp = nSamp) %>%
        mutate(
          iter = iter,
          nSamp = nSamp,
          method = method
        )
    )
  } else if (method == "srs") {
    out <- slice_sample(.data = data, n = nSamp) %>%
      mutate(
        iter = iter,
        nSamp = nSamp,
        method = method
      )
  } else if (method == "lpm") {
    out <- sample_balanced(data = data, nSamp = nSamp) %>%
      mutate(
        iter = iter,
        nSamp = nSamp,
        method = method
      )
  } else {
    stop("unknown sampling method provided.")
  }



  #--- extract coordinates and bind them to the sampled data ---#

  return(out)
}

#' Sample balanced subsets of a large dataset
#'
#' This function samples a balanced subset of a large dataset using the LPM2 method from the SamplingBigData package.
#' The method works by dividing the data into cells, and sampling from each cell in proportion to its size.
#' @inheritParams utils_sample
#' @param p A vector of probabilities to sample each row. If not provided, it defaults to uniform sampling.
#' @return A data.frame containing the sampled rows
#'
#' @importFrom SamplingBigData lpm2_kdtree

sample_balanced <- function(data,
                            nSamp,
                            p = NULL) {
  vals <- . <- NULL

  vals_m <- data %>%
    sf::st_drop_geometry() %>%
    as.matrix(.)

  N <- nrow(vals_m)
  if (is.null(p)) {
    p <- rep(nSamp / N, N)
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
