#' Sample a data frame using different methods
#'
#' This function takes a data frame and performs sampling using three different methods:
#' latin hypercube sampling (LHS), simple random sampling (SRS),
#' and Latin pseudo-maximum sampling (LPM).
#' For each method, it applies the \code{\link{apply_sample()}} function
#' to the data frame, with the specified number of samples and iterations.
#'
#' @param data A data frame to be sampled.
#' @param nSamp The number of samples to generate.
#' @param iter The number of iterations to perform.
#' @param method The method to use for sampling. Can be "lhs" for Latin Hypercube Sampling,
#' "srs" for Simple Random Sampling, or "lpm" else for Balanced Sampling.
#'
#' @importFrom furrr future_map2
#' @importFrom furrr future_map
#' @importFrom sgsR sample_existing
#' @importFrom dplyr mutate slice_sample
#' @importFrom sf st_coordinates st_drop_geometry st_coordinates
#' @importFrom SamplingBigData lpm2_kdtree
#'
#' @return A data frame containing the sampled data using each of the three methods.
#'
#' @examples
#' # Generate some random data
#' set.seed(123)
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#'
#' # Sample the data using utils_sample()
#' sampled_data <- apply_methods(data, nSamp = 10, iter = 5)
#'
#' @export

apply_methods <- function(data, nSamp, iter, method = NULL) {
  #--- sample and return output ---#
  out <- furrr::future_map(
    .x = method,
    .f = ~ apply_sample(
      data = data,
      nSamp = nSamp,
      iter = iter,
      method = .x
    )
  ) %>%
    bind_rows()

  return(out)
}

#' @export
apply_sample <- function(data, nSamp, iter, method) {
  out <- future_map2(
    .x = nSamp,
    .y = iter,
    .f = ~ sample_methods(
      data = data,
      nSamp = .x,
      iter = .y,
      method = method
    )
  ) %>%
    bind_rows()

  return(out)
}


#' @export
sample_methods <- function(data,
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

#' @export
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
