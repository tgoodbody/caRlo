#' Sample a data frame using different methods
#'
#' This function takes a data frame and performs sampling using three different methods:
#' latin hypercube sampling (LHS), simple random sampling (SRS),
#' and Latin pseudo-maximum sampling (LPM).
#'
#' @param data A data frame to be sampled.
#' @param nSamp The number of samples to generate.
#' @param iter The number of iterations to perform.
#' @param method The method to use for sampling. Can be "lhs" for Latin Hypercube Sampling,
#' "srs" for Simple Random Sampling, or "lpm" else for Balanced Sampling.
#' @param cores Number of cores to use for parallel computation
#'
#' @importFrom parallel makePSOCKcluster setDefaultCluster clusterEvalQ clusterMap stopCluster
#' @importFrom sgsR sample_existing
#' @importFrom dplyr mutate slice_sample
#' @importFrom sf st_coordinates st_drop_geometry st_coordinates
#' @importFrom SamplingBigData lpm2_kdtree
#'
#' @return A data frame containing the sampled data using each of the three methods.
#'
#' @export

apply_methods <- function(data, nSamp, iter, method = NULL, cores = NULL) {
  if (!is.null(cores)) {
    #--- parallelize ---#
    cl <- makePSOCKcluster(cores)
    on.exit(stopCluster(cl))
    setDefaultCluster(cl)
    clusterEvalQ(NULL, environment())

    out <- clusterMap(cl, fun = apply_sample, nSamp = nSamp, iter = iter, MoreArgs = list(data = data, method = method))
  } else {
    out <- lapply(X = method, FUN = apply_sample, nSamp = nSamp, iter = iter, data = data)
  }

  return(out)
}

#' Apply Sampling Methods to Data
#'
#' This function applies one of three sampling methods to data: Latin Hypercube Sampling (LHS), Simple Random Sampling (SRS), or Latin Point Mass Sampling (LPM).
#'
#' @inheritParams apply_methods
#'
#' @return A data frame containing the sampled data, the iteration number, the number of samples, and the sampling method used.
#' @export
apply_sample <- function(nSamp, iter, method, data) {
  mapply(FUN = stdmethods, nSamp = nSamp, iter = iter, method = method, MoreArgs = list(data = data), SIMPLIFY = FALSE)
}

#' Apply Sampling Method to Data
#'
#' This function applies one of three sampling methods to data: Latin Hypercube Sampling (LHS), Simple Random Sampling (SRS), or Latin Point Mass Sampling (LPM).
#'
#' @inheritParams apply_methods
#' @return A data frame containing the sampled data, the iteration number, the number of samples, and the sampling method used.
#' @export
stdmethods <- function(data,
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

#' Balanced sampling
#'
#' This function performs Latin Point Mass Sampling (LPM) on data.
#'
#' @inheritParams apply_methods
#'
#' @param p probability string
#' @return A data frame containing the sampled data.
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
