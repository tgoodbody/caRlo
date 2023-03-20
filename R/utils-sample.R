#' Sample a data frame using different methods
#'
#' This function takes a data frame and performs sampling using three different methods:
#' latin hypercube sampling (LHS), simple random sampling (SRS),
#' and Latin pseudo-maximum sampling (LPM).
#'
#' @param data A data frame to be sampled.
#' @param nSamp The number of samples to generate.
#' @param iter The number of iterations to perform.
#' @param method The method to use for sampling. Can be \code{"clhs"} for Latin Hypercube Sampling,
#' \code{"balanced"} else for Balanced Sampling, \code{"srs"} for Simple Random Sampling, or \code{"strat"} for stratified sampling.
#' If \code{method = "strat"}, \code{data} must have an attribute named \code{strata}
#' @param cores Number of cores to use for parallel computation
#' @param ... arguments to pass to \code{\link{sgsR:sample_ahels}{sgsR::sample_ahels()}}
#'
#' @keywords internal
#'
#' @inheritParams monte_carlo
#'
#' @importFrom parallel makePSOCKcluster setDefaultCluster clusterEvalQ clusterMap stopCluster
#' @importFrom sgsR sample_existing
#' @importFrom dplyr mutate slice_sample
#' @importFrom sf st_coordinates st_drop_geometry st_coordinates
#' @importFrom SamplingBigData lpm2_kdtree
#'
#' @return A data frame containing the sampled data using each of the three methods.
apply_methods <- function(data, nSamp, iter, method = NULL, cores = NULL,...) {
  if (!is.null(cores)) {
    #--- parallelize ---#
    cl <- makePSOCKcluster(cores)
    on.exit(stopCluster(cl))
    setDefaultCluster(cl)
    clusterEvalQ(NULL, environment())

    out <- clusterMap(cl, fun = apply_sample, nSamp = nSamp, iter = iter, MoreArgs = list(data = data, method = method,...))
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
#' @keywords internal
#'
#' @return A data frame containing the sampled data, the iteration number, the number of samples, and the sampling method used.
apply_sample <- function(nSamp, iter, method, data, ...) {
  mapply(FUN = stdmethods, nSamp = nSamp, iter = iter, method = method, MoreArgs = list(data = data,...), SIMPLIFY = FALSE)
}

#' Apply Sampling Method to Data
#'
#' This function applies one of three sampling methods to data: Latin Hypercube Sampling (LHS), Simple Random Sampling (SRS), or Latin Point Mass Sampling (LPM).
#'
#' @inheritParams apply_methods
#' @importFrom sgsR sample_ahels
#'
#' @keywords internal
#'
#' @return A data frame containing the sampled data, the iteration number, the number of samples, and the sampling method used.
stdmethods <- function(data,
                       nSamp,
                       iter,
                       method,
                       ...) {


  #--- determine method to use ---#

  if (method == "clhs") {
      out <- sample_existing(existing = data, nSamp = nSamp, type = "clhs") %>%
        mutate(
          iter = iter,
          nSamp = nSamp,
          method = method
        )
  } else if (method == "srs") {
    out <- sample_existing(existing = data, nSamp = nSamp, type = "srs") %>%
      mutate(
        iter = iter,
        nSamp = nSamp,
        method = method
      )
  } else if (method == "balanced") {
    out <- sample_existing(existing = data, nSamp = nSamp, type = "balanced") %>%
      mutate(
        iter = iter,
        nSamp = nSamp,
        method = method
      )
  } else if (method == "ahels"){

    out <- sample_ahels(mraster = mraster, existing = data, nSamp = nSamp, ...) %>%
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

#' Sample ahels using the 'sample_ahels' function from the sgsR package.
#'
#' @inheritParams monte_carlo_ahels
#' @return A dataframe containing the sampled ahels.
#'
#' @importFrom sgsR sample_ahels
#' @keywords internal

ahelsmethod <- function(existing, nFrac, mraster, matrices, ...){

  nExist <- NULL

  mraster <- unwrap(mraster)

  n <- unique(existing$nExist)

  existing <- existing %>%
    select(-nExist)

  if(nFrac < 1){

    out <- sample_ahels(existing = existing, mraster = mraster, nSamp = n*nFrac, matrices = matrices, ...) %>%
      mutate(nSampAhels = as.factor(as.character(nrow(.))))

  } else {

    out <- sample_ahels(existing = existing, mraster = mraster, nSamp = nFrac, matrices = matrices, ...) %>%
      mutate(nSampAhels = as.factor(as.character(nrow(.))))

  }

  return(out)

}
