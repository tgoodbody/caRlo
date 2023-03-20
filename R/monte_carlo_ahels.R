#' This function performs a Monte Carlo simulation of the ahels
#' method.
#'
#' @param existing A data frame of existing samples
#' @param mraster A raster object acting as the population
#' @param nFrac Scalar to use to define the number of samples to add using ahels
#' @param matrices Population maxtrix
#' @param cores The number of cores to use for the parallel
#'     calculation.
#' @param ... Further arguments passed to the \code{\link{ahelsmethod}}
#'     function.
#' @return A nested dataframe with the ahels samples.
#' @export

monte_carlo_ahels <- function(existing, mraster, nFrac = 0.1, matrices = NULL, cores = NULL, ...){


  existing_nested <- existing %>%
    mutate(nExist = nSamp) %>%
    nest(data = c(-nSamp, -iter, -method))

  mr <- terra::wrap(mraster)

  if (!is.null(cores)) {

    cl <- makePSOCKcluster(cores)
    on.exit(stopCluster(cl))
    setDefaultCluster(cl)
    clusterEvalQ(NULL, environment())

    existing_nested$ahels <- clusterMap(cl = cl, fun = ahelsmethod, existing = existing_nested$data, MoreArgs = list(nFrac = nFrac, mraster = mr, matrices = matrices, ...))

  } else {

    if(!is.null(matrices))   matrices <- list(matrices)

    existing_nested$ahels <- mapply(existing = existing_nested$data, FUN = ahelsmethod, MoreArgs = c(nFrac = nFrac, mraster = mr, matrices = matrices, ...), SIMPLIFY = FALSE)
  }

  existing_nested <- existing_nested %>%
    select(-data)

  return(existing_nested)


}
