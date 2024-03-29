#' This function performs a Monte Carlo simulation of the ahels
#' method.
#'
#' @param existing A data frame of existing samples
#' @param mraster A raster object acting as the population
#' @param matrices Population maxtrix
#' @param cores The number of cores to use for the parallel
#'     calculation.
#' @param ... Further arguments passed to the \code{\link{ahelsmethod}}
#'     function.
#' @return A nested dataframe with the ahels samples.
#'
#' @importFrom terra wrap unwrap
#'
#' @export

monte_carlo_ahels <- function(existing, mraster, matrices = NULL, cores = NULL, ...){

  nSamp <- iter <- method <- ahels <- NULL

  existing_nested <- existing %>%
    mutate(nExist = nSamp) %>%
    nest(data = c(-nSamp, -iter, -method))

  mr <- wrap(mraster)

  if (!is.null(cores)) {

    cl <- makePSOCKcluster(cores)
    on.exit(stopCluster(cl))
    setDefaultCluster(cl)
    clusterEvalQ(NULL, environment())

    existing_nested$ahels <- clusterMap(cl = cl, fun = ahelsmethod, existing = existing_nested$data, MoreArgs = list(mraster = mr, matrices = matrices, ...))

  } else {

    if(!is.null(matrices))   matrices <- list(matrices)

    existing_nested$ahels <- mapply(existing = existing_nested$data, FUN = ahelsmethod, MoreArgs = c(mraster = mr, matrices = matrices, ...), SIMPLIFY = FALSE)
  }

  existing_nested <- existing_nested %>%
    select(-data) %>%
    unnest(cols = ahels)

  return(existing_nested)


}
