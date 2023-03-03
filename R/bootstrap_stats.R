#' Sample bootstrap function
#'
#' This function performs bootstrapping on a nested dataframe with a nested column called statistics.
#'
#' @param data A nested dataframe with a nested column called statistics.
#' @param population A vector representing the population from which the bootstrap samples will be drawn.
#' @param cores An optional argument indicating the number of CPU cores to use for parallel computation. If NULL, the function runs the bootstrap sequentially.
#' @param R The number of bootstrap resamples to generate.
#'
#' @import stats
#' @import utils
#'
#' @return A nested dataframe with an additional column bootstrap containing the bootstrapped results.
#'
#'
#' @export
bootstrap_stats <- function(data,
                             population,
                             cores = NULL,
                             R = 10000) {
  iter <- statistics <- nSamp <- method <- statistic <- name <- NULL

  #### data is a nested dataframe with a nested column called  `statistics`

  # unnest statistics
  out <- data %>%
    select(-data, -iter) %>%
    tidyr::unnest(statistics) %>%
    dplyr::group_by(nSamp, method, statistic, name) %>%
    nest()

  x <- out$data

  if (!is.null(cores)) {
    cl <- makePSOCKcluster(cores)
    on.exit(stopCluster(cl))
    setDefaultCluster(cl)
    clusterEvalQ(NULL, environment())

    out$bootstrap <- clusterMap(cl = cl, fun = stdboot, x = x, MoreArgs = list(population = population, R = R))
  } else {
    out$bootstrap <- lapply(X = x, FUN = stdboot, population = population)
  }
  return(out)
}
