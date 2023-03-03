#' Sample bootstrap function
#'
#' This function performs bootstrapping on a nested dataframe with a nested column called statistics.
#'
#' @param data A nested dataframe with a nested column called statistics.
#' @param population A vector representing the population from which the bootstrap samples will be drawn.
#' @param cores An optional argument indicating the number of CPU cores to use for parallel computation. If NULL, the function runs the bootstrap sequentially.
#' @param R The number of bootstrap resamples to generate.
#'
#' @importFrom stats IQR var quantile
#' @import utils
#' @importFrom tidyr unnest
#' @importFrom dplyr group_by filter
#'
#' @return A nested dataframe with an additional column bootstrap containing the bootstrapped results.
#'
#'
#' @export
bootstrap_stats <- function(data,
                            population,
                            cores = NULL,
                            R = 10000) {
  iter <- statistics <- nSamp <- method <- statistic <- name <- stat <- NULL

  #### data is a nested dataframe with a nested column called  `statistics`

  popnames <- names(population)

  # unnest statistics
  out <- data %>%
    select(-data, -iter) %>%
    unnest(statistics) %>%
    group_by(nSamp, method, statistic, name) %>%
    nest() %>%
    filter(statistic %in% popnames)

  #--- check that data and population have matching names ---#
  if(nrow(out) == 0){
    stop("'data' and 'population' must have statistics with the same names.", call. = FALSE)

  }

  stat_names <- unique(out$statistic)


  # Check for unique values not in data frame
  missing_names <- character()
  for (stat in popnames) {
    if (stat %in% stat_names) {
      next
    } else {
      missing_names <- c(missing_names, stat)
    }
  }

  # Print message with missing names (if any)
  if (length(missing_names) > 0) {

    m <- paste("The following statistics do not match in 'data' and 'population. Dropping:",
               paste(missing_names, collapse = ", "))

    message(m)
  }

  #--- data to be bootstrapped ---#
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
