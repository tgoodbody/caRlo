#' Sample bootstrap function
#'
#' This function performs bootstrapping on a nested dataframe with a nested column called statistics.
#'
#' @param data A nested dataframe with a nested column called statistics.
#' @param population A vector representing the population from which the bootstrap samples will be drawn.
#' @param cores An optional argument indicating the number of CPU cores to use for parallel computation. If NULL, the function runs the bootstrap sequentially.
#' @param R The number of bootstrap resamples to generate.
#'
#' @importFrom stats IQR var quantile sd median na.omit
#' @import utils
#' @importFrom tidyr unnest
#' @importFrom dplyr group_by filter left_join rename
#'
#' @return A nested dataframe with an additional column bootstrap containing the bootstrapped results.
#'
#'
#' @export
bootstrap_stats <- function(data,
                            population,
                            cores = NULL,
                            R = 10000) {

  iter <- statistics <- value <- nSamp <- method <- statistic <- name <- stat <- NULL

  #### data is a nested dataframe with a nested column called  `statistics`

  popnames <- unique(population$statistic)

  # unnest statistics
  out <- data %>%
    select(-data, -iter) %>%
    unnest(statistics) %>%
    group_by(nSamp, method, statistic, name)

  #--- check if statistic matches in data and population ---#

  not_in_data <- unique(population$statistic[!(population$statistic %in% out$statistic)])
  not_in_population <- unique(out$statistic[!(out$statistic %in% population$statistic)])

  not_matching <- c(not_in_data, not_in_population)

  # Print message with missing names (if any)
  if (length(not_matching) > 0) {
    m <- paste(
      "The following statistics do not match in 'data' and 'population'. Dropping:",
      paste(not_matching, collapse = ", ")
    )

    message(m)
  }

  #--- left join data and population ---#

  out <- out %>%
    rename(samples = value) %>%
    filter(statistic %in% popnames) %>%
    left_join(., y = population, by = c("statistic", "name")) %>%
    rename(population = value) %>%
    nest(data = c(samples,population))

  matched_stats <- unique(out$statistic)

  #--- check that data and population have matching names ---#
  if (nrow(out) == 0) {
    stop("'data' and 'population' must have statistics with the same names.", call. = FALSE)
  }
#
  # Check for unique values not in data frame
  missing_names <- character()
  for (stat in popnames) {
    if (stat %in% matched_stats) {
      next
    } else {
      missing_names <- c(missing_names, stat)
    }
  }



  #--- data to be bootstrapped ---#
  x <- out$data

  if (!is.null(cores)) {
    cl <- makePSOCKcluster(cores)
    on.exit(stopCluster(cl))
    setDefaultCluster(cl)
    clusterEvalQ(NULL, environment())

    out$bootstrap <- clusterMap(cl = cl, fun = stdboot, x = x, MoreArgs = list(R = R))
  } else {
    out$bootstrap <- lapply(X = x, FUN = stdboot)
  }
  return(out)
}
