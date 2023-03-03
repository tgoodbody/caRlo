#' Calculate nested summary statistics
#'
#' This function takes a data frame and creates a nested data frame by grouping on the columns `nSamp`, `iter`, and `method`.
#' It then calculates summary statistics for each nested data frame using the `stats_summary` function.
#'
#' @inheritParams stats_summary
#' @inheritParams apply_methods
#'
#' @return A tibble containing the nested data frames and their corresponding summary statistics.
#'
#' @importFrom tidyr nest
#' @importFrom dplyr select
#' @importFrom parallel parLapply
#'
#' @export
stats_nested <- function(data,
                         metrics = NULL,
                         cores = NULL,
                         .f = NULL) {
  #--- globals ---#
  nSamp <- iter <- method <- NULL

  data <- data %>%
    st_drop_geometry()

  # if metrics is not null check that metrics is a vector of strings that ALL match column names in data
  # if ALL strings do not match throw an error, if all strings do match select those column names from data
  if (!is.null(metrics)) {
    if (!is.character(metrics)) {
      stop("`metrics` must be a vector of character strings.", call. = FALSE)
    }
    if (!all(metrics %in% names(data))) {
      stop("all elements of metrics must match column names in `data`.", call. = FALSE)
    }
    data <- data[, c(metrics, "nSamp", "iter", "method")]
  }

  if (!is.null(cores)) {
    out <- data %>%
      nest(data = c(-nSamp, -iter, -method))

    x <- out$data

    cl <- makePSOCKcluster(cores)
    on.exit(stopCluster(cl))
    setDefaultCluster(cl)
    clusterEvalQ(NULL, environment())

    out$statistics <- clusterMap(cl = cl, fun = stats_summary, data = x, MoreArgs = list(.f = .f, metrics = metrics))
  } else {
    out <- data %>%
      nest(data = c(-nSamp, -iter, -method)) %>%
      mutate(statistics = lapply(X = data, FUN = stats_summary, .f = .f, metrics = metrics))
  }

  return(out)
}
