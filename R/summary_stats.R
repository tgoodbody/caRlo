#' Compute Summary Statistics
#'
#' This function computes summary statistics for numeric columns in a SpatRaster or a data.frame. If a list of metrics is specified, it only computes summary statistics for those columns.
#'
#' @param data A data frame containing the data to be summarized
#' @param metrics A vector of metric names to be summarized. If `NULL`, all numeric columns will be summarized.
#' @param population A logical indicating whether the summary statistics should be calculated for the population or just the sample. Default is `FALSE`.
#'
#' @return A tibble object containing the computed summary statistics.
#'
#' @importFrom sf st_geometry st_drop_geometry
#' @importFrom terra as.data.frame
#' @examples
#' data("samples")
#'
#' # compute summary statistics for all numeric columns
#' summary_stats(samples)
#'
#' # compute summary statistics for specific columns
#' summary_stats(samples, c("zmean", "zq90"))
#'
#' @export

summary_stats <- function(data,
                          metrics = NULL,
                          population = FALSE) {
  # data must be a spatRaster or dataframe
  if (!inherits(data, c("SpatRaster", "sf", "data.frame", "tibble"))) {
    stop("`data` must be a spatRaster, sf, dataframe, or tibble object.", call. = FALSE)
  }

  # if data is a spatRaster convert it to a dataframe and drop all NA values
  if (inherits(data, "SpatRaster")) {
    data <- as.data.frame(data, na.rm = TRUE)
  }

  # if data is a spatRaster convert it to a dataframe and drop all NA values
  if (is(data, "sf")) {
    if (!inherits(st_geometry(data), "sfc_POINT")) {
      stop("'data' must be an 'sf' object of type 'sfc_POINT' geometry.", call. = FALSE)
    }

    data <- data %>%
      st_drop_geometry()
  }

  # if metrics is not null check that metrics is a vector of strings that ALL match column names in data
  # if ALL strings do not match throw an error, if all strings do match select those column names from data
  if (!is.null(metrics)) {
    if (!is.character(metrics)) {
      stop("`metrics` must be a vector of character strings.", call. = FALSE)
    }
    if (!all(metrics %in% names(data))) {
      stop("all elements of metrics must match column names in `data`.", call. = FALSE)
    }
    data <- data[, metrics]
  }

  # if any columns in data are non-numeric, throw an error
  if (any(!sapply(data, is.numeric))) {
    numeric_cols <- sapply(data, is.numeric)
    data <- data[, numeric_cols]
    message("`data` contains non-numeric columns - dropping to calculate statistics.")
  }

  result <- utils_stats(data = data, population = population)

  # return the result in long format
  return(result)
}

#' Calculate nested summary statistics
#'
#' This function takes a data frame and creates a nested data frame by grouping on the columns `nSamp`, `iter`, and `method`.
#' It then calculates summary statistics for each nested data frame using the `summary_stats` function.
#'
#' @inheritParams summary_stats
#' @inheritParams apply_methods
#'
#' @return A tibble containing the nested data frames and their corresponding summary statistics.
#'
#' @examples
#' data("samples")
#' stats_nested(samples, metrics = c("zmean", "zq90"))
#'
#' @importFrom tidyr nest
#' @importFrom dplyr select
#' @importFrom parallel parLapply
#'
#' @export

stats_nested <- function(data,
                         metrics = NULL,
                         cores = NULL) {
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
    data <- data[, c(metrics,"nSamp","iter","method")]
  }

  if(!is.null(cores)){

    out <- data %>%
      nest(data = c(-nSamp, -iter, -method))

    x <- out$data

    cl <- makePSOCKcluster(cores)
    on.exit(stopCluster(cl))
    setDefaultCluster(cl)
    clusterEvalQ(NULL, environment())

    out$statistics <- parLapply(cl = cl, X = x, fun = function(x) summary_stats(data = x))

  } else {

    out <- data %>%
      nest(data = c(-nSamp, -iter, -method)) %>%
      mutate(statistics = lapply(X = data, FUN = summary_stats))

  }

  return(out)

}

