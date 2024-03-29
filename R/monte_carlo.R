# sampling function

#' Iterate over multiple sampling methods and sample sizes
#'
#' @param data an \code{sf} of type POINT
#' @param metrics a vector of character strings that match desired column names in \code{data}
#' @param nSamp a numeric scalar greater than 0 and less than the number of rows in \code{data}
#' @param iter a numeric scalar greater than 0
#' @param ... parameters to pass to \link{sgsR::sample_ahels}{sample_ahels}
#' @inheritParams apply_methods
#'
#' @return a list of sampled datasets
#'
#' @details This function iterates over multiple sampling methods and sample sizes using \code{utils_sample()}.
#' The function assumes that users have removed coordinate columns if a \code{data.frame} has been provided.
#' Any NA values in \code{data} will be dropped.
#'
#' @importFrom dplyr bind_rows "%>%"
#' @importFrom methods is
#' @export

monte_carlo <- function(data,
                        metrics = NULL,
                        nSamp,
                        iter,
                        method = NULL,
                        cores = NULL,
                        ...) {

  # sample must be an sf or dataframe object
  if (!inherits(st_geometry(data), "sfc_POINT")) {
    stop("'data' must be an 'sf' object of type 'sfc_POINT' geometry.", call. = FALSE)
  }


  # metrics must be a vector of character strings that all match column names in sample
  if (!is.null(metrics)) {
    if (!all(metrics %in% names(data))) {
      stop("Input 'metrics' must be a vector of character strings that match column names in 'data'.")
    }

    data <- data %>%
      select({{metrics}})

  }

  # nSamp must be a numeric scalar greater than 0 and less than the number of rows in sample
  if (!any(is.numeric(nSamp) || nSamp <= 0 || nSamp >= nrow(data))) {
    stop("Input 'nSamp' must be a numeric scalar (or vector of numeric scalars) greater than 0 and less than the number of rows in 'data'.")
  }

  # iter must be a numeric scalar greater than 0
  if (!is.numeric(iter) || iter <= 0) {
    stop("Input 'iter' must be a numeric scalar greater than 0.")
  }

  # drop any NA from sample and notify the user
  if (anyNA(data)) {
    data <- data %>%
      na.omit()
    message("Dropped 'data' rows with NA")
  }

  #--- vectorize nSamp and nRep for sampling ---#
  nSamp <- utils_list_to_vector(nSamp, iter)

  iter <- rep(seq(1, iter, 1), length(nSamp) / iter)

  #--- add default sampling methods ---#
  if (is.null(method)) {
    method <- c("clhs", "balanced", "srs")
  }

  #--- apply sampling
  out <- apply_methods(data = data, nSamp = nSamp, iter = iter, method = method, cores = cores) %>%
    bind_rows()

  row.names(out) <- NULL

  return(out)
}
