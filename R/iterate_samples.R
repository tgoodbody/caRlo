# sampling function

#' Iterate over multiple sampling methods and sample sizes
#'
#' @param data an \code{sf} of type POINT or \code{data.frame} object
#' @param metrics a vector of character strings that match desired column names in \code{data}
#' @param nSamp a numeric scalar greater than 0 and less than the number of rows in \code{data}
#' @param iter a numeric scalar greater than 0
#'
#' @return a list of sampled datasets
#'
#' @details This function iterates over multiple sampling methods and sample sizes using \code{utils_sample()}.
#' The function assumes that users have removed coordinate columns if a \code{data.frame} has been provided.
#' Any NA values in \code{data} will be dropped.
#'
#' @examples
#' # Iterate over multiple sampling methods and sample sizes for the 'meuse' dataset
#' data("plots")
#' iterate_samples <- iterate_samples(data = plots, nSamp = 100, iter = 3)
#'
#' @importFrom furrr future_map
#' @importFrom dplyr bind_rows
#' @export

iterate_samples <- function(data,
                            metrics = NULL,
                            nSamp,
                            iter){

  # sample must be an sf or dataframe object
  if (!inherits(data, c("sf", "data.frame"))) {
    stop("Input 'data' must be an 'sf' object.", call. = FALSE)
  } else {
    if (!inherits(sf::st_geometry(data), "sfc_POINT")) {
      stop("'data' must be an 'sf' object of type 'sfc_POINT' geometry.", call. = FALSE)
    }
  }

  # metrics must be a vector of character strings that all match column names in sample
  if(!is.null(metrics)){
    if (!all(metrics %in% names(data))) {
      stop("Input 'metrics' must be a vector of character strings that all match column names in 'sample'.")
    }
  }

  # nSamp must be a numeric scalar greater than 0 and less than the number of rows in sample
  if (!is.numeric(nSamp) || nSamp <= 0 || nSamp >= nrow(data)) {
    stop("Input 'nSamp' must be a numeric scalar greater than 0 and less than the number of rows in 'sample'.")
  }

  # iter must be a numeric scalar greater than 0
  if (!is.numeric(iter) || iter <= 0) {
    stop("Input 'iter' must be a numeric scalar greater than 0.")
  }

  # drop any NA from sample and notify the user
  if (anyNA(data)) {
    message("Dropped 'sample' rows with NA")
  }

  #--- vectorize nSamp and nRep for sampling ---#
  nSamp <- utils_list_to_vector(nSamp, iter)

  iter <- rep(seq(1,iter,1),length(nSamp) / iter)

  #--- sample and return output ---#
  furrr::future_map(.x = c("lhs","srs","lpm"),
                    .f = ~utils_sample_applied(data = data,
                                               nSamp = nSamp,
                                               iter = iter,
                                               method = .x)) %>%
    bind_rows()
}
