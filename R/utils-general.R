#' Create a vector by repeating values from a list
#'
#' This function takes a list of values and repeats each value a specified number of times to create a vector.
#'
#' @param nSamp Vector of desired sample sizes
#' @param iter Desired number of iterations
#' @return A vector for each \code{nSamp} of length \code{iter}

#--- list from vector for number of samples and iterations ---#
utils_list_to_vector <- function(nSamp, iter) {
  return(unlist(lapply(nSamp, function(x) rep(x, iter))))
}
