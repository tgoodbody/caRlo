#' Sample ahels using the 'sample_ahels' function from the sgsR package.
#'
#' @inheritParams monte_carlo_ahels
#' @return A dataframe containing the sampled ahels.
#'
#' @importFrom sgsR sample_ahels
#' @export

ahelsmethod <- function(existing, nFrac, mraster, matrices, ...){

  mraster <- unwrap(mraster)

  n <- unique(existing$nExist)

  existing <- existing %>%
    select(-nExist)

  if(nFrac < 1){

    out <- sample_ahels(existing = existing, mraster = mraster, nSamp = n*nFrac, matrices = matrices, ...) %>%
      mutate(nSampAhels = nrow(.))

  } else {

    out <- sample_ahels(existing = existing, mraster = mraster, nSamp = nFrac, matrices = matrices, ...) %>%
      mutate(nSampAhels = nrow(.))

  }

  return(out)

}
