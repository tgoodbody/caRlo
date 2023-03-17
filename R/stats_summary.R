#' Compute Summary Statistics
#'
#' This function computes summary statistics for numeric columns in a SpatRaster or a data.frame.
#' If a vector of metric names is specified, it only computes summary statistics for those columns.
#' Rows with NA values are dropped.
#'
#' @param data A data frame containing the data to be summarized
#' @param metrics A vector of metric names to be summarized. If `NULL`, all numeric columns will be summarized.
#' @param population A logical indicating whether the summary statistics should be calculated for the population or just the sample. Default is `FALSE`.
#' @param .f Single parameter statistical function to be applied
#'
#' @return A tibble object containing the computed summary statistics.
#'
#' @importFrom sf st_geometry st_drop_geometry
#' @importFrom terra as.data.frame
#'
#' @export

stats_summary <- function(data,
                          metrics = NULL,
                          population = FALSE,
                          .f = NULL) {
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

  #--- if metrics is not null check that metrics is a vector of strings that ALL match column names in data ---#
  #--- if ALL strings do not match throw an error, if all strings do match select those column names from data ---#
   if (!is.null(metrics)) {
    if (!is.character(metrics)) {
      stop("`metrics` must be a vector of character strings.", call. = FALSE)
    }
    if (!all(metrics %in% names(data))) {
      stop("all elements of metrics must match column names in `data`.", call. = FALSE)
    }

     if(length(metrics) == 1){

       #--- if a single metric is provided make it return a data frame and add the appropriate name ---#
       data <- data.frame(data[[metrics]])
       names(data) <- metrics


     } else {

       #--- if more than 1 metric just subset the data dataframe ---#
       data <- data[[metrics]]


     }



  }

  #--- if any columns in data are non-numeric, throw an message that they are being dropped ---#
  if (any(!sapply(data, is.numeric))) {
    numeric_cols <- sapply(data, is.numeric)
    data <- data[, numeric_cols]
    message("`data` contains non-numeric columns - dropping to calculate statistics.")
  }

  #--- drop any NA from sample and notify the user ---#
  if (anyNA(data)) {
    data <-  data %>%
      na.omit()
    message("Dropped 'data' rows with NA")
  }

  result <- apply_stats(data = data, population = population, .f = .f)

  # return the result in long format
  return(result)
}
