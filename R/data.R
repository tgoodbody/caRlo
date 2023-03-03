#' GEDI data
#'
#' This is an `sf` object containing the spatial data for 1000 GEDI samples in a study area.
#'
#' @format `sf` object.
#' @source This data was collected by the GEDI mission
#' @keywords datasets
"gedi"

#' Plots data
#'
#' This is an `sf` object containing the spatial data for 1000 plots with ALS metrics.
#'
#' @format `sf` object.
#' @keywords datasets
"plots"

#' Monte Carlo samples
#'
#' This is an `tibble` of monte carlo samples from the `monte_carlo()` function.
#'
#' @format `sf` object.
#' @keywords datasets
"samples"

#' Statistics of samples
#'
#' This is a nested `tibble` of statistics for `samples`
#'
#' @format `sf` object.
#' @keywords datasets
"stats"

#' Bootstraps of samples
#'
#' This is a nested `tibble` of bootstraps of statistics for `stats`
#'
#' @format `sf` object.
#' @keywords datasets
"bootstraps"
