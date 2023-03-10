#' Plot summary statistics
#'
#' This function creates a plot of summary statistics using ggplot2.
#'
#' @param data A data frame containing the data to be plotted.
#' @param statistics An optional vector of statistics to include in the plot.
#' @param type The type of plot to create, either "mean" (default) or "box".
#' @param ... Additional arguments passed to ggplot2.
#'
#' @return A ggplot2 object representing the plot of summary statistics.
#'
#' @import ggplot2
#' @importFrom dplyr summarise
#' @export

stats_plot <- function(data, statistics = NULL, type = "mean", ...) {
  #--- globals ---#

  nSamp <- method <- statistic <- name <- value <- ms <- NULL

  if (!type %in% c("mean", "box")) {
    stop("Unknown plot 'type'.", call. = FALSE)
  }

  d <- data %>%
    unnest(statistics) %>%
    group_by(nSamp, method, statistic, name)

  if (!is.null(statistics)) {
    stats <- statistics
  } else {
    stats <- unique(d$statistic)
  }

  if (type == "mean") {
    p <- d %>%
      summarise(
        mean = mean(value),
        stderr = sd(value) / sqrt(nSamp)
      ) %>%
      unique() %>%
      mutate(nSamp = as.factor(nSamp)) %>%
      #filter(statistic %in% ms) %>%
      ggplot(aes(nSamp, mean, ymax = mean + stderr, ymin = mean - stderr, group = interaction(method, name), colour = method, fill = method)) +
      geom_point() +
      geom_ribbon(alpha = 0.15, colour = NA) +
      facet_wrap(name ~ statistic, ...) +
      theme_light()
  }

  if (type == "box") {
    p <- d %>%
      mutate(nSamp = as.factor(nSamp)) %>%
      #filter(statistic %in% ms) %>%
      ggplot(aes(nSamp, value, group = interaction(nSamp, method, name), fill = method)) +
      geom_boxplot(lwd = 0.2) +
      facet_wrap(name ~ statistic, ...) +
      theme_light()
  }

  return(p)
}
