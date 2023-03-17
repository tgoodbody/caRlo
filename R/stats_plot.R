#' Plot summary statistics
#'
#' This function creates a plot of summary statistics using ggplot2.
#'
#' @param data A data frame containing the data to be plotted.
#' @param statistics An optional vector of statistics to include in the plot.
#' @param type The type of plot to create, either "mean" (default) or "box".
#' @param population The output of \code{stats_summary(population = TRUE)}.
#' Represents population parameters. Functions used to calculate statistics
#' must be the same for a resonable output.
#' @param ... Additional arguments passed to ggplot2.
#'
#' @return A ggplot2 object representing the plot of summary statistics.
#'
#' @import ggplot2
#' @importFrom dplyr summarise
#' @export

stats_plot <- function(data, population = NULL, statistics = NULL, type = "mean", ...) {
  #--- globals ---#

  nSamp <- method <- statistic <- name <- value <- ms <- NULL

  if (!type %in% c("mean", "box")) {
    stop("Unknown plot 'type'.", call. = FALSE)
  }

  d <- data %>%
    unnest(statistics) %>%
    group_by(nSamp, method, statistic, name)

  if (!is.null(statistics)) {

    d <- d %>%
      filter(statistic %in% statistics)
  }

  if(!"mask" %in% colnames(d)){

    d$mask <- "Not specified"

  }

  if (type == "mean") {
    p <- d %>%
      group_by(mask, .add = TRUE) %>%
      summarise(
        mean = mean(value),
        stderr = sd(value) / sqrt(nSamp)
      ) %>%
      unique() %>%
      mutate(nSamp = as.factor(nSamp)) %>%
      #filter(statistic %in% ms) %>%
      ggplot(aes(nSamp, mean, ymax = mean + stderr, ymin = mean - stderr, group = interaction(method, name, mask), colour = method, fill = method)) +
      scale_color_brewer(palette="Dark2") +
      scale_fill_brewer(palette="Dark2") +
      geom_point(aes(shape = mask)) +
      geom_ribbon(alpha = 0.15, colour = NA) +
      facet_wrap(name ~ statistic, ...) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, hjust=1))
  }

  if (type == "box") {
    p <- d %>%
      mutate(nSamp = as.factor(nSamp)) %>%
      #filter(statistic %in% ms) %>%
      ggplot(aes(nSamp, value, group = interaction(nSamp, method, name), fill = method)) +
      scale_color_brewer(palette="Dark2") +
      scale_fill_brewer(palette="Dark2") +
      geom_boxplot(lwd = 0.2) +
      facet_wrap(name ~ statistic, ...) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, hjust=1))
  }


  if(!is.null(population)){

    population <- population %>%
      filter(statistic %in% statistics)

    p <- p + geom_hline(data = population, aes(yintercept = value),alpha = 0.5,lty = "dashed")

  }

  return(p)
}
