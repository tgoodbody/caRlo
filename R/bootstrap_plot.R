#' Plot bootstrap statistics
#'
#' This function creates a plot of bootstrapped statistics using ggplot2.
#'
#' @param data A data frame containing the data to be plotted.
#' @param statistics An optional vector of statistics to include in the plot.
#' @param bootstat An optional vector of bootstrapped statistics to include in the plot.
#' @param type The type of plot to create, either "mean" (default) or "box".
#' @param ... Additional arguments passed to ggplot2.
#'
#' @return A ggplot2 object representing the plot of summary statistics.
#'
#' @import ggplot2
#' @importFrom dplyr summarise
#' @export

bootstrap_plot <- function(data, statistics = NULL, bootstat = NULL, type = "mean", ...) {
  #--- globals ---#

  nSamp <- std.error <- bias <- bootstrap <- method <- statistic <- name <- value <- ms <- NULL

  if (!type %in% c("mean", "bias", "box")) {
    stop("Unknown plot 'type'.", call. = FALSE)
  }

  d <- data %>%
    unnest(bootstrap) %>%
    group_by(nSamp, method, statistic, name) %>%
    select(-data)

  if (!is.null(statistics)) {
    stats <- statistics

    d <- d %>%
      filter(statistic %in% statistics)
  }

  if(!is.null(bootstat)){

    if(bootstat %in% d$bootstrap){

      d <- d %>%
        filter(bootstrap == {{bootstat}})

    } else {

      message(paste0("'",bootstat, "' not found in 'data'"), call. = FALSE)
    }
  }

  if (type == "mean") {
    p <- d %>%
      mutate(nSamp = as.factor(nSamp)) %>%
      #filter(statistic %in% ms) %>%
      ggplot(aes(nSamp, bootstat, ymax = bootstat + std.error, ymin = bootstat - std.error, group = interaction(statistic, method, name, bootstrap), colour = method, fill = method, shape = bootstrap)) +
      scale_color_brewer(palette="Dark2") +
      scale_fill_brewer(palette="Dark2") +
      geom_ribbon(alpha = 0.35, colour = NA) +
      geom_point() +
      facet_wrap(name ~ statistic, ...) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, hjust=1))
  }

  if (type == "bias") {
    p <- d %>%
      mutate(nSamp = as.factor(nSamp)) %>%
      #filter(statistic %in% ms) %>%
      ggplot(aes(nSamp, bias, group = interaction(statistic, method, name, bootstrap), colour = method, fill = method, shape = bootstrap)) +
      scale_color_brewer(palette="Dark2") +
      scale_fill_brewer(palette="Dark2") +
      geom_point() +
      geom_line() +
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



  return(p)
}
