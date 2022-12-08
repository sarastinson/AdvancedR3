#' Perform summary statistics
#'
#' @param data Lipidomics dataset
#'
#' @return A data.frame/tibble
descriptive_stats <- function(data) {
  data %>%
    dplyr::group_by(metabolite) %>%
    dplyr::summarise(across(value, list(
      mean = mean,
      sd = sd,
      median = median,
      iqr = IQR
    ))) %>%
    dplyr::mutate(across(tidyselect::where(is.numeric), round, digits = 1))
}

#' Make plot of gender by class
#'
#' @param data Lipidomics dataset
#'
#' @return A ggplot object
plot_count_stats <- function(data) {
  data %>%
    dplyr::distinct(code, gender, class) %>%
    ggplot2::ggplot(aes(x = class, fill = gender)) +
    ggplot2::geom_bar(position = "dodge")
}

#' Make plot of distributions by metabolites
#'
#' @param data Lipiddomics dataset
#'
#' @return A ggplot object
plot_distributions <- function(data) {
  data %>%
    ggplot2::ggplot(aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(vars(metabolite), scales = "free")
}

#' Title Metabolite values to snakecase
#'
#' @param data Lipidomics dataset.
#'
#' @return snakecase df.
metabolite_values_to_snakecase <- function(data) {
  data %>%
    dplyr::mutate(metabolite = snakecase::to_snake_case(metabolite))
}
