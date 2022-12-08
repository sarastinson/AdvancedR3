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
