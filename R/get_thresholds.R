#' Get thresholds for analytics/operational parameters
#' @param file path to csv file with thresholds for Haridwar site (default:
#' \code{kwb.pilot:::package_file("shiny/haridwar/data/thresholds.csv")})
#' @return returns data frame thresholds for operational/analytical parameters
#' @import dplyr
#' @importFrom utils read.csv
#' @export

get_thresholds <- function(file = package_file("shiny/haridwar/data/thresholds.csv")) {
  read.csv(file, stringsAsFactors = FALSE) %>%
    dplyr::mutate_(
      "label" = "sprintf('%s %s %3.1f (%s)',
                ParameterName,
                ParameterThresholdComparison,
                ParameterThreshold,
                ParameterThresholdSource)"
    )
}
