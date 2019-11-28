#' Get thresholds for analytics/operational parameters
#' @param csv_path path to csv file with thresholds for Haridwar site (default:
#' system.file(file.path("shiny/haridwar/data", thresholds.csv")
#' @return returns data frame thresholds for operational/analytical parameters
#' @import dplyr
#' @importFrom utils read.csv
#' @export

get_thresholds <- function(csv_path = system.file(
                           file.path(
                             "shiny/haridwar/data",
                             "thresholds.csv"
                           ),
                           package = "kwb.pilot"
                         )) {
  read.csv(
    csv_path,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate_("label" = "sprintf('%s %s %3.1f (%s)',
                ParameterName,
                ParameterThresholdComparison,
                ParameterThreshold,
                ParameterThresholdSource)")
}
