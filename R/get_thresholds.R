#' Get thresholds for analytics/operational parameters
#' @param file path to csv file with thresholds for Haridwar site (default:
#' \code{kwb.pilot:::shiny_file("haridwar/data/thresholds.csv")})
#' @return returns data frame thresholds for operational/analytical parameters
#' @importFrom dplyr mutate_
#' @importFrom utils read.csv
#' @export

get_thresholds <- function(file = shiny_file("haridwar/data/thresholds.csv"))
{
  read.csv(file, stringsAsFactors = FALSE) %>%
    dplyr::mutate_(
      label = "sprintf('%s %s %3.1f (%s)',
                ParameterName,
                ParameterThresholdComparison,
                ParameterThreshold,
                ParameterThresholdSource)"
    )
}
