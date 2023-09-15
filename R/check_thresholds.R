#' Check thresholds
#'
#' @param df a dataframe as retrieved by import_data_haridwar()
#' @param thresholds thresholds dataframe as retrieved by get_thresholds()
#' (default: "raw")
#' @return dataframe with thresholds check results for selected time period (i.e.
#' whether Parameters are below/above min/max thresholds defined in dataframe
#' 'thresholds')
#' @export

check_thresholds <- function(df, # haridwar_day_list,
                             thresholds = get_thresholds()) {
  thresholds$ParameterThresholdComparisonR <- gsub(
    pattern = "^[=]",
    replacement = "==",
    thresholds$ParameterThresholdComparison
  )

  thresholds$number_total <- 0L
  thresholds$number_of_satisfying <- 0L
  thresholds$numberOfExceedance <- 0L

  thresholds$exceedanceLabel <- "No data within reporting period!"

  for (i in seq_len(nrow(thresholds))) {
    
    cond1 <- df$ParameterCode == thresholds$ParameterCode[i]
    cond1 <- cond1 & df$SiteCode == thresholds$SiteCode[i]
    cond1 <- cond1 & ! is.na(df$ParameterValue)

    cond2 <- eval(parse(text = sprintf(
      "df$ParameterValue %s %s",
      thresholds$ParameterThresholdComparisonR[i],
      thresholds$ParameterThreshold[i]
    )))

    condition <- cond1 & cond2

    n_total <- sum(cond1)
    n_satisfy <- sum(condition)
    n_exceed <- n_total - n_satisfy

    thresholds$number_total[i] <- n_total
    thresholds$number_of_satisfying[i] <- n_satisfy

    if (n_total > 0L) {
      
      thresholds$numberOfExceedance[i] <- n_exceed
      thresholds$exceedanceLabel[i] <- sprintf(
        "%d (%2.1f %%)", n_exceed, kwb.utils::percentage(n_exceed, n_total)
      )
    }
  }

  thresholds$Threshold <- sprintf(
    "%s %3.1f %s (%s)",
    thresholds$ParameterThresholdComparison,
    thresholds$ParameterThreshold,
    thresholds$ParameterUnit,
    thresholds$ParameterThresholdSource
  )

  thresholds <- thresholds[order(thresholds$ParameterName), ]

  kwb.utils::renameAndSelect(thresholds, list(
    ParameterName = "Parameter",
    Threshold = "Threshold criterium",
    exceedanceLabel = "Number/Percentage of non-satifying measurements"
  ))
}
