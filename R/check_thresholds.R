#' Check thresholds
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


  thresholds$number_total <- 0
  thresholds$number_of_satisfying <- 0
  thresholds$numberOfExceedance <- 0

  thresholds$exceedanceLabel <- "No data within reporting period!"


  for (idx in seq_len(nrow(thresholds))) {
    cond1 <- df$ParameterCode == thresholds$ParameterCode[idx] & df$SiteCode == thresholds$SiteCode[idx] & !is.na(df$ParameterValue)
    cond2 <- eval(parse(text = sprintf(
      "df$ParameterValue %s %s",
      thresholds$ParameterThresholdComparisonR[idx],
      thresholds$ParameterThreshold[idx]
    )))

    condition <- cond1 & cond2

    number_total <- nrow(df[cond1, ])

    number_of_satisfying <- nrow(df[condition, ])
    number_of_exceedances <- number_total - number_of_satisfying

    thresholds$number_total[idx] <- number_total
    thresholds$number_of_satisfying[idx] <- number_of_satisfying

    if (number_total > 0) {
      thresholds$numberOfExceedance[idx] <- number_of_exceedances
      thresholds$exceedanceLabel[idx] <- sprintf(
        "%d (%2.1f %%)",
        number_of_exceedances,
        100 * number_of_exceedances / number_total
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


  thresholds <- thresholds[, c("ParameterName", "Threshold", "exceedanceLabel")]

  names(thresholds) <- c(
    "Parameter",
    "Threshold criterium",
    "Number/Percentage of non-satifying measurements"
  )

  return(thresholds)
}
