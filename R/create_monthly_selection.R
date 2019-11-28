#' Create monthly selection
#' @param startDate (default: '2016-09-01')
#' @param endDate (default: Sys.Date())
#' (default: "raw")
#' @return dataframe with first/last day for each month between 'startDate' and
#' 'endDate' month including a column 'label' (used in shiny app for month
#' selection)
#' @importFrom lubridate days_in_month
#' @export

create_monthly_selection <- function(startDate = "2016-09-01",
                                     endDate = Sys.Date()) {
  startDate <- as.Date(startDate)

  endMonth <- as.Date(sprintf(
    "%s-01",
    format(endDate, "%Y-%m")
  ))



  df <- data.frame(start = seq(startDate, endMonth, by = "1 month"))


  days_in_months <- lubridate::days_in_month(df$start)

  df$end <- as.Date(sprintf(
    "%s-%s",
    format(df$start, "%Y-%m"),
    days_in_months
  ))

  df$label <- format(df$start, "%B %Y")

  return(df)
}
