#' Create monthly selection
#' @param startDate (default: '2016-09-01')
#' @param endDate (default: Sys.Date())
#' (default: "raw")
#' @return dataframe with first/last day for each month between 'startDate' and
#' 'endDate' month including a column 'label' (used in shiny app for month
#' selection)
#' @importFrom lubridate days_in_month
#' @export

create_monthly_selection <- function(
  startDate = "2016-09-01", endDate = Sys.Date()
)
{
  ym <- function(x) format(x, "%Y-%m")
  
  endMonth <- as.Date(sprintf("%s-01", ym(endDate)))

  start <- seq(as.Date(startDate), endMonth, by = "1 month")
  
  end <- as.Date(sprintf("%s-%s", ym(start), lubridate::days_in_month(start)))

  data.frame(
    start = start, 
    end = end, 
    label = format(start, "%B %Y"),
    stringsAsFactors = FALSE
  )
}
