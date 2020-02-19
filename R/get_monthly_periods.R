#' Helper function to get last day of month
#' @param date in format as.Date()
#' @importFrom lubridate days
#' @importFrom lubridate ceiling_date
#' @return vector with last day of month(s)
#' @keywords internal
last_day <- function(date) {
  lubridate::ceiling_date(date, "month") - lubridate::days(1)
}


#' Get monthly periods
#' @param year_month_start start year month (default: '2017-06')
#' @param year_month_end end year month (default: current month)
#' @param tz (default: 'CET')
#' @return dataframe with monthly periods
#' @importFrom lubridate days
#' @export
get_monthly_periods <- function(year_month_start = "2017-06",
                                year_month_end = format(Sys.Date(), "%Y-%m"),
                                tz = "CET") {
  ymd_start <- as.Date(sprintf("%s-01", year_month_start), tz = tz)
  ymd_end <- as.Date(sprintf("%s-01", year_month_end), tz = tz)


  month_start <- seq(from = ymd_start, to = ymd_end, by = "month")
  month_end <- last_day(month_start)


  data.frame(
    year_month = format(month_start, format = "%Y-%m"),
    start = month_start,
    end = month_end,
    stringsAsFactors = FALSE
  )
}


#' Berlin-Tiefwerder: get rawfilepaths for months
#' @param monthly_period one row of data.frame as retrieved by function
#' first row of get_monthly_periods(), i.e. year month is (default: '2017-06')
#' @param  raw_data_dir directory with operational raw data files for Berlin Tiefwerder
#' (default: \code{kwb.pilot:::package_file("shiny/berlin_t/data/operation")})
#' @param max_offset_days number of days in previous/next month to look for beginning/
#' ending of month (default: 7)
#' @return dataframe with monthly periods
#' @importFrom lubridate days
#' @importFrom kwb.utils stringList
#' @export
get_rawfilespaths_for_month <- function(
  monthly_period = get_monthly_periods()[1, ],
  raw_data_dir = package_file("shiny/berlin_t/data/operation"),
  max_offset_days = 7
)
{
  rawfiles <- stringr::str_sub(string = list.files(raw_data_dir), start = 1, end = 10)


  offset_days <- lubridate::days(seq_len(max_offset_days))


  min_offset <- as.character(c(monthly_period$start - rev(offset_days)))
  min_offset_bool <- min_offset %in% rawfiles

  if (any(min_offset_bool)) {
    min_offset <- rev(min_offset[min_offset_bool])[1]
  } else {
    warning(sprintf(
      "Importing period: '%s'
                  No data for one of the following days in previous month found:\n%s",
      monthly_period$year_month,
      kwb.utils::stringList(min_offset)
    ))
    min_offset <- NA
  }

  max_offset <- as.character(c(monthly_period$end + offset_days))
  max_offset_bool <- max_offset %in% rawfiles

  if (any(max_offset_bool)) {
    max_offset <- max_offset[max_offset_bool][1]
  } else {
    warning(sprintf(
      "Importing period: '%s'
                  No data for one of the following days in next month found:\n%s",
      monthly_period$year_month,
      kwb.utils::stringList(max_offset)
    ))
    max_offset <- NA
  }

  dates_to_grap <- c(
    min_offset,
    as.character(seq(
      monthly_period$start,
      monthly_period$end,
      by = "days"
    )),
    max_offset
  )

  grap_indices <- rawfiles %in% dates_to_grap

  files_for_month <- list.files(raw_data_dir, full.names = TRUE)[grap_indices]

  return(files_for_month)
}
