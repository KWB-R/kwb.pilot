#' Helper function to get last day of month
#'
#' @param date in format as.Date()
#' @importFrom lubridate days
#' @importFrom lubridate ceiling_date
#' @return vector with last day of month(s)
#' @keywords internal
last_day <- function(date) {
  lubridate::ceiling_date(date, "month") - lubridate::days(1)
}

#' Get monthly periods
#'
#' @param year_month_start start year month (default: '2017-06')
#' @param year_month_end end year month (default: current month)
#' @param tz (default: 'CET')
#' @return dataframe with monthly periods
#' @importFrom lubridate days
#' @export
get_monthly_periods <- function(year_month_start = "2017-06",
                                year_month_end = format(Sys.Date(), "%Y-%m"),
                                tz = "CET") {
  first_day <- function(x) as.Date(paste0(x, "-01"), tz = tz)

  month_start <- seq(
    from = first_day(year_month_start),
    to = first_day(year_month_end),
    by = "month"
  )

  data.frame(
    year_month = format(month_start, format = "%Y-%m"),
    start = month_start,
    end = last_day(month_start),
    stringsAsFactors = FALSE
  )
}

#' Berlin-Tiefwerder: get rawfilepaths for months
#'
#' @param monthly_period one row of data.frame as retrieved by function
#' first row of get_monthly_periods(), i.e. year month is (default: '2017-06')
#' @param  raw_data_dir directory with operational raw data files for Berlin Tiefwerder
#' (default: \code{kwb.pilot:::shiny_file("berlin_t/data/operation")})
#' @param max_offset_days number of days in previous/next month to look for beginning/
#' ending of month (default: 7)
#' @return dataframe with monthly periods
#' @importFrom lubridate days
#' @importFrom kwb.utils stringList
#' @export
get_rawfilespaths_for_month <- function(monthly_period = get_monthly_periods()[1, ],
                                        raw_data_dir = shiny_file("berlin_t/data/operation"),
                                        max_offset_days = 7) {
  rawfiles <- stringr::str_sub(list.files(raw_data_dir), start = 1, end = 10)

  offset_days <- lubridate::days(seq_len(max_offset_days))

  get_offset <- function(type) {
    type <- match.arg(type, c("min", "max"))

    offset <- as.character(if (type == "min") {
      c(monthly_period$start - rev(offset_days))
    } else {
      c(monthly_period$end + offset_days)
    })

    if (any(available <- offset %in% rawfiles)) {
      return(ifelse(type == "min", rev, identity)(offset[available]))[1]
    }

    warning(
      sprintf("Importing period: '%s'\n", monthly_period$year_month),
      "No data for one of the following days in ",
      ifelse(type == "min", "previous", "next"),
      " month found:\n",
      kwb.utils::stringList(offset)
    )

    return(NA)
  }

  dates_to_grab <- c(
    get_offset(type = "min"),
    as.character(seq(monthly_period$start, monthly_period$end, by = "days")),
    get_offset(type = "max")
  )

  list.files(raw_data_dir, full.names = TRUE)[rawfiles %in% dates_to_grab]
}
