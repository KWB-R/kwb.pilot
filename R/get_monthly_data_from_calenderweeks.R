#' Helper function: get calender weeks for time period
#' @param start start of period (default: '2017-04-24')
#' @param end end of period (default: .Date())
#' @return data.frame with daily date sequence for  and corresponding calendar week
#' @importFrom lubridate ymd
#' @importFrom kwb.utils noFactorDataFrame
#' @export

calenderweek_from_dates <- function(start = "2017-04-24", end = Sys.Date())
{
  dates <- seq(
    from = lubridate::ymd(start),
    to = lubridate::ymd(end),
    by = "days"
  )

  kwb.utils::noFactorDataFrame(
    date = dates,
    yearmonth = format(dates, format = "%Y-%m"),
    jahrmonattag = format(dates, format = "%d.%m.%y"),
    year = as.numeric(format(dates, format = "%Y")),
    month = as.numeric(format(dates, format = "%m")),
    day = as.numeric(format(dates, format = "%d")),
    cw = sprintf("%02d", lubridate::isoweek(dates))
  )
}

#' Helper function for Berlin-S: get all calendar week files for monthy
#' @param year_month month to be imported (e.g. 2017-04')
#' @return character vector with operational filenames with all calendar weeks
#' that need to be imported for Berlin Schoenerlinde
#' @importFrom dplyr filter_ pull
#' @importFrom kwb.utils noFactorDataFrame
#' @importFrom tidyr separate
#' @export
get_monthly_data_from_calendarweeks <- function(year_month)
{
  cw_for_month <- calenderweek_from_dates() %>%
    dplyr::filter_(~ yearmonth == year_month) %>%
    dplyr::pull("cw") %>%
    unique()

  dir_operation <- shiny_file("berlin_s/data/operation")

  files <- list.files(dir_operation, pattern = ".csv")

  files_to_import <- tidyr::separate_(
    data = kwb.utils::noFactorDataFrame(files = files),
    col = "files",
    into = c("a", "year", "c", "cw", "e"),
    remove = FALSE
  ) %>%
    dplyr::select_("files", "year", "cw") %>%
    dplyr::filter_(~ cw %in% cw_for_month) %>%
    dplyr::pull("files")

  sprintf("%s/%s", dir_operation, files_to_import)
}

if (FALSE)
{
  ##############################################################################
  #### Make one CSV file of Ozone_2017_BisKW_29.csv for each calendar week 17-29
  ##############################################################################
  shiny_file <- getFromNamespace("shiny_file", ns = "kwb.pilot")

  old_data <- readLines(shiny_file(
    "berlin_s/data/operation/Ozone_2017_BisKW_29.csv"
  ))

  date_strings <- stringr::str_sub(old_data, 1, 8)

  for (week in 17:29) {
    
    pattern <- calenderweek_from_dates() %>%
      dplyr::filter(cw == week) %>%
      dplyr::pull(jahrmonattag) %>%
      to_pattern_or()

    kwb.utils::catAndRun(
      sprintf("Search and rewrite data for calender week %d", week),
      writeLines(
        text = c(old_data[1:3], old_data[grep(pattern, date_strings)]),
        con = sprintf("Ozone_2017_KW_%d.csv", week)
      )
    )
  }
}
