#' Helper function: get calender weeks for time period
#' @param start start of period (default: '2017-04-24')
#' @param end end of period (default: .Date())
#' @return data.frame with daily date sequence for  and corresponding calendar week
#' @importFrom lubridate ymd
#' @export

calenderweek_from_dates <- function(start = "2017-04-24",
                       end = Sys.Date()) {


dates <- seq(from = lubridate::ymd(start),
             to = lubridate::ymd(end),
             by = "days")


data.frame(date = dates,
           yearmonth =  format(dates, format = "%Y-%m"),
           jahrmonattag = format(dates, format = "%d.%m.%y"),
           year = as.numeric(format(dates, format = "%Y")),
           month = as.numeric(format(dates, format = "%m")),
           day = as.numeric(format(dates, format = "%d")),
           cw = sprintf("%02d", lubridate::isoweek(dates)),
           stringsAsFactors = FALSE)

}

#' Helper function for Berlin-S: get all calendar week files for monthy
#' @param year_month month to be imported (e.g. 2017-04')
#' @return character vector with operational filenames with all calendar weeks
#' that need to be imported for Berlin Schoenerlinde
#' @import dplyr
#' @importFrom tidyr separate
#' @export
get_monthly_data_from_calendarweeks <- function(year_month) {



cw_for_month <- calenderweek_from_dates() %>%
    dplyr::filter_(~yearmonth == year_month) %>%
    dplyr::pull("cw") %>%
    unique()



files_to_import <- tidyr::separate_(data = data.frame(files = list.files(system.file("shiny/berlin_s/data/operation",
                                                                   package = "kwb.pilot"),
                                                       pattern = ".csv"),
                                    stringsAsFactors =  FALSE),
                  col = "files",
                  into = c("a", "year", "c", "cw", "e"),
                  remove = FALSE) %>%
    dplyr::select_("files", "year", "cw") %>%
    dplyr::filter_(~cw %in% cw_for_month) %>%
    dplyr::mutate_(file_path =  "sprintf('%s/%s',
                                       system.file('shiny/berlin_s/data/operation',
                                                   package = 'kwb.pilot'),
                                       files)") %>%
    dplyr::pull("file_path")


return(files_to_import)
}

if (FALSE) {


##########################################################################################
#### Make one CSV file of Ozone_2017_BisKW_29.csv for each calendar week 17-29
##########################################################################################

old_data <- readLines(system.file("shiny/berlin_s/data/operation/Ozone_2017_BisKW_29.csv",
                                  package = "kwb.pilot"))


for (selected_cw in 17:29) {

  jahrmonattag <-  calenderweek_from_dates() %>%
    dplyr::filter(cw == selected_cw) %>%
    dplyr::pull(jahrmonattag)

  print(sprintf("Search data for calender week %d", selected_cw))
  indices <- grep(stringr::str_sub(old_data, 1, 8), pattern = to_pattern_or(jahrmonattag))


  writeLines(c(old_data[1:3], old_data[indices]),
             con = sprintf("Ozone_2017_KW_%d.csv", selected_cw) )
}

}
