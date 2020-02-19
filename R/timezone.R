#' Helper function: to check whether object is of type POSIXct
#'
#' @param x a vector that should be tested whether
#' @return returns TRUE if of tpye POSIXct
#' @keywords internal
is_POSIXct <- function(x) {
  inherits(x, "POSIXct")
}

#' Timezone set: sets a user defined time zone
#'
#' @param df a dataframe containing a datetime column
#' @param tz timezone (default: "UTC")
#' @param col_datetime name of the datetime column (default: "DateTime")
#' @return returns data frame with specified time zone
#' @references Check possible "tz" arguments in column "TZ*" of table
#' \url{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones} for more
#' details.
#' @export
set_timezone <- function(df, tz = "UTC", col_datetime = "DateTime") {

  ### Convert tibble into R data.frame
  df <- as.data.frame(df)
  ### Assumption: first column of dataframe always needs to date/time (i.e. POSIXct)
  if (is_POSIXct(df[, col_datetime])) {
    df[, col_datetime] <- as.character(df[, col_datetime])
    df[, col_datetime] <- as.POSIXct(df[, col_datetime], tz = tz)
  } else {
    clean_stop(
      "Column ", col_datetime, " needs to be of type DATE/TIME (POSIXct). ", 
      "Please check sheet 'xyz' of imported xls file 'xyz'!"
    )
  }
  return(df)
}

#' Timezone change: changes time zone to user defined time zone
#'
#' @param df a dataframe containing a datetime column
#' @param tz timezone (default: "UTC")
#' @param col_datetime name of the datetime column (default: "DateTime")
#' @param debug print debug messages (default: TRUE)
#' @return returns data frame with changed time zone
#' @references Check possible "tz" arguments in column "TZ*" of table
#' \url{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones} for more
#' details.
#' @importFrom lubridate with_tz
#' @export
change_timezone <- function(df, tz = "UTC", col_datetime = "DateTime",
                            debug = TRUE) {
  if (is_POSIXct(df[, col_datetime])) {
    tz_org <- paste(unique(base::attr((df[, col_datetime]), "tzone")), collapse = " , ")
    if (tz_org == tz) {
      if (debug) {
        print(sprintf(
          "Original time zone(s) %s and new time zone %s are identical",
          tz_org,
          tz
        ))
      }
    } else if (tz_org != tz) {
      if (debug) {
        print(sprintf(
          "Changing original time zone(s) %s to %s",
          tz_org,
          tz
        ))
      }
      df[, col_datetime] <- lubridate::with_tz(time = df[, col_datetime], tzone = tz)
    } else {
      clean_stop(
        "Column ", col_datetime, " needs to be of type DATE/TIME (POSIXct). ", 
        "Please check sheet 'xyz' of imported xls file 'xyz'!"
      )
    }
    return(df)
  }
}


#' Timezone: get valid time zones from Wikipedia
#' @return returns data frame valid time zones (column: TZ.) from Wikipedia
#' @references Check possible "tz" arguments in column "TZ*" of table
#' \url{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones} for more
#' details.
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 read_html
#' @export
get_valid_timezones <- function() {
  url_tz <- "https://en.wikipedia.org/wiki/List_of_tz_database_time_zones"

  res <- url_tz %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = ".//*[@id='mw-content-text']/div/table[1]") %>%
    rvest::html_table() %>%
    as.data.frame()

  return(res)
}
