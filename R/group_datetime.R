#' Group DateTime by user defined period (year, month, day, hour, minute)
#'
#' @param df a data frame as retrieved by import_data_haridwar()
#' @param by an aggregation time step in seconds (default: 600 seconds) for intra-
#' day aggregation or "day", "month" or "year" for longer time spans
#' @param fun function to be used for grouping measurement data of column ParameterValue
#' (default: stats::median)
#' (default: kwb.pilot:::package_file("shiny/haridwar/.my.cnf"))
#' @param col_datetime column name of datetime column (default: DateTime)
#' @param col_datatype column name of data type column (default: DataType)
#' @param dbg print debug information
#' @return returns data frame with data aggregated according to user defined
#' aggregation time step
#' @importFrom dplyr across group_by setdiff summarise
#' @importFrom xts align.time
#' @importFrom fasttime fastPOSIXct
#' @importFrom kwb.utils stringList
#' @export

group_datetime <- function(df,
                           by = 600,
                           fun = "stats::median",
                           col_datetime = "DateTime",
                           col_datatype = "DataType",
                           dbg = TRUE) {
  if (is.character(by)) {
    by <- tolower(by)
  }

  grp_list <- list(
    year = "%Y-01-01 00:00:00",
    month = "%Y-%m-01 00:00:00",
    day = "%Y-%m-%d 00:00:00"
  )

  if (!is.numeric(by) && !by %in% names(grp_list)) {
    clean_stop(
      sprintf("'%s' is no valid aggregation time step!\n", by),
      "Please select one of: ", kwb.utils::stringList(names(grp_list)),
      " for parameter 'by' or provide a temporal aggregation interval in ",
      "seconds (e.g. 600 in case of 10 minute aggregation)."
    )
  }

  # Extract the time column
  timestamps <- kwb.utils::selectColumns(df, col_datetime)

  if (!is.numeric(by)) {
    timely <- if (by == "day") "daily" else paste0(by, "ly")

    text <- paste("Performing", timely, "temporal aggregation!")

    times <- fasttime::fastPOSIXct(
      format(timestamps, format = grp_list[[by]]),
      tz = base::check_tzones(timestamps),
      required.components = 3L
    )

    descriptions <- sprintf("%s %s", timely, fun)
  } else {
    text <- paste(
      "Performing temporal aggregation for", by, "seconds time periods!"
    )

    times <- xts::align.time(timestamps, n = by) - by / 2

    descriptions <- sprintf("%d seconds %s", by, fun)
  }

  # Extract the type column
  types <- kwb.utils::selectColumns(df, col_datatype)

  # Overwrite original columns with new time objects and type labels
  df[, col_datetime] <- times
  df[, col_datatype] <- sprintf("%s (%s)", types, descriptions)

  kwb.utils::catAndRun(text, dbg = dbg, expr = {
    df %>%
      dplyr::group_by(dplyr::across(.cols = dplyr::setdiff(names(df), 
                                                           "ParameterValue"))
                      ) %>%
      dplyr::summarise(ParameterValue = eval(parse(text = paste0(fun, 
                                                                 "(.data$ParameterValue)"))),
                       .groups = "keep") %>%
      as.data.frame()
  })
}
