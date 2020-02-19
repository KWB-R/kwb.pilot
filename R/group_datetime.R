#' Group DateTime by user defined period (year, month, day, hour, minute)
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
#' @import dplyr
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

  if ((!by %in% names(grp_list)) & !is.numeric(by)) {
    msg <- sprintf(
      "'%s' is no valid aggregation time step!\n Please select one of: %s for parameter 'by' or
                 provide a temporal aggregation interval in seconds (e.g. 600 in case of 10 minute aggregation))",
      by,
      kwb.utils::stringList(names(grp_list))
    )
    clean_stop(msg)
  } else if ((by %in% names(grp_list)) & !is.numeric(by)) {
    datetime_org <- df[, col_datetime]
    tz_org <- base::check_tzones(datetime_org)
    df[, col_datetime] <- fasttime::fastPOSIXct(
      format(
        df[, col_datetime],
        format = grp_list[[by]]
      ),
      tz = tz_org,
      required.components = 3L
    )
    if (by == "day") by <- "dai"
    if (dbg) print(sprintf("Performing %sly temporal aggregation!", by))

    df[, col_datatype] <- sprintf("%s (%sly %s)", df[, col_datatype], by, fun)
  } else {
    if (dbg) print(sprintf("Performing temporal aggregation for %d seconds time periods!", by))
    df[, col_datetime] <- xts::align.time(df[, col_datetime], n = by)
    df[, col_datatype] <- sprintf("%s (%d seconds %s)", df[, col_datatype], by, fun)
  }


  df <- df %>%
    dplyr::group_by_(.dots = dplyr::setdiff(names(df), "ParameterValue")) %>%
    dplyr::summarise_("ParameterValue" = sprintf("%s(ParameterValue)", fun)) %>%
    as.data.frame()

  return(df)
}
