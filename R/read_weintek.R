#' Read Weintek data from single file
#'
#' @param path path to Weintek file
#' @param tz time zone (default: CET) the measurements are taken (passed to
#' function kwb.pilot:::set_timezone())
#' @param dbg debug (default: TRUE)
#'
#' @return data frame with Weintek raw data
#' @export
#'
read_weintek <- function(path, tz = "CET", dbg = TRUE) {
  if (dbg) {
    message("Importing file: ", path)
  }

  df <- kwb.utils::renameColumns(readxl::read_xlsx(path), renamings = list(
    Datum = "DateTime",
    Date = "DateTime",
    Time = "time",
    Zeit = "time",
    Millisekunde = "Millisecond",
    "32-bit Float" = "ParameterValue"
  ))

  columns <- intersect(names(df), c("DateTime", "Millisecond", "ParameterValue"))

  df <- df[, columns]

  set_timezone(df, tz = tz, col_datetime = "DateTime")
}


#' Read Weintek data from multiple files
#'
#' @param files path to Weintek files
#' @param tz time zone (default: CET) the measurements are taken (passed to
#' function kwb.pilot:::set_timezone())
#' @param dbg debug (default: TRUE)
#' @importFrom stats setNames
#' @return data frame with Weintek raw data
#' @export
#'
read_weintek_batch <- function(files, tz = "CET", dbg = TRUE) {
  paraname_site <- basename(dirname(files))

  data_list <- stats::setNames(
    object = lapply(files, read_weintek, tz = tz, dbg = dbg),
    nm = paraname_site
  )

  data_list %>%
    data.table::rbindlist(fill = TRUE, idcol = "paraname_site") %>%
    tidyr::separate(
      col = "paraname_site",
      into = c("ParameterName", "SiteName"),
      sep = "_"
    ) %>%
    dplyr::mutate(
      DataType = "raw",
      Source = "online"
    ) %>%
    dplyr::select(-.data$Millisecond)
}
