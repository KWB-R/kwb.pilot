#' Wrapper for fst::read.fst to read DateTime column in POSIXct format
#'
#' @param path path to fst file
#' @param tz timezone of DateTime to be imported (default: "CET")
#' @param col_datetime column name containing numeric values in nanoseconds since
#' 1970-01-01 (default: "DateTime")
#' @param ... further arguments passed to fst::read.fst
#' @return data.frame with formatting of DateTime column POSIXct
#' @importFrom fst read.fst
#' @export
read_fst <- function(path, tz = "CET", col_datetime = "DateTime", ...) {
  df <- fst::read.fst(path, ...)
  df[, col_datetime] <- as.POSIXct(df[, col_datetime], origin = "1970-01-01", tz = "CET")
  df
}


#' Load fst data for shiny app
#'
#' @param fst_dir directory of fst files to be loaded
#' @importFrom kwb.utils assignGlobally
#' @export
load_fst_data <- function(fst_dir) {
  print("### Step 4: Loading data ##########################")

  step_no <- 0L

  step_assign <- function(title, varname, filename) {
    step_no <<- step_no + 1L
    print(sprintf("### %d): %s", step_no, title))
    kwb.utils::assignGlobally(varname, read_fst(file.path(fst_dir, filename)))
  }

  step_assign("Raw data", "siteData_raw_list", "siteData_raw_list.fst")
  step_assign("10 minutes data", "siteData_10min_list", "siteData_10min_list.fst")
  step_assign("hourly data", "siteData_hour_list", "siteData_hour_list.fst")
  step_assign("daily data", "siteData_day_list", "siteData_day_list.fst")
}
