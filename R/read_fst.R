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
read_fst <- function(path,
                     tz = "CET",
                     col_datetime = "DateTime",
                     ...) {
  df <- fst::read.fst(
    path,
    ...
  )
  df[, col_datetime] <- as.POSIXct(df[, col_datetime], origin = "1970-01-01", tz = "CET")
  return(df)
}


#' Load fst data for shiny app
#'
#' @param fst_dir directory of fst files to be loaded
#' @export
load_fst_data <- function(fst_dir) {
  print("### Step 4: Loading data ##########################")
  print("### 1): Raw data")
  assign(
    x = "siteData_raw_list",
    value = kwb.pilot::read_fst(path = file.path(
      fst_dir,
      "siteData_raw_list.fst"
    )),
    envir = .GlobalEnv
  )


  print("### 2) 10 minutes data")
  assign(
    x = "siteData_10min_list",
    value = kwb.pilot::read_fst(path = file.path(
      fst_dir,
      "siteData_10min_list.fst"
    )),
    envir = .GlobalEnv
  )


  print("### 3) hourly data")
  assign(
    x = "siteData_hour_list",
    value = kwb.pilot::read_fst(path = file.path(
      fst_dir,
      "siteData_hour_list.fst"
    )),
    envir = .GlobalEnv
  )


  print("### 4) daily data")
  assign(
    x = "siteData_day_list",
    value = kwb.pilot::read_fst(path = file.path(
      fst_dir,
      "siteData_day_list.fst"
    )),
    envir = .GlobalEnv
  )
}
