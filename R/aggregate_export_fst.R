#' Berlin-Tiefwerder: aggregate and export to fst
#'
#' @param year_month_start start year month (default: '2017-06')
#' @param year_month_end end year month (default: current month)
#' @param compression (default: 100)
#' @return exports data for each month into subfolder: /data/fst/year-month
#' @importFrom data.table rbindlist
#' @importFrom fst write.fst
#' @export
aggregate_export_fst_berlin_t <- function(
  year_month_start = "2017-06",
  year_month_end = format(Sys.Date(), "%Y-%m"),
  compression = 100
)
{
  aggregate_export_fst(
    year_month_start = year_month_start,
    year_month_end = year_month_end,
    compression = compression,
    FUN_get_monthly_data = get_rawfilespaths_for_month,
    FUN_import = import_data_berlin_t,
    FUN_calculate_ops = calculate_operational_parameters_berlin_t,
    prefix = "berlin_t"
  )
}

#' Berlin-Schoenerlinde: aggregate and export to fst
#'
#' @param year_month_start start year month (default: '2017-04')
#' @param year_month_end end year month (default: current month)
#' @param compression (default: 100)
#' @return exports data for each month into subfolder: /data/fst/year-month
#' @importFrom data.table rbindlist
#' @importFrom fst write.fst
#' @export
aggregate_export_fst_berlin_s <- function(
  year_month_start = "2017-04",
  year_month_end = format(Sys.Date(), "%Y-%m"),
  compression = 100
)
{
  aggregate_export_fst(
    year_month_start = year_month_start,
    year_month_end = year_month_end,
    compression = compression,
    FUN_get_monthly_data = function(x) {
      get_monthly_data_from_calendarweeks(year_month = x$year_month)
    },
    FUN_import = import_data_berlin_s,
    FUN_calculate_ops = calculate_operational_parameters_berlin_s,
    prefix ="berlin_s"
  )
}

#' Berlin-Friedrichshagen: aggregate and export to fst
#'
#' @param year_month_start start year month (default: '2019-11')
#' @param year_month_end end year month (default: current month)
#' @param compression (default: 100)
#' @return exports data for each month into subfolder: /data/fst/year-month
#' @importFrom data.table rbindlist
#' @importFrom fst write.fst
#' @importFrom stringr str_remove
#' @importFrom fs dir_ls
#' @export
aggregate_export_fst_berlin_f <- function(
  year_month_start = "2019-11",
  year_month_end = format(Sys.Date(), "%Y-%m"),
  compression = 100
)
{
  # Define function to list raw data files related to one month
  FUN_get_monthly_data <- function(x) {
    year_month <- unique(x$year_month)
    stopifnot(length(year_month == 1L))
    fs::dir_ls(
      package_file("shiny/berlin_f/data/raw/online_data"),
      recurse = TRUE, 
      regexp = sprintf(
        "^[^~].*%s[0-3][0-9].*\\.xlsx$",
        stringr::str_remove(year_month, "-")
      )
    )
  }
  
  aggregate_export_fst(
    year_month_start = year_month_start,
    year_month_end = year_month_end,
    compression = compression,
    FUN_get_monthly_data = FUN_get_monthly_data,
    FUN_import = import_data_berlin_f,
    FUN_calculate_ops = calculate_operational_parameters_berlin_f,
    prefix = "berlin_f",
    calculate_ops_on_level = "aggregated"
  )
}

#' MBR4.0: aggregate and export to fst
#' @param  siteData_raw_list tidy MBR4 data as retrieved by \code{\link{tidy_mbr4_data}},
#' (default: kwb.pilot::tidy_mbr4_data(kwb.pilot::read_mbr4()))
#' @param compression (default: 100)
#' @return exports data for each month into subfolder: /data/fst/year-month
#' @importFrom data.table rbindlist
#' @importFrom fst write.fst
#' @importFrom stringr str_remove
#' @importFrom fs dir_ls
#' @export
aggregate_export_fst_mbr4 <- function(
  siteData_raw_list = tidy_mbr4_data(read_mbr4()), 
  compression = 100
)
{
  aggregate_export_fst(
    compression = compression,
    # FUN_calculate_ops = calculate_operational_parameters_mbr4,
    calculate_ops_on_level = "aggregated",
    siteData_raw_list = siteData_raw_list,
    export_dir_path = sprintf("%s/data/fst", shiny_file("mbr4.0"))
  )
}

# check_or_create_export_dir ---------------------------------------------------
check_or_create_export_dir <- function(path) {
  if (!dir.exists(path)) {
    print(sprintf("Creating export path: %s", path))
    dir.create(path, recursive = TRUE)
  }
}
