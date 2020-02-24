#
# TODO: Create a general function and let these two functions be wrappers
#   around the general function!
#

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
  monthly_periods <- get_monthly_periods(
    year_month_start = year_month_start,
    year_month_end = year_month_end
  )

  as_posix_cet <- function(fmt, x) as.POSIXct(sprintf(fmt, x), tz = "CET")

  for (year_month in monthly_periods$year_month) {
    
    monthly_period <- monthly_periods[monthly_periods$year_month == year_month, ]
    
    print(sprintf("Importing data for month '%s':", year_month))
    
    raw_data_file_paths <- get_rawfilespaths_for_month(monthly_period)

    system.time(
      siteData_raw_list <- import_data_berlin_t(
        raw_data_files = raw_data_file_paths
      )
    )

    datetime_start <- as_posix_cet("%s 00:00:00", monthly_period$start)
    datetime_end <- as_posix_cet("%s 23:59:59", monthly_period$end)
    
    times <- kwb.utils::selectColumns(siteData_raw_list, "DateTime")
    
    condition <- times >= datetime_start & times <= datetime_end
    
    siteData_raw_list <- siteData_raw_list[condition, ]
    
    print(sprintf(
      "Reduced imported data points to time period: %s - %s",
      as.character(min(siteData_raw_list$DateTime)),
      as.character(max(siteData_raw_list$DateTime))
    ))
    
    calc_dat <- calculate_operational_parameters_berlin_t(df = siteData_raw_list)
    
    siteData_raw_list <- data.table::rbindlist(
      l = list(siteData_raw_list, calc_dat), use.names = TRUE, fill = TRUE
    ) %>%
      as.data.frame()
    
    export_dir_path <- sprintf(
      "%s/data/fst/%s",
      package_file("shiny/berlin_t"),
      monthly_period$year_month
    )
    
    check_or_create_export_dir(export_dir_path)
    
    system.time(fst::write.fst(
      siteData_raw_list,
      path = sprintf("%s/siteData_raw_list.fst", export_dir_path),
      compress = compression
    ))
    
    print("### Step 4: Performing temporal aggregation ##########################")
    
    system.time(
      siteData_10min_list <- group_datetime(siteData_raw_list, by = 10 * 60)
    )
    
    fst::write.fst(
      siteData_10min_list,
      path = sprintf("%s/siteData_10min_list.fst", export_dir_path),
      compress = compression
    )
    
    system.time(
      siteData_hour_list <- group_datetime(siteData_10min_list, by = 60 * 60)
    )
    
    fst::write.fst(
      siteData_hour_list,
      path = sprintf("%s/siteData_hour_list.fst", export_dir_path),
      compress = compression
    )
    
    system.time(
      siteData_day_list <- group_datetime(siteData_hour_list, by = "day")
    )
    
    fst::write.fst(
      siteData_day_list,
      path = sprintf("%s/siteData_day_list.fst", export_dir_path),
      compress = compression
    )
  }
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
  monthly_periods <- get_monthly_periods(
    year_month_start = year_month_start,
    year_month_end = year_month_end
  )

  as_posix_cet <- function(fmt, x) as.POSIXct(sprintf(fmt, x), tz = "CET")
  
  
  for (year_month in monthly_periods$year_month) {
    
    monthly_period <- monthly_periods[monthly_periods$year_month == year_month,]
    
    print(sprintf("Importing data for month '%s':", year_month))
    
    raw_data_file_paths <- get_monthly_data_from_calendarweeks(
      year_month = monthly_period$year_month
    )

    system.time(
      siteData_raw_list <- import_data_berlin_s(raw_data_files = raw_data_file_paths)
    )

    datetime_start <- as_posix_cet("%s 00:00:00", monthly_period$start)
    datetime_end <- as_posix_cet("%s 23:59:59", monthly_period$end)

    times <- kwb.utils::selectColumns(siteData_raw_list, "DateTime")
    
    condition <- times >= datetime_start & times <= datetime_end
    
    siteData_raw_list <- siteData_raw_list[condition, ]
    
    print(sprintf(
      "Reduced imported data points to time period: %s - %s",
      as.character(min(siteData_raw_list$DateTime)),
      as.character(max(siteData_raw_list$DateTime))
    ))
    
    calc_dat <- calculate_operational_parameters_berlin_s(df = siteData_raw_list)
    
    siteData_raw_list <- data.table::rbindlist(
      l = list(siteData_raw_list, calc_dat), use.names = TRUE, fill = TRUE
    ) %>%
      as.data.frame()

    export_dir_path <- sprintf(
      "%s/data/fst/%s",
      package_file("shiny/berlin_s"),
      monthly_period$year_month
    )

    check_or_create_export_dir(export_dir_path)
    
    system.time(fst::write.fst(
      siteData_raw_list,
      path = sprintf("%s/siteData_raw_list.fst", export_dir_path),
      compress = compression
    ))
    
    print("### Step 4: Performing temporal aggregation ##########################")
    
    system.time(
      siteData_10min_list <- group_datetime(siteData_raw_list, by = 10 * 60)
    )
    
    fst::write.fst(
      siteData_10min_list,
      path = sprintf("%s/siteData_10min_list.fst", export_dir_path),
      compress = compression
    )
    
    system.time(
      siteData_hour_list <- group_datetime(siteData_10min_list, by = 60 * 60)
    )
    
    fst::write.fst(
      siteData_hour_list,
      path = sprintf("%s/siteData_hour_list.fst", export_dir_path),
      compress = compression
    )
    
    system.time(
      siteData_day_list <- group_datetime(siteData_hour_list, by = "day")
    )
    
    fst::write.fst(
      siteData_day_list,
      path = sprintf("%s/siteData_day_list.fst", export_dir_path),
      compress = compression
    )
  }
}

#' Berlin-Friedrichshagen: aggregate and export to fst
#' 
#' @param year_month_start start year month (default: '2019-11')
#' @param year_month_end end year month (default: current month)
#' @param compression (default: 100)
#' @return exports data for each month into subfolder: /data/fst/year-month
#' @importFrom data.table rbindlist
#' @importFrom fst write.fst
#<<<<<<< HEAD
#' @importFrom stringr str_remove
#' @importFrom fs dir_ls
#=======
#>>>>>>> 591dee808dea6cb67180a6fab98e4d25637cbc85
#' @export
aggregate_export_fst_berlin_f <- function(
  year_month_start = "2019-11",
  year_month_end = format(Sys.Date(), "%Y-%m"),
  compression = 100
)
{
  monthly_periods <- get_monthly_periods(
    year_month_start = year_month_start,
    year_month_end = year_month_end
  )
  
  as_posix_cet <- function(fmt, x) as.POSIXct(sprintf(fmt, x), tz = "CET")
  
#<<<<<<< HEAD

#=======
#  times <- kwb.utils::selectColumns()
  
#>>>>>>> 591dee808dea6cb67180a6fab98e4d25637cbc85
  for (year_month in monthly_periods$year_month) {
    
    monthly_period <- monthly_periods[monthly_periods$year_month == year_month,]
    
    print(sprintf("Importing data for month '%s':", year_month))
    
#<<<<<<< HEAD
    raw_data_file_paths <- fs::dir_ls(package_file("shiny/berlin_f/data/operation"), 
                        recurse = TRUE, regexp = sprintf("^[^~].*%s[0-3][0-9].*\\.xlsx$", 
                                                         stringr::str_remove(year_month, "-")))
    
    
  
#=======
#    raw_data_file_paths <- get_monthly_data_from_calendarweeks(
#      year_month = monthly_period$year_month
#    )
#    
#>>>>>>> 591dee808dea6cb67180a6fab98e4d25637cbc85
    system.time(
      siteData_raw_list <- import_data_berlin_f(raw_data_files = raw_data_file_paths)
    )
    
    datetime_start <- as_posix_cet("%s 00:00:00", monthly_period$start)
    datetime_end <- as_posix_cet("%s 23:59:59", monthly_period$end)
    
    times <- kwb.utils::selectColumns(siteData_raw_list, "DateTime")
    
    condition <- times >= datetime_start & times <= datetime_end
    
    siteData_raw_list <- siteData_raw_list[condition, ]
    
    print(sprintf(
      "Reduced imported data points to time period: %s - %s",
      as.character(min(siteData_raw_list$DateTime)),
      as.character(max(siteData_raw_list$DateTime))
    ))
    
#<<<<<<< HEAD
    #calc_dat <- calculate_operational_parameters_berlin_f(df = siteData_raw_list)
    
    # siteData_raw_list <- data.table::rbindlist(
    #   l = list(siteData_raw_list, calc_dat), use.names = TRUE, fill = TRUE
    # ) %>%
    #   as.data.frame()
#=======
#    calc_dat <- calculate_operational_parameters_berlin_f(df = siteData_raw_list)
    
#    siteData_raw_list <- data.table::rbindlist(
#      l = list(siteData_raw_list, calc_dat), use.names = TRUE, fill = TRUE
#    ) %>%
#      as.data.frame()
#>>>>>>> 591dee808dea6cb67180a6fab98e4d25637cbc85
    
    export_dir_path <- sprintf(
      "%s/data/fst/%s",
      package_file("shiny/berlin_f"),
      monthly_period$year_month
    )
    
    check_or_create_export_dir(export_dir_path)
    
    system.time(fst::write.fst(
      siteData_raw_list,
      path = sprintf("%s/siteData_raw_list.fst", export_dir_path),
      compress = compression
    ))
    
    print("### Step 4: Performing temporal aggregation ##########################")
    
    system.time(
      siteData_10min_list <- group_datetime(siteData_raw_list, by = 10 * 60)
    )
    
    fst::write.fst(
      siteData_10min_list,
      path = sprintf("%s/siteData_10min_list.fst", export_dir_path),
      compress = compression
    )
    
    system.time(
      siteData_hour_list <- group_datetime(siteData_10min_list, by = 60 * 60)
    )
    
    fst::write.fst(
      siteData_hour_list,
      path = sprintf("%s/siteData_hour_list.fst", export_dir_path),
      compress = compression
    )
    
    system.time(
      siteData_day_list <- group_datetime(siteData_hour_list, by = "day")
    )
    
    fst::write.fst(
      siteData_day_list,
      path = sprintf("%s/siteData_day_list.fst", export_dir_path),
      compress = compression
    )
  }
}


# check_or_create_export_dir ---------------------------------------------------
check_or_create_export_dir <- function(path)
{
  if (! dir.exists(path)) {
    print(sprintf("Creating export path: %s", path))
    dir.create(path, recursive = TRUE)
  }
}
