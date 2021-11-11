# aggregate_export_fst ---------------------------------------------------------

#' Aggregate and Export to FST Format
#'
#' @param year_month_start start year month in yyyy-mm format, e.g. "2017-06"
#' @param year_month_end end year month (default: current month)
#' @param compression (default: 100)
#' @param FUN_get_monthly_data function to be called to determine
#'   "raw_data_file_paths"
#' @param FUN_import function to be called to read data into "siteData_raw_list"
#' @param FUN_calculate_ops function to be called to calculate operational
#'   parameters. If \code{NULL} (the default), no operational parameters are
#'   calculated
#' @param prefix site-specific prefix to be used as a sub folder name in the
#'   export directory
#' @param calculate_ops_on_level one of "raw", "aggregated". Determines whether
#'   to calculate operational parameters based on the raw or on the aggregated
#'   values. Not used if \code{FUN_calculate_ops} is \code{NULL}
#' @param siteData_raw_list existing raw data. If given (not \code{NULL}), no
#'   monthly periods are calculated and no data are imported. The given data are
#'   treated as one and only (monthly?) period.
#' @param export_dir_path path to the export directory. Only required if raw
#'   data are given in \code{siteData_raw}.
#' @return exports data for each month into subfolder: /data/fst/year-month
#' @importFrom data.table rbindlist
#' @importFrom fst write.fst
#' @export
aggregate_export_fst <- function(
  year_month_start = NULL,
  year_month_end = format(Sys.Date(), "%Y-%m"),
  compression = 100,
  FUN_get_monthly_data = NULL,
  FUN_import = NULL,
  FUN_calculate_ops = NULL,
  prefix = NULL,
  calculate_ops_on_level = c("raw", "aggregated")[1L],
  siteData_raw_list = NULL,
  export_dir_path = NULL
)
{
  stopifnot(calculate_ops_on_level %in% c("raw", "aggregated"))

  # If no raw data are given in "siteData_raw_list", determine the monthly 
  # periods for which to read raw data  
  if (is.null(siteData_raw_list)) {
    
    monthly_periods <- get_monthly_periods(
      year_month_start = year_month_start,
      year_month_end = year_month_end
    )
    
    year_months <- monthly_periods$year_month
    
  } else {
    
    # Otherwise enter the following loop only once with one fake value that
    # is actually not used
    year_months <- "whole-period"
  }

  # Loop over monthly periods (may be only one period "whole-period")
  for (year_month in year_months) {

    # Import data for that month (only if not given in "siteData_raw_list"!)
    if (is.null(siteData_raw_list)) {
      
      siteData_raw_list <- import_data_for_month(
        monthly_periods = monthly_periods, 
        year_month = year_month, 
        FUN_get_monthly_data = FUN_get_monthly_data,
        FUN_import = FUN_import
      )
    }

    # Determine the path to the export directory (if not given)
    export_dir_path <- kwb.utils::defaultIfNULL(
      export_dir_path, 
      sprintf("%s/data/fst/%s", package_file("shiny", prefix), year_month)
    )
    
    # Make sure that the export directory exists    
    check_or_create_export_dir(export_dir_path)
    
    # If applicable, calculate operational parameters on raw values
    if (calculate_ops_on_level == "raw") {
      siteData_raw_list <- add_operational(siteData_raw_list, FUN_calculate_ops)
    }

    # Write raw values
    system.time(fst::write.fst(
      siteData_raw_list,
      path = sprintf("%s/siteData_raw_list.fst", export_dir_path),
      compress = compression
    ))
    
    print("### Step 4: Performing temporal aggregation (10 min) #################")

    # Calculate 10 minute values    
    system.time(
      siteData_10min_list <- group_datetime(siteData_raw_list, by = 10 * 60)
    )

    # If applicable, calculate operational parameters on aggregated values
    if (calculate_ops_on_level == "aggregated") {
      siteData_10min_list <- add_operational(siteData_10min_list, FUN_calculate_ops)
    }
    
    # Write 10 minute values
    fst::write.fst(
      siteData_10min_list,
      path = sprintf("%s/siteData_10min_list.fst", export_dir_path),
      compress = compression
    )
    
    print("### Step 6: Performing temporal aggregation (1 h, 1 day) #########################")

    # Calculate hourly values    
    system.time(
      siteData_hour_list <- group_datetime(siteData_10min_list, by = 60 * 60)
    )

    # Write hourly values    
    fst::write.fst(
      siteData_hour_list,
      path = sprintf("%s/siteData_hour_list.fst", export_dir_path),
      compress = compression
    )

    # Calculate daily values    
    system.time(
      siteData_day_list <- group_datetime(siteData_hour_list, by = "day")
    )
    
    # Write daily values
    fst::write.fst(
      siteData_day_list,
      path = sprintf("%s/siteData_day_list.fst", export_dir_path),
      compress = compression
    )
    
  } # End of loop over monthly periods
}

# import_data_for_month --------------------------------------------------------
import_data_for_month <- function(
  monthly_periods, 
  year_month, 
  FUN_get_monthly_data,
  FUN_import
)
{
  print(sprintf("Importing data for month '%s':", year_month))
  
  monthly_period <- monthly_periods[monthly_periods$year_month == year_month, ]
  
  datetime_start <- as_posix_cet("%s 00:00:00", monthly_period$start)
  datetime_end <- as_posix_cet("%s 23:59:59", monthly_period$end)
  
  raw_data_file_paths <- FUN_get_monthly_data(monthly_period)
  
  system.time(
    siteData_raw_list <- FUN_import(raw_data_files = raw_data_file_paths)
  )
  
  times <- kwb.utils::selectColumns(siteData_raw_list, "DateTime")
  
  condition <- times >= datetime_start & times <= datetime_end
  
  siteData_raw_list <- siteData_raw_list[condition, ]
  
  print(sprintf(
    "Reduced imported data points to time period: %s - %s",
    as.character(min(siteData_raw_list$DateTime)),
    as.character(max(siteData_raw_list$DateTime))
  ))
  
  siteData_raw_list
}

# add_operational --------------------------------------------------------------
add_operational <- function(df, FUN_calculate_ops = NULL)
{
  print("### Step: Calcualtating operational parameters ##########################")
  
  if (is.null(FUN_calculate_ops)) {
    
    print("### -> Skipped (no calculation function given).")
    return(df)
  }
  
  calc_dat <- FUN_calculate_ops(df = df)
  
  data.table::rbindlist(
    l = list(df, calc_dat), 
    use.names = TRUE, 
    fill = TRUE
  ) %>%
    as.data.frame()
}
