use_live_data <- FALSE

if (use_live_data) {

library(kwb.pilot)

analytics <- file.path(getwd(),
                       "data/analytics.xlsx")

mySQL <- file.path(getwd(),
                   ".my.cnf")

op_meta <- file.path(getwd(),
                     "data/operation_parameters.csv")

system.time(
siteData_raw_list <- kwb.pilot::import_data_haridwar(analytics_path = analytics,
                                          operation_mySQL_conf = mySQL,
                                          operation_meta_path = op_meta))

print("### Step 4: Performing temporal aggregation ##########################")
system.time(
siteData_10min_list <- kwb.pilot::group_datetime(siteData_raw_list,
                                                      by = 10*60))

system.time(
siteData_hour_list <- kwb.pilot::group_datetime(siteData_raw_list,
                                                     by = 60*60))

system.time(
  siteData_day_list <- kwb.pilot::group_datetime(siteData_raw_list,
                                                        by = "day"))



saveRDS(siteData_raw_list, file = "data/siteData_raw_list.Rds")
saveRDS(siteData_10min_list, file = "data/siteData_10min_list.Rds")
saveRDS(siteData_hour_list, file = "data/siteData_hour_list.Rds")
saveRDS(siteData_day_list, file = "data/siteData_day_list.Rds")

} else {
  #siteData_raw_list <- readRDS("data/siteData_raw_list.Rds")
  siteData_10min_list <- readRDS("data/siteData_10min_list.Rds")
  #siteData_hour_list <- readRDS("data/siteData_hour_list.Rds")
  #siteData_day_list <- readRDS("data/siteData_day_list.Rds")
}

print("### Step 5: Importing threshold information ##########################")
thresholds <- kwb.pilot::get_thresholds()

# Limit reporting time range to available data
data_timerange <- as.Date(paste0(format(range(siteData_10min_list$DateTime), format = "%Y-%m"), "-01"))

print("### Step 6: Specify available months for reporting ##########################")
report_months <- kwb.pilot::create_monthly_selection(startDate = as.character(data_timerange[1]),
                                                          endDate = data_timerange[2])


print("### Step 7: Add default calculated operational parameters ##########################")
report_calc_paras <- unique(kwb.pilot::calculate_operational_parameters(df = siteData_10min_list)$ParameterName)
