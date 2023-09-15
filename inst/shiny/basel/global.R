use_live_data <- FALSE

if (use_live_data) {

library(kwb.pilot)
library(dplyr)



siteData_raw_list <- kwb.pilot::import_data_basel()


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

threshold_file <- kwb.pilot:::shiny_file("basel/data/thresholds.csv")

thresholds <- kwb.pilot::get_thresholds(threshold_file)

print("### Step 6: Specify available months for reporting ##########################")
report_months <- kwb.pilot::create_monthly_selection(startDate = "2017-05-01")

#print("### Step 7: Add default calculated operational parameters ##########################")

report_calc_paras <- "NOT_IMPLEMENTED_YET"
