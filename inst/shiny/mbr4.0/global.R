use_live_data <- TRUE

if (use_live_data) {

library(kwb.pilot)

kwb.pilot::

print("### Step 5: Importing threshold information ##########################")

print("### NOT IMPLEMENTED YET")
threshold_file <- kwb.pilot:::package_file("shiny/berlin_f/data/thresholds.csv")
 
thresholds <- kwb.pilot::get_thresholds(threshold_file)

print("### Step 6: Specify available months for reporting ##########################")
report_months <- kwb.pilot::create_monthly_selection(startDate = "2019-11-01")

#print("### Step 7: Add default calculated operational parameters ##########################")

#report_calc_paras <- unique(kwb.pilot::calculate_operational_parameters(df = siteData_10min_list)$ParameterName)

report_calc_paras <- "NOT_IMPLEMENTED_YET"
