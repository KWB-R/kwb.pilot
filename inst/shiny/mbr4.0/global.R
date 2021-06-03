use_live_data <- TRUE

if (use_live_data) {

library(kwb.pilot)

paths_list <- list(
  url = Sys.getenv("MBR4.0_URL"),
  export_dir = "data/",
  tsv_file = "<export_dir>/mbr4.tsv"
  )
paths <- kwb.utils::resolve(paths_list)

print("### Step 5: Importing threshold information ##########################")

print("### NOT IMPLEMENTED YET")
threshold_file <- kwb.pilot:::package_file("shiny/mbr4.0/data/thresholds.csv")
 
thresholds <- kwb.pilot::get_thresholds(threshold_file)

print("### Step 6: Specify available months for reporting ##########################")
report_months <- kwb.pilot::create_monthly_selection(startDate = "2019-11-01")

#print("### Step 7: Add default calculated operational parameters ##########################")

#report_calc_paras <- unique(kwb.pilot::calculate_operational_parameters(df = siteData_10min_list)$ParameterName)

report_calc_paras <- "NOT_IMPLEMENTED_YET"
