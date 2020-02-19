use_live_data <- TRUE

if (use_live_data) {

library(kwb.pilot)

year_month_start <- format(Sys.Date() - months(1, abbreviate = FALSE),
                           format = "%Y-%m")
year_month_end <- format(Sys.Date(), format = "%Y-%m")

print("#################################################################################")
print(sprintf(" ###### Generating & exporting .fst files for months: %s - %s",
      year_month_start,
      year_month_end))
print("#################################################################################")

kwb.pilot::aggregate_export_fst_berlin_t(year_month_start = year_month_start,
                                              year_month_end = year_month_end)


month_pattern <- kwb.pilot:::to_month_pattern(year_month_start, year_month_end)
kwb.pilot::merge_and_export_fst(time_pattern = month_pattern)

}

kwb.pilot::load_fst_data(fst_dir = kwb.pilot:::package_file("shiny/berlin_t/data"))

print("### Step 5: Importing threshold information ##########################")

threshold_file <- kwb.pilot:::package_file("shiny/berlin_t/data/thresholds.csv")

thresholds <- kwb.pilot::get_thresholds(threshold_file)

print("### Step 6: Specify available months for reporting ##########################")
report_months <- kwb.pilot::create_monthly_selection(startDate = "2017-06-01")

#print("### Step 7: Add default calculated operational parameters ##########################")

#report_calc_paras <- unique(kwb.pilot::calculate_operational_parameters(df = siteData_10min_list)$ParameterName)

report_calc_paras <- "NOT_IMPLEMENTED_YET"
