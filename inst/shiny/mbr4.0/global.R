use_live_data <- TRUE

if (use_live_data) {
  
  library(kwb.pilot)

  # year_month_start <- format(Sys.Date() - months(1, abbreviate = FALSE),
  #                            format = "%Y-%m")
  year_month_start <- "2021-03"
  year_month_end <- format(Sys.Date(), format = "%Y-%m")

  kwb.utils::catAndRun(messageText = "Raw data: Downloading", expr = {
  siteData_raw <- kwb.pilot::read_mbr4(target_dir = kwb.pilot::shiny_file("mbr4.0/data/raw/online_data"))
  })
  kwb.utils::catAndRun(messageText = "Raw data: Tidying", expr = {
  siteData_raw_list <-  kwb.pilot::tidy_mbr4_data(mbr4_data = siteData_raw, 
                                                  path_metadata = kwb.pilot::shiny_file("mbr4.0/data/metadata.csv")
                                                  )
  })
  
  print("#################################################################################")
  print(sprintf(" ###### Generating & exporting .fst files for months: %s - %s",
                year_month_start,
                year_month_end))
  print("#################################################################################")
  
  kwb.pilot::aggregate_export_fst_mbr4(mbr4_data_tidy = siteData_raw_list)
  
  
  
}

kwb.pilot::load_fst_data(fst_dir = kwb.pilot::shiny_file("mbr4.0/data/fst"))

print("### Step 5: Importing threshold information ##########################")

print("### NOT IMPLEMENTED YET")
#threshold_file <- kwb.pilot:::package_file("shiny/mbr4.0/data/thresholds.csv")

#thresholds <- kwb.pilot::get_thresholds(threshold_file)

print("### Step 6: Specify available months for reporting ##########################")
report_months <- kwb.pilot::create_monthly_selection(startDate = "2021-03-01")

#print("### Step 7: Add default calculated operational parameters ##########################")

#report_calc_paras <- unique(kwb.pilot::calculate_operational_parameters(df = siteData_10min_list)$ParameterName)

report_calc_paras <- "NOT_IMPLEMENTED_YET"
