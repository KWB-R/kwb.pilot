#' Imports Haridwar data
#'
#' @param analytics_path Define path of analytics EXCEL spreadsheet to be
#' imported (default: kwb.pilot:::shiny_file("haridwar/data/analytics.xlsx"))
#' @param operation_mySQL_conf column name pattern for identifying raw data
#' (default: kwb.pilot:::shiny_file("haridwar/.my.cnf"))
#' @param operation_meta_path path to table with meta data for operational
#' parameters (default: kwb.pilot:::shiny_file("haridwar/data/operation_parameters.csv"))
#' @param excludedSheets all sheets, which are not listed here will be imported
#' as lab data sheets (default: c("Parameters", "Location", "Sites", "#Summary",
#' "Site_and_Parameter", "Observations", "dP", "ORP", "Flow", "Current_Voltage",
#' "As_total_Arsenator"))
#' @param skip number of rows to skip for each lab data sheet (default: 69), i.e.
#' for all sheets which are not explictly excluded with parameter "excludedSheets"
#' @param debug if TRUE print debug messages (default: TRUE)
#' @return returns data frame with Haridwar raw data (operation & analytics)
#' @import readxl
#' @import tidyr
#' @importFrom dplyr setdiff select_ filter_ mutate_
#' @importFrom plyr rbind.fill
#' @importFrom utils read.csv
#' @export

import_data_haridwar <- function(
  analytics_path = shiny_file("haridwar/data/analytics.xlsx"),
  operation_mySQL_conf = shiny_file("haridwar/.my.cnf"),
  operation_meta_path = shiny_file("haridwar/data/operation_parameters.csv"),
  excludedSheets = c(
    "Parameters",
    "Location",
    "Sites",
    "#Summary",
    "Site_and_Parameter",
    "Observations",
    "dP",
    "ORP",
    "Flow",
    "Current_Voltage",
    # "SAK_254",
    # "SAK_463",
    "As_total_Arsenator"
  ),
  skip = 69,
  debug = TRUE
)
{
  if (! file.exists(analytics_path)) {
    clean_stop(sprintf(
      "No analytics file %s is located under: %s",
      basename(analytics_path), dirname(analytics_path)
    ))
  }
  
  if (!file.exists(operation_mySQL_conf)) {
    clean_stop(
      "No '.my.cnf' file located under: ", dirname(operation_mySQL_conf), ".\n",
      "Please once specify the path to a valid MySQL config file with ",
      "parameter 'mySQL_conf'"
    )
  }

  dbg <- function(...) kwb.utils::catIf(debug, ...)

  ##############################################################################
  ##############################################################################
  #### Site 1: Haridwar
  ##############################################################################
  ##############################################################################

  ##############################################################################
  #### 1) Import analytics data from EXCEL spreadsheet
  ##############################################################################
  dbg("### Step 1: Import analytics data #####################################")

  all_sheets <- readxl::excel_sheets(path = analytics_path)

  analytics_to_import <- all_sheets[! all_sheets %in% excludedSheets]

  analytics_4014 <- import_sheets(
    xlsPath = analytics_path,
    sheets_analytics = analytics_to_import,
    skip = skip
  )

  drop.cols <- c("Who", "Comments", "LocationName", "LocationID")
  select.cols <- dplyr::setdiff(names(analytics_4014), drop.cols)

  analytics_4014 <- analytics_4014[, select.cols] %>%
    dplyr::filter_("!is.na(ParameterValue)") %>%
    dplyr::mutate_(Source = "as.character('offline')")

  ##############################################################################
  #### 2) Operational data
  ##############################################################################

  dbg("### Step 2: Import operational data ###################################")

  #### 2.1) Import

  operation <- import_operation(mysql_conf = operation_mySQL_conf)

  dbg("### Step 3: Standardise analytics & operational data ##################")

  drop.cols <- c("AnlagenID", "LocationName", "id", "localTime")

  select.cols <- dplyr::setdiff(names(operation), drop.cols)

  operation <- operation[, select.cols]

  operation_list <- tidyr::gather_(
    data = operation,
    key_col = "ParameterCode",
    value_col = "ParameterValue",
    gather_cols = dplyr::setdiff(names(operation), "DateTime")
  ) %>%
    dplyr::filter_("!is.na(ParameterValue)")

  sites_meta <- analytics_4014 %>%
    group_by_(~SiteCode, ~SiteName) %>%
    summarise_(n = "n()") %>%
    select_(~SiteCode, ~SiteName)

  operation_para_names <- utils::read.csv(
    file = operation_meta_path,
    stringsAsFactors = FALSE
  )

  columns <- dplyr::setdiff(names(operation_para_names), c(
    "Comments",
    "ParameterThresholdComparison",
    "ParameterThreshold",
    "ParameterThresholdSource"
  ))

  operation_para_names <- operation_para_names %>%
    dplyr::select_(.dots = columns) %>%
    left_join(sites_meta)

  has_no_name <- is.na(operation_para_names$SiteName)

  operation_para_names$SiteName[has_no_name] <- "General"

  operation_list <- operation_list %>%
    dplyr::left_join(operation_para_names) %>%
    dplyr::mutate_(
      Source = "as.character('online')",
      DataType = "as.character('raw')"
    )

  plyr::rbind.fill(operation_list, analytics_4014) %>%
    dplyr::filter_("!is.na(ParameterValue)") %>%
    dplyr::mutate_(
      "SiteName_ParaName_Unit" = "ifelse(test = ParameterName == 'Redox potential' & SiteName == 'Tank water',
                                     sprintf('%s: %s %d (%s)', SiteName, ParameterName, measurementID, ParameterUnit),
                                     sprintf('%s: %s (%s)', SiteName, ParameterName, ParameterUnit))",
      "measurementID" = "ifelse(test = ParameterName == 'Redox potential' & SiteName == 'Tank water',
                                     1, measurementID)"
    )
}
