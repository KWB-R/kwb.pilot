#' Imports Haridwar data
#' @param analytics_path Define path of analytics EXCEL spreadsheet to be
#' imported (default: system.file(file.path("shiny/haridwar/data",
#' "analytics.xlsx"),
#' package = "kwb.pilot"))
#' @param operation_mySQL_conf column name pattern for identifying raw data
#' (default: system.file("shiny/haridwar/.my.cnf", package = "kwb.pilot"))
#' @param operation_meta_path path to table with meta data for operational
#' parameters (default: system.file(file.path("shiny/haridwar/data",
#' "operation_parameters.csv"), package = "kwb.pilot"))
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
#' @import dplyr
#' @importFrom plyr rbind.fill
#' @importFrom utils read.csv
#' @export

import_data_haridwar <- function(analytics_path = system.file(
                                 file.path(
                                   "shiny",
                                   "haridwar/data/analytics.xlsx"
                                 ),
                                 package = "kwb.pilot"
                               ),
                               operation_mySQL_conf = system.file(
                                 "shiny/haridwar/.my.cnf",
                                 package = "kwb.pilot"
                               ),
                               operation_meta_path = system.file(
                                 file.path(
                                   "shiny/haridwar/data",
                                   "operation_parameters.csv"
                                 ),
                                 package = "kwb.pilot"
                               ),
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
                                 #"SAK_254",
                                 #"SAK_463",
                                 "As_total_Arsenator"
                               ),
                               skip = 69,
                               debug = TRUE) {
  if (!file.exists(analytics_path)) {
    msg <- sprintf(
      "No analytics file %s is located under: %s",
      basename(analytics_path),
      dirname(analytics_path)
    )
    stop(
      msg,
      call. = FALSE
    )
  }


  if (!file.exists(operation_mySQL_conf)) {
    msg <- sprintf("No '.my.cnf' file located under: %s\n.
                   Please once specify the path to a valid MySQL config file with parameter
                   'mySQL_conf'", dirname(operation_mySQL_conf))
    stop(
      msg,
      call. = FALSE
    )
  }

  ###############################################################################
  ###############################################################################
  ###############################################################################
  #### Site 1: Haridwar
  ###############################################################################
  ###############################################################################
  ###############################################################################

  ###############################################################################
  #### 1) Import analytics data from EXCEL spreadsheet
  ###############################################################################
  if (debug) print("### Step 1: Import analytics data ##########################")


  all_sheets <- readxl::excel_sheets(path = analytics_path)

  analytics_to_import <- all_sheets[!all_sheets %in% excludedSheets]



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


  ###############################################################################
  #### 2) Operational data
  ###############################################################################

  if (debug) print("### Step 2: Import operational data ##########################")



  #### 2.1) Import

  operation <- import_operation(mysql_conf = operation_mySQL_conf)


  if (debug) print("### Step 3: Standardise analytics & operational data ##########################")

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


  cols_to_drop <- c(
    "Comments",
    "ParameterThresholdComparison",
    "ParameterThreshold",
    "ParameterThresholdSource"
  )

  operation_para_names <- operation_para_names %>%
    dplyr::select_(.dots = dplyr::setdiff(names(operation_para_names), cols_to_drop)) %>%
    left_join(sites_meta)


  operation_para_names$SiteName[is.na(operation_para_names$SiteName)] <- "General"


  operation_list <- operation_list %>%
    dplyr::left_join(operation_para_names) %>%
    dplyr::mutate_(
      Source = "as.character('online')",
      DataType = "as.character('raw')"
    )




  haridwar_raw_list <- plyr::rbind.fill(operation_list, analytics_4014) %>%
    dplyr::filter_("!is.na(ParameterValue)") %>%
    dplyr::mutate_(
      "SiteName_ParaName_Unit" = "ifelse(test = ParameterName == 'Redox potential' & SiteName == 'Tank water',
                                     sprintf('%s: %s %d (%s)', SiteName, ParameterName, measurementID, ParameterUnit),
                                     sprintf('%s: %s (%s)', SiteName, ParameterName, ParameterUnit))",
      "measurementID" = "ifelse(test = ParameterName == 'Redox potential' & SiteName == 'Tank water',
                                     1, measurementID)"
    )

  return(haridwar_raw_list)
}
