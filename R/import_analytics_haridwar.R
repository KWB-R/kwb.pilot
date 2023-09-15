#' Helper function: fill empty datetime & create measurementID column in case of
#' samples
#' @param df a dataframe
#' @param col_rawData_pattern column name pattern for identifying raw data
#' (default: "raw")
#' @param col_datetime column name pattern for identifying datetime column
#' (default: "DateTime"),
#' @param dbg print debug messages (default: FALSE)
#' @return returns data frame with filled "DateTime" column and new column
#' "measurementID" in case of samples
#' @importFrom plyr rbind.fill
#' @keywords internal
fill_datetime <- function(
  df,
  col_rawData_pattern = "raw",
  col_datetime = "DateTime",
  dbg = FALSE
)
{
  columns_raw_data <- grep(names(df), pattern = col_rawData_pattern)
  
  df$totSamples <- rowSums(!is.na(df[, columns_raw_data]))
  
  df$measurementID <- NA
  
  dates_indices <- which(! is.na(df[[col_datetime]]))
  
  for (start_index in dates_indices) {
    
    measurementID <- 0L
    end_index <- start_index + 2L
    
    for (ind in start_index:end_index) {
      
      if (df$totSamples[ind] > 0L) {
        
        measurementID <- measurementID + 1
        df$DateTime[ind] <- df$DateTime[start_index]
        df$measurementID[ind] <- measurementID
        
      } else {
        
        if (dbg) {
          print("Do nothing")
        }
      }
    }
  }
  
  df
}

#' Imports an analytics sheet from an EXCEL spreadsheet
#' @param xlsPath path to xls file with analytics data
#' @param sheet a sheet in spreadsheet defined with "xlsPath" containing
#' analytics data (check with: readxl::excel_sheets(xlsPath))
#' @param col_names do the (default: TRUE)
#' (default: "DateTime"),
#' @param col_rawData_pattern specify pattenr of columns containing raw data
#' (default: "raw")
#' @param col_ignore_pattern  specify pattern of columns that should be ignored
#' of importing (default: "mean|empty|X_|RX|not_used")
#' @param skip number of rows in sheet to skip (default: 69),
#' @param tz_org  specify timezone of samples  (default: "UTC")
#' @param tz_export specify timezone for data export (default: "UTC")
#' @return returns data frame with normalised analytics data in list form
#' @import readxl tidyr dplyr
#' @importFrom kwb.utils stringList
#' @keywords internal
import_sheet <- function(
  xlsPath,
  sheet,
  col_names = TRUE,
  col_rawData_pattern = "raw",
  col_ignore_pattern = "mean|empty|X_|RX|not_used",
  skip = 69,
  tz_org = "UTC",
  tz_export = "UTC"
)
{
  ### Read original EXCEL sheet
  tmp_par1 <- readxl::read_excel(
    path = xlsPath,
    sheet = sheet,
    col_names = col_names,
    skip = skip
  )
  
  names(tmp_par1)[1L] <- "DateTime"
  
  ### Check if all data points in first column are of type DATE/TIME
  if (is.character(tmp_par1$DateTime)) {
    
    date_time_entries <- tmp_par1$DateTime[! is.na(tmp_par1$DateTime)]
    
    non_datetime_indices <- is.na(suppressWarnings(as.numeric(date_time_entries)))
    
    non_datetime_values <- date_time_entries[non_datetime_indices]
    
    msg <- sprintf(
      "All data values in first column need to be of type 'DATE/TIME'\n
                    The following value(s) do not satisfy this condition: %s\n
                    Please check/correct the value(s) in sheet '%s' of imported xls file '%s'!",
      kwb.utils::stringList(non_datetime_values),
      sheet,
      xlsPath
    )
    
    clean_stop(msg)
  }
  
  ### Fill missing date/time entries in case samples were taken
  ### (for details: see function: fill_datetime)
  
  ### Ignore columns without headers:
  cols_with_headers <- which(names(tmp_par1) != "")
  
  tmp_par2 <- fill_datetime(
    tmp_par1[, cols_with_headers],
    col_rawData_pattern = col_rawData_pattern
  ) %>%
    dplyr::filter_(~ totSamples > 0L)
  
  ### Define time zone of samples
  tmp_par3 <- set_timezone(tmp_par2, tz = tz_org)
  
  ### Define time zone be be used for export
  tmp_par4 <- change_timezone(tmp_par3, tz = tz_export)
  
  col_import <- ! grepl(pattern = col_ignore_pattern, x = names(tmp_par4))
  
  tmp_par5 <- tmp_par4[[col_import]]
  
  col_values <- grep("@", names(tmp_par5), value = TRUE)
  
  tmp_par5_list <- tidyr::gather_(
    data = tmp_par5,
    key_col = "Keys",
    value_col = "ParameterValue",
    gather_cols = col_values
  )
  
  tmp_par6_list <- tidyr::separate_(
    tmp_par5_list,
    col = "Keys",
    into = c("ParameterCode", "SiteCode", "DataType"),
    sep = "@",
    remove = TRUE
  )
  
  ### Remove rows with NA as ParameterValue
  tmp_par7_list <- tmp_par6_list %>%
    dplyr::filter_("!is.na(ParameterValue)")
  
  ### Cast to numeric just in case EXCEL data is imported as CHARACTER
  tmp_par7_list$ParameterValue <- suppressWarnings(
    as.numeric(tmp_par7_list$ParameterValue)
  )
  
  non_numeric_paravals <- tmp_par7_list$ParameterValue[is.na(tmp_par7_list$ParameterValue)]
  
  ### Check if all parameter values are of type NUMERIC
  if (any(non_numeric_paravals)) {
    
    msg <- sprintf(
      "All parameter values need to be numeric!\n
                   The following value(s) do not satisfy this condition: %s\n
                   Please check/correct the value(s) in sheet '%s' of imported xls file '%s'!",
      kwb.utils::stringList(non_numeric_paravals),
      sheet,
      xlsPath
    )
    
    clean_stop(msg)
  }
  
  tmp_par7_list
}

#' Imports multiple analytics sheets from an EXCEL spreadsheet
#' @param xlsPath path to xls file with analytics data
#' @param sheets_analytics a character vector with the names of the sheets
#' with analytics data (check with: readxl::excel_sheets(xlsPath))
#' @param sheet_parameters sheet name containing parameter metadata (default:
#' "Parameters")
#' @param sheet_sites sheet name containing sites metadata (default:
#' "Sites")
#' @param sheet_location sheet name containing location metadata (default:
#' "Location")
#' @param col_rawData_pattern specify pattenr of columns containing raw data
#' (default: "raw")
#' @param col_ignore_pattern  specify pattern of columns that should be ignored
#' of importing (default: "mean|empty|X_|RX|not_used")
#' @param skip number of rows in sheet to skip (default: 69),
#' @param tz_org  specify timezone of samples  (default: "UTC")
#' @param tz_export specify timezone for data export (default: "UTC")
#' @param dbg print debug messages (default: TRUE)
#' @return returns data frame with normalised analytics data in list form
#' @import readxl tidyr dplyr
#' @export
import_sheets <- function(
  xlsPath,
  sheets_analytics,
  sheet_parameters = "Parameters",
  sheet_sites = "Sites",
  sheet_location = "Location",
  col_rawData_pattern = "raw",
  col_ignore_pattern = "mean|empty|X_|RX|not_used",
  ### skip: rows to skip for each sheet
  skip = 69,
  ### tz_org:
  tz_org = NULL,
  ### tz_export:
  tz_export = "UTC",
  dbg = TRUE
)
{
  sites <- readxl::read_excel(path = xlsPath, sheet = sheet_sites)
  location <- readxl::read_excel(path = xlsPath, sheet = sheet_location)
  parameters <- readxl::read_excel(xlsPath, sheet = sheet_parameters)
  
  ### If no explicit time zone for analytics is defined, use value of column "ParameterUnit"
  ### in sheet_parameters with "ParameterCode TZ"
  if (is.null(tz_org)) {
    tz_org <- parameters$ParameterUnit[parameters$ParameterCode == "TZ"]
  }
  
  data_frames <- lapply(seq_along(sheets_analytics), function(sheet_index) {
    
    mySheet <- sheets_analytics[sheet_index]
    
    if (dbg) {
      
      print(sprintf(
        "Importing & normalising analytics sheet: '%s' from '%s'",
        mySheet,
        basename(xlsPath)
      ))
    }
    
    tmp <- import_sheet(
      xlsPath = xlsPath,
      sheet = mySheet,
      col_rawData_pattern = col_rawData_pattern,
      col_ignore_pattern = col_ignore_pattern,
      skip = skip,
      tz_org = tz_org,
      tz_export = tz_export
    )
    
    tmp <- dplyr::left_join(tmp, sites, by = "SiteCode") %>%
      dplyr::left_join(parameters, by = "ParameterCode") %>%
      dplyr::left_join(location, by = "LocationID") %>%
      dplyr::select_(
        ~LocationID,
        ~LocationName,
        ~DateTime,
        ~measurementID,
        # ~Keys,
        ~ParameterCode,
        ~ParameterName,
        # ~ParameterComments,
        ~SiteCode,
        ~SiteName,
        # ~SiteComments,
        ~DataType,
        ~ParameterValue,
        ~ParameterUnit,
        ~Comments,
        ~Who
      )
  })

  do.call(plyr::rbind.fill, data_frames)
}

#' Plot analytics data (in PDF)
#' @param df dataframe as retrieved by import_sheets()
#' @return creates new subdirectory "/report" in current working directory
#' and stores pdf plots there
#' @import ggplot2
#' @importFrom ggforce facet_wrap_paginate
#' @importFrom grDevices dev.off pdf
#' @export
plot_analytics <- function(df)
{
  locIDs <- unique(df$LocationID)
  
  for (loc_index in seq_along(locIDs)) {
    
    tmp <- df[df$LocationID == locIDs[loc_index], ]
    ### Create column "SiteLabel" for plotting (based on SiteCode & SiteName) in
    ### order to introduce an ordered plotting for second plot (starting with SP1:
    ### well water left -> ending with SP4: tank water)
    tmp$SiteLabel <- sprintf("%s (%s)", tmp$SiteCode, tmp$SiteName)
    
    # Calculate the number of pages (based on unique ParameterNames)
    n_pages <- length(unique(tmp[["ParameterName"]]))
    
    title_label <- sprintf(
      "Location: %s (ID: %s)",
      unique(tmp$LocationName),
      unique(tmp$LocationID)
    )
    
    ### 1) Time series plot for each substance
    pdfDir <- "report"
    dir.create(pdfDir)
    
    grDevices::pdf(
      file = file.path(pdfDir, sprintf("%d_analytics_timeSeries.pdf", locIDs[loc_index])),
      width = 10,
      height = 7
    )
    
    for (i in seq_len(n_pages)) {
      
      g1 <- ggplot2::ggplot(tmp, aes_string(
        x = "DateTime",
        y = "ParameterValue",
        col = "SiteLabel"
      )) +
        ggforce::facet_wrap_paginate(
          ~ParameterName,
          nrow = 1,
          ncol = 1,
          scales = "free_y",
          page = i
        ) +
        ggplot2::geom_point() +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "top") +
        ggplot2::labs(title = title_label)
      
      print(g1)
    }
    
    grDevices::dev.off()
    
    ### 2) All values per monitoring location for each substance
    grDevices::pdf(
      file = file.path(pdfDir, sprintf("%d_analytics_allSites_onePlot.pdf", locIDs[loc_index])),
      width = 10,
      height = 7
    )
    
    for (i in seq_len(n_pages)) {
      
      g2 <- ggplot2::ggplot(tmp, aes_string(
        x = "SiteLabel",
        y = "ParameterValue",
        col = "SiteLabel"
      )) +
        ggforce::facet_wrap_paginate(
          ~ParameterName,
          nrow = 1,
          ncol = 1,
          scales = "free_y",
          page = i
        ) +
        ggplot2::geom_jitter(width = 0.05, height = 0, alpha = 0.4) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "top") +
        ggplot2::labs(title = title_label)
      
      print(g2)
    }
    
    grDevices::dev.off()
  }
}
