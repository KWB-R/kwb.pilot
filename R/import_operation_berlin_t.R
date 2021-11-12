#' BerlinTiefwerder: import lab data
#' @param xlsx_path  full path to lab data EXCEL file in xlsx format
#' (default: kwb.pilot:::shiny_file("berlin_t/data/analytics.xlsx"))
#' @return a list of imported lab data for Berlin-Tiefwerder
#' @import tidyr
#' @importFrom dplyr left_join mutate
#' @importFrom readxl read_xlsx
#' @importFrom magrittr "%>%"
#' @export
import_lab_data_berlin_t <- function(
  xlsx_path = shiny_file("berlin_t/data/analytics.xlsx")
)
{
  lab_results <- xlsx_path %>%
    readxl::read_xlsx(sheet = "Tabelle1", skip = 12) %>%
    dplyr::mutate_(
      ParameterName = gsub(pattern = "\\s*\\(.*", "", "ParameterCode")
    )
  
  gather_cols <- setdiff(names(lab_results), c(
    "ParameterCode", "ParameterUnit", "ParameterName"
  ))
  
  sep_into <- c(
    "ProbenNr", "Date", "Termin", "Komplexkuerzel", "Ort_Typ", "Art",
    "Gegenstand", "Bezeichnung", "SiteName", "InterneKN", "Bemerkung",
    "DateTime"
  )
  
  df <- lab_results %>%
    tidyr::gather_("Combi", "ParameterValueRaw", gather_cols) %>%
    tidyr::separate_("Combi", sep_into, sep = "@", remove = TRUE)
  
  par_value_raw <- kwb.utils::selectColumns(df, "ParameterValueRaw")
  par_value_txt <- comma_to_dot(par_value_raw)
  par_value_num <- as.numeric(gsub("<", "", par_value_txt))
  
  is_below <- grepl("<", par_value_txt)
  
  df <- kwb.utils::setColumns(
    df,
    Date = column_to_date(df, "Date"),
    Termin = column_to_date(df, "Termin"),
    DateTime = num_column_to_posix_cet(df, "DateTime"),
    DetectionLimit = ifelse(is_below, "below", "above"),
    DetectionLimit_numeric = ifelse(is_below, par_value_num, NA),
    ParameterValue = ifelse(is_below, par_value_num / 2, par_value_num)
  )
  
  site_names <- unique(kwb.utils::selectColumns(df, "SiteName"))
  
  site_meta <- data.frame(
    SiteCode = seq_along(site_names),
    SiteName = site_names,
    stringsAsFactors = FALSE
  )
  
  list(
    matrix = lab_results,
    list = df %>%
      dplyr::left_join(site_meta) %>%
      dplyr::mutate(Source = "offline")
  )
}

#' Read PENTAIR operational data
#'
#' @param raw_data_dir path of directory containing PENTAIR xls files
#' (default: kwb.pilot:::shiny_file("berlin_t/data/operation"))
#' @param raw_data_files vector with full path to operational raw data files that
#' allows to limit import to specific files (default: NULL). If specified parameter
#' "raw_data_dir" will not be used
#' @param meta_file_path path to metadata file (default:
#' kwb.pilot:::shiny_file("berlin_t/data/parameter_site_metadata.csv"))
#' @param locale locale (default: \code{\link[readr]{locale}}(tz = "CET"))
#' @param col_types col_types (default: \code{\link[readr]{cols}})
#' @return data.frame with imported PENTAIR operational data
#' @import tidyr
#' @importFrom readr cols locale read_tsv
#' @importFrom magrittr "%>%"
#' @importFrom data.table rbindlist
#' @importFrom kwb.utils catAndRun
#' @importFrom utils write.csv
#' @export
read_pentair_data <- function(
  raw_data_dir = shiny_file("berlin_t/data/operation"),
  raw_data_files = NULL,
  meta_file_path = shiny_file("berlin_t/data/parameter_site_metadata.csv"),
  locale = readr::locale(tz = "CET"),
  col_types = readr::cols()
)
{
  xls_files <- if (is.null(raw_data_files)) {
    list_full_xls_files(raw_data_dir)
  } else {
    raw_data_files
  }
  
  if (file.exists(meta_file_path)) {
    
    meta_data <- read.csv(
      file = meta_file_path, 
      header = TRUE, 
      sep = ",", 
      dec = ".",
      stringsAsFactors = FALSE
    )
    
    columns <- c("TimeStamp", meta_data$ParameterCode[meta_data$ZeroOne == 1])
    
    raw_list <- lapply(xls_files, FUN = function(xls_file) {
      
      print(paste("Importing raw data file:", xls_file))
      
      tmp <- readr::read_tsv(
        file = xls_file, locale = locale, col_types = col_types
      )
      
      relevant_paras <- names(tmp)[names(tmp) %in% columns]
      tmp[, relevant_paras]
      
      df_tidy <- data.table::rbindlist(raw_list, use.names = TRUE, fill = TRUE)
      
      gather_cols <- setdiff(names(df_tidy), "TimeStamp")
    })
    
  } else {
    
    raw_list <- lapply(xls_files, FUN = function(xls_file) {
      
      print(paste("Importing raw data file:", xls_file))
      
      tmp <- readr::read_tsv(
        file = xls_file, 
        locale = locale, 
        col_types = col_types
      )
    })
    
    df_tidy <- data.table::rbindlist(l = raw_list, use.names = TRUE, fill = TRUE)
    
    gather_cols <- setdiff(names(df_tidy), "TimeStamp")
    
    meta_data <- tibble::tibble(
      ParameterCode = gather_cols, 
      ParameterName = gather_cols, 
      ParameterUnit = "", 
      SiteCode = "", 
      SiteName = "", 
      ZeroOne = 1
    )
    
    meta_path <- file.path(raw_data_dir, "parameter_site_metadata_dummy.csv")
    
    msg_text <- sprintf(
      "No metadata file provided. Generating and exporting dummy metadata file to '%s'.",
      meta_path
    )
    
    kwb.utils::catAndRun(messageText = msg_text, expr = { 
      write.csv(meta_data, file = meta_path, row.names = FALSE)
    })
  }
  
  meta_data$ParameterLabel <- sprintf_columns(
    "%s (%s)", 
    meta_data, 
    columns = c("ParameterName", "ParameterUnit")
  )
  
  df_tidy <- data.table::rbindlist(l = raw_list, use.names = TRUE, fill = TRUE)
  
  gather_cols <- setdiff(names(df_tidy), "TimeStamp")
  
  df_tidy <-  df_tidy %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(gather_cols),
      names_to = "ParameterCode", 
      values_to = "ParameterValue"
    ) %>%
    dplyr::rename(
      DateTime = "TimeStamp"
    ) %>%
    dplyr::left_join(
      y = meta_data %>% dplyr::select(-tidyselect::matches("ZeroOne"))
    ) %>%
    as.data.frame()
  
  df_tidy$Source <- "online"
  
  df_tidy$SiteName[is.na(df_tidy$SiteName)] <- "General"
  
  df_tidy
}

#' Import data for Berlin Tiefwerder
#'
#' @param raw_data_dir path of directory containing PENTAIR xls files
#' (default: kwb.pilot:::shiny_file("berlin_t/data/operation"))
#' @param raw_data_files vector with full path to operational raw data files that
#' allows to limit import to specific files (default: NULL). If specified parameter
#' "raw_data_dir" will not be used
#' @param analytics_path  full path to lab data EXCEL file in xlsx format
#' (default: kwb.pilot:::shiny_file("berlin_t/data/analytics.xlsx"))
#' @param meta_file_path path to metadata file (default:
#' kwb.pilot:::shiny_file("berlin_t/data/parameter_site_metadata.csv"))
#' @return data.frame with imported operational data (analytics´data to be added as
#' soon as available)
#' @export
import_data_berlin_t <- function(
  raw_data_dir = shiny_file("berlin_t/data/operation"),
  raw_data_files = NULL,
  analytics_path = shiny_file("berlin_t/data/analytics.xlsx"),
  meta_file_path = shiny_file("berlin_t/data/parameter_site_metadata.csv")
)
{
  df <- read_pentair_data(raw_data_dir, raw_data_files, meta_file_path)
  
  #### To do: joind with ANALYTICS data as soon as available
  # data_berlin_t_offline <- read_pentair_data(raw_data_dir = raw_data_dir,
  #                                    meta_file_path = meta_file_path)
  
  # data_berlin_t_offline <- import_lab_data_berlin_t(raw_data_dir = raw_data_dir,
  #                                           meta_file_path = meta_file_path)
  
  df$DataType <- "raw"
  
  df$SiteName_ParaName_Unit <- sprintf_columns("%s: %s (%s)", df, columns = c(
    "SiteName", "ParameterName", "ParameterUnit"
  ))
  
  ### Remove duplicates if any exist
  remove_duplicates(df, col_names = c(
    "DateTime", "ParameterCode", "SiteCode"
  ))
}
