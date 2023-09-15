# import_lab_data_berlin_t -----------------------------------------------------

#' BerlinTiefwerder: import lab data
#' 
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
    readxl::read_xlsx(sheet = "Tabelle1", skip = 12L) %>%
    dplyr::mutate_(ParameterName = gsub("\\s*\\(.*", "", "ParameterCode"))
  
  parameters <- setdiff(
    names(lab_results), 
    c("ParameterCode", "ParameterUnit", "ParameterName")
  )
  
  sep_into <- c(
    "ProbenNr", "Date", "Termin", "Komplexkuerzel", "Ort_Typ", "Art",
    "Gegenstand", "Bezeichnung", "SiteName", "InterneKN", "Bemerkung",
    "DateTime"
  )
  
  df <- lab_results %>%
    tidyr::gather_("Combi", "ParameterValueRaw", parameters) %>%
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

# read_pentair_data ------------------------------------------------------------

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
  
  meta_data <- if (file.exists(meta_file_path)) {
    read_pentair_meta_data(meta_file_path)
  } # else NULL
  
  df_tidy <- xls_files %>%
    lapply(function(xls_file) {
      kwb.utils::catAndRun(
        paste("Importing raw data file:", xls_file), 
        expr = {
          data <- readr::read_tsv(
            file = xls_file, 
            locale = locale, 
            col_types = col_types
          )
          if (is.null(meta_data)) {
            data
          } else {
            is_active <- meta_data$ZeroOne == 1
            parameters <- meta_data$ParameterCode[is_active]
            data[, intersect(names(data), c("TimeStamp", parameters))]
          }
        }
      )
    }) %>%
    data.table::rbindlist(use.names = TRUE, fill = TRUE)
  
  parameters <- setdiff(names(df_tidy), "TimeStamp")

  if (is.null(meta_data)) {
    meta_data <- write_default_pentair_meta_data(
      parameters = parameters, 
      target_dir = raw_data_dir
    )
  }
  
  meta_data$ParameterLabel <- sprintf_columns(
    "%s (%s)", 
    meta_data, 
    columns = c("ParameterName", "ParameterUnit")
  )
  
  df_tidy <- df_tidy %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(parameters),
      names_to = "ParameterCode", 
      values_to = "ParameterValue"
    ) %>%
    dplyr::rename(
      DateTime = "TimeStamp"
    ) %>%
    dplyr::left_join(
      y = kwb.utils::removeColumns(meta_data, "ZeroOne")
    ) %>%
    as.data.frame()
  
  df_tidy$Source <- "online"
  
  df_tidy$SiteName[is.na(df_tidy$SiteName)] <- "General"
  
  df_tidy
}

# read_pentair_meta_data -------------------------------------------------------
read_pentair_meta_data <- function(file)
{
  read.csv(
    file = file, 
    header = TRUE, 
    sep = ",", 
    dec = ".",
    stringsAsFactors = FALSE
  )
}

# write_default_pentair_meta_data ----------------------------------------------
write_default_pentair_meta_data <- function(parameters, target_dir)
{
  meta_data <- tibble::tibble(
    ParameterCode = parameters, 
    ParameterName = parameters, 
    ParameterUnit = "", 
    SiteCode = "", 
    SiteName = "", 
    ZeroOne = 1L
  )
  
  file <- file.path(target_dir, "parameter_site_metadata_dummy.csv")
    
  kwb.utils::catAndRun(
    paste("No metadata file provided.", sprintf(
      "Generating and exporting dummy metadata file to '%s'.", file
    )),
    expr = write.csv(meta_data, file = file, row.names = FALSE)
  )
  
  # Return the meta data
  meta_data
}

# import_data_berlin_t ---------------------------------------------------------

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
  
  # TODO: join with ANALYTICS data as soon as available
  
  # data_berlin_t_offline <- read_pentair_data(
  #   raw_data_dir = raw_data_dir,
  #   meta_file_path = meta_file_path
  # )
  
  # data_berlin_t_offline <- import_lab_data_berlin_t(
  #   raw_data_dir = raw_data_dir,
  #   meta_file_path = meta_file_path
  # )
  
  df$DataType <- "raw"
  
  df$SiteName_ParaName_Unit <- sprintf_columns(
    "%s: %s (%s)", 
    df, 
    columns = c("SiteName", "ParameterName", "ParameterUnit")
  )
  
  # Remove duplicates if any exist
  remove_duplicates(
    df, 
    col_names = c("DateTime", "ParameterCode", "SiteCode")
  )
}
