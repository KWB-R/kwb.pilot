#' BerlinTiefwerder: import lab data
#' @param xlsx_path  full path to lab data EXCEL file in xlsx format
#' (default: kwb.pilot:::package_file("shiny/berlin_t/data/analytics.xlsx"))
#' @return a list of imported lab data for Berlin-Tiefwerder
#' @import tidyr
#' @import dplyr
#' @importFrom readxl read_xlsx
#' @importFrom magrittr "%>%"
#' @export
import_lab_data_berlin_t <- function(
  xlsx_path = package_file("shiny/berlin_t/data/analytics.xlsx")
)
{
  lab_results <- readxl::read_xlsx(
    path = xlsx_path,
    sheet = "Tabelle1",
    skip = 12
  ) %>%
    dplyr::mutate_(ParameterName = gsub(pattern = "\\s*\\(.*", "", "ParameterCode"))


  lab_results_list <- lab_results %>%
    tidyr::gather_(
      key_col = "Combi",
      value_col = "ParameterValueRaw",
      gather_cols = setdiff(
        names(lab_results),
        c(
          "ParameterCode",
          "ParameterUnit",
          "ParameterName"
        )
      )
    ) %>%
    tidyr::separate_(
      col = "Combi",
      into = c(
        "ProbenNr",
        "Date",
        "Termin",
        "Komplexkuerzel",
        "Ort_Typ",
        "Art",
        "Gegenstand",
        "Bezeichnung",
        "SiteName",
        "InterneKN",
        "Bemerkung",
        "DateTime"
      ),
      sep = "@",
      remove = TRUE
    ) %>%
    dplyr::mutate_(Date = "as.numeric(Date)") %>%
    dplyr::mutate_(Date = "janitor::excel_numeric_to_date(date_num = Date,
                                                        date_system = 'modern')") %>%
    dplyr::mutate_(Termin = "as.numeric(Termin)") %>%
    dplyr::mutate_(Termin = "janitor::excel_numeric_to_date(date_num = Termin,
                                                          date_system = 'modern')") %>%
    dplyr::mutate_(DateTime = "gsub(',', '.', DateTime)") %>%
    dplyr::mutate_(DateTime = "as.POSIXct(as.numeric(DateTime)*24*3600,
                                        origin = '1899-12-30',
                                        tz = 'CET')") %>%
    dplyr::mutate_(
      ParameterValue = "gsub(',', '.', ParameterValueRaw)",
      DetectionLimit = "ifelse(test = grepl('<', ParameterValue),
                                          yes = 'below',
                                          no = 'above')"
    ) %>%
    dplyr::mutate_(
      DetectionLimit_numeric = "ifelse(test = grepl('<', ParameterValue),
                                                    yes = as.numeric(gsub('<', '', ParameterValue)),
                                                    no = NA)",
      ParameterValue = "ifelse(test = grepl('<', ParameterValue),
                                          yes = as.numeric(gsub('<', '', ParameterValue))/2,
                                          no = as.numeric(ParameterValue))"
    )


  site_names <- unique(lab_results_list$SiteName)

  site_meta <- data.frame(
    SiteCode = seq_along(site_names),
    SiteName = site_names,
    stringsAsFactors = FALSE
  )

  lab_results_list <- lab_results_list %>%
    dplyr::left_join(site_meta) %>%
    dplyr::mutate(Source = "offline")



  res <- list(
    matrix = lab_results,
    list = lab_results_list
  )

  return(res)
}


#' Read PENTAIR operational data
#' @param raw_data_dir path of directory containing PENTAIR xls files 
#' (default: kwb.pilot:::package_file("shiny/berlin_t/data/operation"))
#' @param raw_data_files vector with full path to operational raw data files that
#' allows to limit import to specific files (default: NULL). If specified parameter
#' "raw_data_dir" will not be used
#' @param meta_file_path path to metadata file (default:
#' kwb.pilot:::package_file("shiny/berlin_t/data/parameter_site_metadata.csv"))
#' @return data.frame with imported PENTAIR operational data
#' @import tidyr
#' @importFrom readr read_tsv
#' @importFrom magrittr "%>%"
#' @importFrom data.table rbindlist
#' @export
read_pentair_data <- function(
  raw_data_dir = package_file("shiny/berlin_t/data/operation"),
  raw_data_files = NULL,
  meta_file_path = package_file("shiny/berlin_t/data/parameter_site_metadata.csv")
)
{
  meta_data <- read.csv(
    file = meta_file_path,
    header = TRUE,
    sep = ",",
    dec = ".",
    stringsAsFactors = FALSE
  )


  meta_data$ParameterLabel <- sprintf(
    "%s (%s)",
    meta_data$ParameterName,
    meta_data$ParameterUnit
  )


  if (is.null(raw_data_files)) {
    xls_files <- list.files(
      path = raw_data_dir,
      pattern = "*.xls",
      full.names = TRUE
    )
  } else {
    xls_files <- raw_data_files
  }

  raw_list <- lapply(
    xls_files,
    FUN = function(xls_file) {
      print(paste("Importing raw data file:", xls_file))
      tmp <- readr::read_tsv(
        file = xls_file,
        locale = readr::locale(tz = "CET")
      )
      relevant_paras <- names(tmp)[names(tmp) %in%
        c("TimeStamp", meta_data$ParameterCode[meta_data$ZeroOne == 1])]
      tmp[, relevant_paras]
    }
  )


  df_tidy <- data.table::rbindlist(
    l = raw_list,
    use.names = TRUE
  ) %>%
    tidyr::gather_(
      key_col = "ParameterCode",
      value_col = "ParameterValue",
      gather_cols = setdiff(names(raw_list[[1]]), "TimeStamp")
    ) %>%
    dplyr::rename_(DateTime = "TimeStamp") %>%
    dplyr::left_join(y = meta_data %>%
      select_(.dots = "-ZeroOne")) %>%
    as.data.frame()

  df_tidy$Source <- "online"

  no_sitenames <- is.na(df_tidy$SiteName)

  df_tidy$SiteName[no_sitenames] <- "General"

  return(df_tidy)
}

#' Import data for Berlin Tiefwerder
#' @param raw_data_dir path of directory containing PENTAIR xls files 
#' (default: kwb.pilot:::package_file("shiny/berlin_t/data/operation"))
#' @param raw_data_files vector with full path to operational raw data files that
#' allows to limit import to specific files (default: NULL). If specified parameter
#' "raw_data_dir" will not be used
#' @param analytics_path  full path to lab data EXCEL file in xlsx format 
#' (default: kwb.pilot:::package_file("shiny/berlin_t/data/analytics.xlsx"))
#' @param meta_file_path path to metadata file (default:
#' kwb.pilot:::package_file("shiny/berlin_t/data/parameter_site_metadata.csv"))
#' @return data.frame with imported operational data (analytics´data to be added as
#' soon as available)
#' @export
import_data_berlin_t <- function(
  raw_data_dir = package_file("shiny/berlin_t/data/operation"),
  raw_data_files = NULL,
  analytics_path = package_file("shiny/berlin_t/data/analytics.xlsx"),
  meta_file_path = package_file("shiny/berlin_t/data/parameter_site_metadata.csv")
)
{
  data_berlin_t <- read_pentair_data(
    raw_data_dir = raw_data_dir,
    raw_data_files = raw_data_files,
    meta_file_path = meta_file_path
  )

  #### To do: joind with ANALYTICS data as soon as available
  # data_berlin_t_offline <- read_pentair_data(raw_data_dir = raw_data_dir,
  #                                    meta_file_path = meta_file_path)

  # data_berlin_t_offline <- import_lab_data_berlin_t(raw_data_dir = raw_data_dir,
  #                                           meta_file_path = meta_file_path)


  data_berlin_t$DataType <- "raw"


  data_berlin_t$SiteName_ParaName_Unit <- sprintf(
    "%s: %s (%s)",
    data_berlin_t$SiteName,
    data_berlin_t$ParameterName,
    data_berlin_t$ParameterUnit
  )


  ### Remove duplicates if any exist
  data_berlin_t <- remove_duplicates(
    df = data_berlin_t,
    col_names = c("DateTime", "ParameterCode", "SiteCode")
  )

  return(data_berlin_t)
}
