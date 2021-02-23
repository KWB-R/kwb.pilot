#' Create WEDECO metafile data
#' 
#' @param raw_data_file file path to raw data which should be used for as template
#' for meta file creation
#' @return data.frame with meta data file structure
#' @importFrom data.table fread
#' @importFrom tidyr separate
#' @export
create_wedeco_metafile <- function(raw_data_file)
{
  ozone <- kwb.utils::catAndRun(
    paste("Importing raw data file:", raw_data_file), 
    data.table::fread(input = raw_data_file, header = TRUE, sep = ";", skip = 2)
  )
  
  indices_names <- seq(from = 1, by = 2, to = ncol(ozone) - 1)
  indices_units <- indices_names + 1L

  na_vector <- rep(NA, length(indices_names))
  units <- names(ozone)[indices_units]
                   
  meta_file <- data.frame(
    ParameterCode = na_vector,
    ParameterName = na_vector,
    SiteCode = na_vector,
    SiteName = na_vector,
    ParameterName_SiteName = names(ozone)[indices_names],
    ParameterUnitOrg = units,
    ParameterUnit = units,
    stringsAsFactors = FALSE
  ) %>%
    tidyr::separate(
      col = "ParameterName_SiteName",
      into = c("ProzessSignal", "ProzessID", "ProzessName"),
      sep = " - ",
      remove = FALSE
    )

  meta_file$ProzessID <- as.numeric(meta_file$ProzessID)

  meta_file$ParameterUnit <- kwb.utils::multiSubstitute(
    strings = meta_file$ParameterUnit, 
    replacements = list(
      "_" = "/", 
      "V.*" = ""
    )
  )
  
  meta_file
}

#' Import WEDECO raw data
#' 
#' @param raw_data_dir path to raw data directory
#' @param raw_data_files vector with full path to operational raw data files that
#' allows to limit import to specific files (default: NULL). If specified parameter
#' "raw_data_dir" will not be used
#' @param meta_file_path path to meta data file
#' @importFrom stringr str_sub
#' @importFrom lubridate parse_date_time2
#' @importFrom data.table rbindlist
#' @export
read_wedeco_data <- function(
  raw_data_dir = package_file("shiny/berlin_s/data/operation"),
  raw_data_files = NULL,
  meta_file_path = package_file("shiny/berlin_s/data/parameter_site_metadata.csv")
)
{
  meta_data <- read.csv(meta_file_path, stringsAsFactors = FALSE) %>%
    dplyr::select_(
      "ProzessID",
      "ParameterCode",
      "ParameterName",
      "ParameterUnit",
      "SiteCode",
      "SiteName",
      "ZeroOne"
    )

  meta_data$ParameterLabel <- sprintf(
    "%s (%s)",
    meta_data$ParameterName,
    meta_data$ParameterUnit
  )

  files_to_import <- if (is.null(raw_data_files)) {
    list_full_csv_files(raw_data_dir)
  } else {
    raw_data_files
  }

  raw_list <- lapply(
    files_to_import,
    FUN = function(pathfile) {
      
      print(paste("Importing raw data file:", pathfile))
      
      ozone <- data.table::fread(
        pathfile, header = TRUE, sep = ";", dec = ",", fill = TRUE, skip = 2
      )
      
      indices_names <- seq(from = 1, by = 2, to = ncol(ozone) - 1)
      indices_units <- indices_names + 1L

      process_ids <- stringr::str_sub(names(ozone)[indices_names], 6, 11)

      names(ozone)[indices_units] <- process_ids

      ozone <- ozone[, c(1, indices_units), with = FALSE]

      names(ozone)[1] <- "DateTime"

      ozone$DateTime <- lubridate::parse_date_time2(
        ozone$DateTime,
        orders = "d!.m!*.y!* H!:M!:S!",
        tz = "CET"
      )

      columns <- c("DateTime", meta_data$ProzessID[meta_data$ZeroOne == 1])
      
      relevant_paras <- names(ozone)[names(ozone) %in% columns]
      
      ozone[, relevant_paras] %>%
        tidyr::gather_(
          key_col = "ProzessID",
          value_col = "ParameterValue",
          gather_cols = setdiff(relevant_paras, "DateTime")
        ) %>%
        dplyr::mutate_(ProzessID = "as.numeric(ProzessID)")
    })

  meta_data <- meta_data %>%
    select_(.dots = "-ZeroOne")

  df_tidy <- data.table::rbindlist(l = raw_list, use.names = TRUE) %>%
    dplyr::left_join(y = meta_data) %>%
    as.data.frame()

  df_tidy$Source <- "online"

  df_tidy$SiteName[kwb.utils::isNaOrEmpty(df_tidy$SiteName)] <- "General"

  df_tidy
}

#' Import data for Berlin Schoenerlinde
#' @param raw_data_dir path of directory containing WEDECO CSV files
#' (default: kwb.pilot:::package_file("shiny/berlin_s/data/operation"))
#' @param raw_data_files vector with full path to operational raw data files that
#' allows to limit import to specific files (default: NULL). If specified parameter
#' "raw_data_dir" will not be used
#' @param meta_file_path path to metadata file (default:
#' kwb.pilot:::package_file("shiny/berlin_s/data/parameter_site_metadata.csv"))
#' @return list with "df": data.frame with imported operational data (analytics
#' data to be added as soon as available) and "added_data_points": number of
#' added data points in case of existing fst file was updated with new operational
#' data
#' @export
import_data_berlin_s <- function(
  raw_data_dir = package_file("shiny/berlin_s/data/operation"),
  raw_data_files = NULL,
  meta_file_path = package_file("shiny/berlin_s/data/parameter_site_metadata.csv")
)
{
  df <- read_wedeco_data(raw_data_dir, raw_data_files, meta_file_path)

  df$DataType <- "raw"

  df$SiteName_ParaName_Unit <- sprintf(
    "%s: %s (%s)", df$SiteName, df$ParameterName, df$ParameterUnit
  )

  ### Remove duplicates if any exist
  remove_duplicates(
    df = df, col_names = c("DateTime", "ParameterCode", "SiteCode")
  )
}
