#' Create WEDECO metafile data
#' @param raw_data_file file path to raw data which should be used for as template
#' for meta file creation
#' @return data.frame with meta data file structure
#' @importFrom data.table fread
#' @importFrom tidyr separate
#' @export
create_wedeco_metafile <- function(raw_data_file) {
  print(paste("Importing raw data file:", raw_data_file))
  ozone <- data.table::fread(
    input = raw_data_file,
    header = TRUE,
    sep = ";",
    skip = 2
  )


  indices_names <- seq(from = 1, by = 2, to = ncol(ozone) - 1)
  indices_units <- seq(from = 2, by = 2, to = ncol(ozone))

  meta_file <- data.frame(
    ParameterCode = rep(NA, length(indices_names)),
    ParameterName = rep(NA, length(indices_names)),
    SiteCode = rep(NA, length(indices_names)),
    SiteName = rep(NA, length(indices_names)),
    ParameterName_SiteName = names(ozone)[indices_names],
    ParameterUnitOrg = names(ozone)[indices_units],
    ParameterUnit = names(ozone)[indices_units],
    stringsAsFactors = FALSE
  ) %>%
    tidyr::separate(
      col = "ParameterName_SiteName",
      into = c(
        "ProzessSignal",
        "ProzessID",
        "ProzessName"
      ),
      sep = " - ",
      remove = FALSE
    )

  meta_file$ProzessID <- as.numeric(meta_file$ProzessID)

  meta_file$ParameterUnit <- sub(
    pattern = "_",
    replacement = "/",
    x = meta_file$ParameterUnit
  )

  meta_file$ParameterUnit <- sub(
    pattern = "V.*",
    replacement = "",
    x = meta_file$ParameterUnit
  )



  return(meta_file)
}



#' Import WEDECO raw data
#' @param raw_data_dir path to raw data directory
#' @param raw_data_files vector with full path to operational raw data files that
#' allows to limit import to specific files (default: NULL). If specified parameter
#' "raw_data_dir" will not be used
#' @param meta_file_path path to meta data file
#' @importFrom stringr str_sub
#' @importFrom lubridate parse_date_time2
#' @importFrom data.table rbindlist
#' @export
read_wedeco_data <- function(raw_data_dir = system.file(
                             "shiny/berlin_s/data/operation",
                             package = "kwb.pilot"
                           ),
                           raw_data_files = NULL,
                           meta_file_path = system.file(
                             "shiny/berlin_s/data/parameter_site_metadata.csv",
                             package = "kwb.pilot"
                           )) {

  meta_data <- read.csv(
    file = meta_file_path,
    stringsAsFactors = FALSE
  ) %>%
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


  if (is.null(raw_data_files))  {
  files_to_import <- list.files(
    raw_data_dir,
    pattern = ".csv",
    full.names = TRUE)
  } else {
    files_to_import <- raw_data_files
    }


  raw_list <- lapply(
    files_to_import,
    FUN = function(pathfile) {
      print(paste("Importing raw data file:", pathfile))
      ozone <- data.table::fread(
        input = pathfile,
        header = TRUE,
        sep = ";",
        dec = ",",
        fill = TRUE,
        skip = 2
      )

      indices_names <- seq(from = 1, by = 2, to = ncol(ozone) - 1)
      indices_units <- seq(from = 2, by = 2, to = ncol(ozone))


      process_ids <- stringr::str_sub(names(ozone)[indices_names], 6, 11)

      names(ozone)[indices_units] <- process_ids

      ozone <- ozone[, c(1, indices_units), with = FALSE]


      names(ozone)[1] <- "DateTime"


      ozone$DateTime <- lubridate::parse_date_time2(
        ozone$DateTime,
        orders = "d!.m!*.y!* H!:M!:S!",
        tz = "CET"
      )

      relevant_paras <- names(ozone)[names(ozone) %in% c("DateTime",
                                                         meta_data$ProzessID[meta_data$ZeroOne == 1])]



       ozone[, ..relevant_paras] %>%
        tidyr::gather_(
          key_col = "ProzessID",
          value_col = "ParameterValue",
          gather_cols = setdiff(relevant_paras, "DateTime")
        ) %>%
        dplyr::mutate_(ProzessID = "as.numeric(ProzessID)")

    })


  meta_data <- meta_data %>%
    select_(.dots = "-ZeroOne")

  df_tidy <-  data.table::rbindlist(
              l = raw_list,
              use.names = TRUE) %>%
              dplyr::left_join(y = meta_data) %>%
              as.data.frame()


  df_tidy$Source <- "online"

  no_sitenames <- is.na(df_tidy$SiteName) | df_tidy$SiteName == ""

  df_tidy$SiteName[no_sitenames] <- "General"


  return(df_tidy)
}

#' Import data for Berlin Schoenerlinde
#' @param raw_data_dir path of directory containing WEDECO CSV files (default:
#' (default: system.file("shiny/berlin_s/data/operation",
#' package = "kwb.pilot"))))
#' @param raw_data_files vector with full path to operational raw data files that
#' allows to limit import to specific files (default: NULL). If specified parameter
#' "raw_data_dir" will not be used
#' @param meta_file_path path to metadata file (default:
#' system.file("shiny/berlin_s/data/parameter_site_metadata.csv", package =
#' "kwb.pilot")))
#' @return list with "df": data.frame with imported operational data (analytics
#' data to be added as soon as available) and "added_data_points": number of
#' added data points in case of existing fst file was updated with new operational
#' data
#' @export
import_data_berlin_s <- function(raw_data_dir = system.file(
                                 "shiny/berlin_s/data/operation",
                                 package = "kwb.pilot"
                               ),
                               raw_data_files = NULL,
                               meta_file_path = system.file(
                                 "shiny/berlin_s/data/parameter_site_metadata.csv",
                                 package = "kwb.pilot"
                               )) {
  data_berlin_s <- read_wedeco_data(
    raw_data_dir = raw_data_dir,
    raw_data_files = raw_data_files,
    meta_file_path = meta_file_path
  )


  data_berlin_s$DataType <- "raw"


  data_berlin_s$SiteName_ParaName_Unit <- sprintf(
    "%s: %s (%s)",
    data_berlin_s$SiteName,
    data_berlin_s$ParameterName,
    data_berlin_s$ParameterUnit
  )


  ### Remove duplicates if any exist
  data_berlin_s <- remove_duplicates(
    df = data_berlin_s,
    col_names = c("DateTime", "ParameterCode", "SiteCode")
  )

  return(data_berlin_s)
}
