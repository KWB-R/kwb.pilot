#' Import data for Berlin Friedrichshagen
#' @param raw_data_dir path of directory containing Weintek xlsx files 
#' (default: kwb.pilot:::package_file("shiny/berlin_f/data/operation"))
#' @param raw_data_files vector with full path to operational raw data files that
#' allows to limit import to specific files (default: NULL). If specified parameter
#' "raw_data_dir" will not be used
#' @param analytics_path  full path to lab data EXCEL file in xlsx format 
#' (default: kwb.pilot:::package_file("shiny/berlin_f/data/analytics.xlsx"))
#' @param meta_file_path path to metadata file (default:
#' kwb.pilot:::package_file("shiny/berlin_f/data/parameter_site_metadata.csv"))
#' @return data.frame with imported operational data (analytics´data to be added as
#' soon as available)
#' @export
import_data_berlin_f <- function(
  raw_data_dir = package_file("shiny/berlin_f/data/operation"),
  raw_data_files = NULL,
  analytics_path = package_file("shiny/berlin_f/data/analytics.xlsx"),
  meta_file_path = package_file("shiny/berlin_f/data/parameter_site_metadata.csv")
)
{
  data_berlin_f <- read_pentair_data(
    raw_data_dir = raw_data_dir,
    raw_data_files = raw_data_files,
    meta_file_path = meta_file_path
  )
  
  #### To do: joind with ANALYTICS data as soon as available
  # data_berlin_f_offline <- read_pentair_data(raw_data_dir = raw_data_dir,
  #                                    meta_file_path = meta_file_path)
  
  # data_berlin_f_offline <- import_lab_data_berlin_f(raw_data_dir = raw_data_dir,
  #                                           meta_file_path = meta_file_path)
  
  
  data_berlin_f$DataType <- "raw"
  
  
  data_berlin_f$SiteName_ParaName_Unit <- sprintf(
    "%s: %s (%s)",
    data_berlin_f$SiteName,
    data_berlin_f$ParameterName,
    data_berlin_f$ParameterUnit
  )
  
  
  ### Remove duplicates if any exist
  data_berlin_f <- remove_duplicates(
    df = data_berlin_f,
    col_names = c("DateTime", "ParameterCode", "SiteCode")
  )
  
  return(data_berlin_f)
}
