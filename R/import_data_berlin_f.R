#' Import data for Berlin Friedrichshagen

#' @param raw_data_files vector with full path to operational raw data files that
#' allows to limit import to specific files (default:
#' fs::dir_ls(kwb.pilot:::shiny_file("berlin_f/data/raw/online_data""), recurse = TRUE,
#' regexp = "^[^~].*\\.xlsx$")).
#' @param meta_file_path path to metadata file (default:
#' kwb.pilot:::shiny_file("berlin_f/data/raw/online_data/parameter_site_metadata.csv"))
#' @return data.frame with imported operational data (analytics´data to be added as
#' soon as available)
#' @export
#' @importFrom fs dir_ls
import_data_berlin_f <- function(
  raw_data_files = fs::dir_ls(shiny_file("berlin_f/data/raw/online_data"), recurse = TRUE, regexp = "^[^~].*\\.xlsx$"),
  meta_file_path = shiny_file("berlin_f/data/raw/online_data/parameter_unit_metadata.csv")
)
{
  # kwb.utils::assignPackageObjects("kwb.pilot")

  # =======
  #   raw_data_dir = shiny_file("berlin_f/data/operation"),
  #   raw_data_files = fs::dir_ls(raw_data_dir, recurse = TRUE, regexp = "^[^~].*\\.xlsx$"),
  #   analytics_path = shiny_file("berlin_f/data/analytics.xlsx"),
  #   meta_file_path = shiny_file("berlin_f/data/parameter_site_metadata.csv")
  # )
  # {

  #### To do: joind with ANALYTICS data as soon as available
  # data_berlin_f_offline <- read_pentair_data(raw_data_dir = raw_data_dir,
  #                                    meta_file_path = meta_file_path)

  # data_berlin_f_offline <- import_lab_data_berlin_f(raw_data_dir = raw_data_dir,
  #                                           meta_file_path = meta_file_path)

  meta_data <- readr::read_csv(meta_file_path,
    col_types = "cc",
    locale = readr::locale(encoding = "UTF-8")
  )

  data_berlin_f <- read_weintek_batch(raw_data_files) %>%
    dplyr::left_join(meta_data) %>%
    dplyr::mutate(
      DataType = "raw",
      ParameterCode = sprintf(
        "%s_%s",
        .data$ParameterName,
        .data$SiteName
      ),
      SiteName_ParaName_Unit = sprintf(
        "%s: %s (%s)",
        .data$SiteName,
        .data$ParameterName,
        .data$ParameterUnit
      )
    ) %>%
    ### Remove duplicates if any exist
    remove_duplicates(col_names = c("DateTime", "ParameterName", "SiteName"))

  Encoding(data_berlin_f$ParameterUnit) <- "UTF-8"

  #   data_berlin_f <- remove_duplicates(
  #     df = data_berlin_f,
  #     col_names = c("DateTime", "ParameterName", "SiteName")
  #   )

  data_berlin_f
}
