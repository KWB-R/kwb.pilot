#' MBR 4.0 Data Tidy
#'
#' @param mbr4_data tibble as retrieved by \code{read_mbr4}
#' @param path_metadata path to metadata file (default: 
#' kwb.pilot:::shiny_file("mbr4.0/data/metadata.csv"))
#' @return tidy MBR 4 data in long format
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom readr read_csv cols
#' @importFrom dplyr rename filter left_join
#' @export
#' @examples
#' \dontrun{
#' mbr4_data <- read_mbr4()
#' mbr4_data_tidy <- kwb.pilot::tidy_mbr4_data(mbr4_data)
#' }
tidy_mbr4_data <- function(
  mbr4_data,
  path_metadata = shiny_file("mbr4.0/data/metadata.csv")
)
{
  remove_cols <- c("zustand", "meldungen", "id")
  
  keep_cols <- c("Zeitstempel")

  metadata <- readr::read_csv(
    file = path_metadata, 
    col_types = readr::cols(.default = "c")
  )
  
  mbr4_data %>%
    dplyr::select(!tidyselect::all_of(remove_cols)) %>% 
    remove_duplicates() %>% 
    tidyr::pivot_longer(
      cols = !tidyselect::all_of(keep_cols),
      names_to = "ParameterCode_SiteCode",
      values_to = "ParameterValue"
    ) %>%
    dplyr::filter(!is.na(.data$ParameterValue)) %>%
    dplyr::rename("DateTime" = .data$Zeitstempel) %>%
    dplyr::left_join(metadata, by = "ParameterCode_SiteCode") %>%
    dplyr::mutate(SiteName_ParaName_Unit = sprintf(
      "%s: %s (%s)",
      .data$SiteName,
      .data$ParameterName,
      .data$ParameterUnit
    ))
}
