#' MBR 4.0 Data Tidy
#'
#' @param mbr4_data tibble as retrieved by \link{read_mbr4}
#'
#' @return tidy MBR 4 data in long format
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom readr read_csv cols
#' @importFrom dplyr rename filter left_join
#' @examples
#' \dontrun{
#' mbr4_data <- read_mbr4()
#' mbr4_data_tidy <- kwb.pilot::tidy_mbr4_data(mbr4_data)
#' }
tidy_mbr4_data <- function(mbr4_data,
                           path_metadata = kwb.pilot:::shiny_file("mbr4.0/data/metadata.csv")) {
  keep_cols <- c("Zeitstempel", "zustand", "meldungen", "id")

  metadata <- readr::read_csv(file = path_metadata, 
                              col_types = readr::cols(.default = "c"))
  
  mbr4_data %>%
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
