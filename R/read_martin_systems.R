#' Read Martin Systems data from single CSV file
#'
#' @description CSV format should use the ";" as field and "," as decimal 
#' separator
#' @param path path to Martin Systems file
#' @param tz time zone (default: CET) the measurements are taken (passed to 
#' function readr::locale(tz = tz))
#' @param dbg debug (default: TRUE)
#'
#' @return data frame with Martin Systems raw data in "long" format
#' @export
#'
read_martin_systems <- function(path, tz = "CET", dbg = TRUE)
{
  if (dbg) {
    message("Importing file: ", path)
  }
  
  df <- readr::read_csv2(path,
                         locale = readr::locale(tz = tz)) %>% 
    kwb.utils::renameColumns(renamings = list(
    Zeitstempel = "DateTime"
  ))
  
  tidyr::pivot_longer(df,
                      cols = gather_cols , 
                      names_to = "ParameterCode", 
                      values_to = "ParameterValue")

  
}
