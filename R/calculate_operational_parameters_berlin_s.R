#' Calculate operational parameters for Berlin-Schoenerlinde
#' @param df a data frame as retrieved by read_wedeco_data()
#' @param calc_list list with calculation operations to be carried out
#' (default: list(deltaSAK  = "(1-SCAN_SAK_Ablauf/SCAN_SAK_Zulauf)*100",
#' Ozoneintrag = "(C_O3_Zugas - C_O3_Abgas)*Q_Gas/Q_Ozonanlage"))
#' @param calc_list_name full names of parameters to be used for plotting for each
#' calculation specified wit 'calc_list'. default: c('delta SAK', 'Ozoneintrag')
#' @param calc_list_unit units of parameters to be used for plotting for each
#' calculation specified wit 'calc_list'. default: c("percent", "mg-O3/L")
#' @param calc_paras a vector with parameter codes used for performing calculations
#' defined in 'calc_list' (default: c("SCAN_SAK_Ablauf", "SCAN_SAK_Zulauf",
#' "C_O3_Zugas", "C_O3_Abgas", "Q_Gas", "Q_Ozonanlage"))
#' @return dataframe with calculated operational parameters
#' @export
#' @examples
#' \dontrun{
#' raw_list <- read_wedeco_data()
#' myDat <- calculate_operational_parameters_berlin_s(df = raw_list)}


calculate_operational_parameters_berlin_s <- function(df,
                                                      calc_list = list(
                                                        deltaSAK = "(1-SCAN_SAK_Ablauf/SCAN_SAK_Zulauf)*100",
                                                        Ozoneintrag = "(C_O3_Zugas - C_O3_Abgas)*Q_Gas/Q_Ozonanlage"
                                                      ),
                                                      calc_list_name = c("delta SAK", "Ozoneintrag"),
                                                      calc_list_unit = c("%", "mg-O3/L"),
                                                      calc_paras = c(
                                                        "SCAN_SAK_Ablauf",
                                                        "SCAN_SAK_Zulauf",
                                                        "C_O3_Zugas",
                                                        "C_O3_Abgas",
                                                        "Q_Gas",
                                                        "Q_Ozonanlage"
                                                      )) {
  res <- calculate_operational_parameters(
    df,
    calc_list,
    calc_list_name,
    calc_list_unit,
    calc_paras
  )
  res$SiteName <- "General"
  res$SiteName_ParaName_Unit <- sprintf("General (calculated): %s", res$ParameterLabel)
  res$DataType <- "calculated"
  res$Source <- "online"
  return(res)
}
