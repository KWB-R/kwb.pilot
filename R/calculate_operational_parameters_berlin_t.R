#' Calculate operational parameters for Berlin-Tiefwerder
#' @param df a data frame as retrieved by read_pentair_data()
#' @param calc_list list with calculation operations to be carried out
#' (default: list(recovery  = "100*`FY-20-01`/`FT-10-01`"))
#' @param calc_list_name full names of parameters to be used for plotting for each
#' calculation specified wit 'calc_list'. default: c('recovery')
#' @param calc_list_unit units of parameters to be used for plotting for each
#' calculation specified wit 'calc_list'. default: c("percent")
#' @param calc_paras a vector with parameter codes used for performing calculations
#' defined in 'calc_list' (default: c("FY-20-01", "FT-10-01")
#' @return dataframe with calculated operational parameters
#' @export
#' @examples
#' \dontrun{
#' raw_list <- read_pentair_data()
#' myDat <- calculate_operational_parameters_berlin_t(df = raw_list)}


calculate_operational_parameters_berlin_t <- function(df,
                                                      calc_list = list(recovery = "100*`FY-20-01`/`FT-10-01`"),
                                                      calc_list_name = c("recovery"),
                                                      calc_list_unit = c("%"),
                                                      calc_paras = c(
                                                        "FY-20-01",
                                                        "FT-10-01"
                                                      )) {
  res <-  kwb.pilot::calculate_operational_parameters(
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
