#' Calculate operational parameters for Berlin-Friedrichshagen
#' @param df a data frame as retrieved by import_data_berlin_f()
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
#' raw_list <- import_data_berlin_f()
#' myDat <- calculate_operational_parameters_berlin_f(df = raw_list)}

calculate_operational_parameters_berlin_f <- function(
  df,
  calc_list = list(vfrPerm = "`Durchfluss_Rohwasser` - `Durchfluss_Konzentrat`", 
                   recovery = "100*(`Durchfluss_Rohwasser` - `Durchfluss_Konzentrat`) / `Durchfluss_Rohwasser`", 
                   deltaPreProcFeed = "`Druck_Arbeitsdruck` - `Druck_Rohwasser`",
                   deltaPreProcConc = "`Druck_Arbeitsdruck` - `Druck_Konzentrat`"                 
                   ),
  calc_list_name = c("Durchfluss Permeat", 
                     "Rückhalt", 
                     "Differenzdruck Rohwasser", 
                     "Differenzdruck Konzentrat"),
  calc_list_unit = c("l/h", "%", "bar", "bar"),
  calc_paras = c("Durchfluss_Rohwasser", 
                 "Durchfluss_Konzentrat", 
                 "Druck_Arbeitsdruck", 
                 "Druck_Rohwasser",
                 "Druck_Konzentrat")
)
{
  res <-  calculate_operational_parameters(
    df,
    calc_list,
    calc_list_name,
    calc_list_unit,
    calc_paras
  )
  
  res$SiteName <- "General"
  res$SiteName_ParaName_Unit <- paste("General (calculated):", res$ParameterLabel)
  res$DataType <- "calculated"
  res$Source <- "online"
  
  res
}
