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
                   yield = "100*(`Durchfluss_Rohwasser` - `Durchfluss_Konzentrat`) / `Durchfluss_Rohwasser`", 
                   recovery = "100*(1 - `LF_Permeat` / `LF_Rohwasser`)", 
                   deltaPreProcFeed = "`Druck_Arbeitsdruck` - `Druck_Rohwasser`",
                   deltaPreProcConc = "`Druck_Arbeitsdruck` - `Druck_Konzentrat`",
                   #Membranfläche NF 270 37m² (4x in Reihe) (update 2019-12-03)
                   #surf <- 4*37
                   flux = "(vfrPerm + `Durchfluss_Rezirkulation`)/(4*37)" 
                   ),
  calc_list_name = c("Durchfluss Permeat", 
                     "Ausbeute", 
                     "Rückhalt", 
                     "Differenzdruck Rohwasser", 
                     "Differenzdruck Konzentrat", 
                     "Flux"),
  calc_list_unit = c("l/h", 
                     "%", 
                     "%", 
                     "bar", 
                     "bar", 
                     "l/h/m2"),
  calc_paras = c("Durchfluss_Rohwasser", 
                 "Durchfluss_Konzentrat", 
                 "Durchfluss_Rezirkulation",
                 "Druck_Arbeitsdruck", 
                 "Druck_Rohwasser",
                 "Druck_Konzentrat", 
                 "LF_Permeat", 
                 "LF_Rohwasser",
                 "Durchfluss_Rezirkulation")
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
