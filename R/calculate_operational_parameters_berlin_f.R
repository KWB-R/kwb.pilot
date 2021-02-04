
#' Calculate normalised permeate flow
#'
#' @param tempFeed tempFeed 
#' @param conLoop conLoop
#' @param vfrPerm vfrPerm
#' @param vfrLoop vfrLoop
#' @param vfrFeed vfrFeed
#' @param prePerm prePerm
#' @param preProc preProc
#' @param preConc preConc
#' @param nwp0 nwp0
#' @param vfrPerm0 vfrPerm0 
#'
#' @return nwpt
#' @export
#' @importFrom tibble tibble
#' 
normalised_permeate_flow <- function(tempFeed, 
                                     conLoop, 
                                     vfrPerm, 
                                     vfrLoop, 
                                     vfrFeed,
                                     prePerm,
                                     preProc, 
                                     preConc,
                                     nwp0 = 1.429162,
                                     vfrPerm0 = 800) {

res <- tibble::tibble(tcf = exp(3020 * (1/298 - (1/(273 + tempFeed)))), 
               cfc = conLoop * 0.65 * ((log(1/(1 - vfrPerm / (vfrLoop + vfrFeed)))) / (vfrPerm/(vfrLoop + vfrFeed))), 
               ###Osmotischer Druck
               preOsmo = (cfc * (tempFeed + 320)) / 491000,
               nwp = ((preProc - (preProc - preConc) / 2 - prePerm - preOsmo) * tcf),
               nwpt = (vfrPerm * (nwp0 / nwp)) * (vfrPerm / vfrPerm0),
               nwpr = - (1 - nwpt / vfrPerm) * 100
               )

res$nwpt
               
               
}               



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
                   conLoop = "(`Durchfluss_Rohwasser`*`LF_Rohwasser` + `Durchfluss_Rezirkulation`*`LF_Konzentrat`)/(`Durchfluss_Rohwasser` + `Durchfluss_Rezirkulation`)", 
                   recovery = "100*(1 - `LF_Permeat` / conLoop)", 
                   deltaPreProcConc = "`Druck_Arbeitsdruck` - `Druck_Konzentrat`",
                   #Membranfläche NF 4x in Reihe: #4 x NF 270-4040 mit 7,6 m² aktiver Fläche
                   #surf = 4 * 7.6
                   flux = "vfrPerm / (4 * 7.6)", 
                   cfv =  "(`Durchfluss_Rohwasser`+ `Durchfluss_Rezirkulation`) / ((pi * 0.0095^2) * 1000 * 3600)",
                   tmp = "((`Druck_Arbeitsdruck` + `Druck_Konzentrat`) / 2) - `Druck_Permeat`",
                   nwpt = "normalised_permeate_flow(tempFeed = `Temperatur_Rohwasser`, 
                                     conLoop = `conLoop`,
                                     vfrPerm = `vfrPerm`, 
                                     vfrLoop = `Durchfluss_Rezirkulation`,
                                     vfrFeed = `Durchfluss_Rohwasser`, 
                                     prePerm = `Druck_Permeat`, 
                                     preProc = `Druck_Arbeitsdruck`, 
                                     preConc = `Druck_Konzentrat`,
                                     nwp0 = 1.429162,
                                     vfrPerm0 = 800)",
                   nwpr = "- ((1 - (nwpt / vfrPerm))) * 100"
                   ),
  calc_list_name = c("Durchfluss Permeat", 
                     "Ausbeute", 
                     "Leitfähigkeit Rezirkulation",
                     "Rückhalt", 
                     "Druckverlust (Feed - Konzentrat)", 
                     "Flux", 
                     "Überströmungsgeschwindigkeit",
                     "Transmembrandruck", 
                     "Normalisierter Permeatstrom",
                     "Relativer Permeatstrom"
                     ),
  calc_list_unit = c("l/h", 
                     "%", 
                     "µS/cm",
                     "%", 
                     "bar", 
                     "l/h/m2",
                     "m/s",
                     "bar", 
                     "l/h", 
                     "%"),
  calc_paras = c("Durchfluss_Rohwasser", 
                 "Durchfluss_Konzentrat", 
                 "Durchfluss_Rezirkulation",
                 "Druck_Arbeitsdruck", 
                 "Druck_Rohwasser",
                 "Druck_Konzentrat",
                 "Druck_Permeat",
                 "LF_Permeat", 
                 "LF_Rohwasser",
                 "LF_Konzentrat", 
                 "Temperatur_Rohwasser")
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
