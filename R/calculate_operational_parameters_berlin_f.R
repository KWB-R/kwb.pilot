
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
normalised_permeate_flow <- function(
  tempFeed,
  conLoop,
  vfrPerm,
  vfrLoop,
  vfrFeed,
  prePerm,
  preProc,
  preConc,
  nwp0 = 1.429162,
  vfrPerm0 = 800
)
{
  res <- tibble::tibble(
    tcf = exp(3020 * (1 / 298 - (1 / (273 + tempFeed)))),
    cfc = conLoop * 0.65 * ((log(1 / (1 - vfrPerm / (vfrLoop + vfrFeed)))) / (vfrPerm / (vfrLoop + vfrFeed))),
    ### Osmotischer Druck
    preOsmo = (.data$cfc * (tempFeed + 320)) / 491000,
    nwp = ((preProc - (preProc - preConc) / 2 - prePerm - .data$preOsmo) * .data$tcf),
    nwpt = (vfrPerm * (nwp0 / .data$nwp)) * (vfrPerm / vfrPerm0),
    nwpr = -(1 - .data$nwpt / vfrPerm) * 100
  )
  
  res$nwpt
}

#' Calculate operational parameters for Berlin-Friedrichshagen
#' @param df a data frame as retrieved by import_data_berlin_f()
#' @param calc_list list with calculation operations to be carried out
#' @param calc_list_name full names of parameters to be used for plotting for each
#' calculation specified wit 'calc_list'.
#' @param calc_list_unit units of parameters to be used for plotting for each
#' calculation specified wit 'calc_list'.
#' @param calc_paras a vector with parameter codes used for performing calculations
#' defined in 'calc_list'
#' @return dataframe with calculated operational parameters
#' @export
#' @examples
#' \dontrun{
#' raw_list <- import_data_berlin_f()
#' myDat <- calculate_operational_parameters_berlin_f(df = raw_list)
#' }
#'
calculate_operational_parameters_berlin_f <- function(
  df,
  calc_list = get_calc_info_berlin_f(),
  calc_list_name = get_calc_info_berlin_f("name"),
  calc_list_unit = get_calc_info_berlin_f("unit"),
  calc_paras = get_calc_info_berlin_f("paras")
)
{
  res <- calculate_operational_parameters(
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

# get_calc_info_berlin_f -------------------------------------------------------
get_calc_info_berlin_f <- function(what = "expr")
{
  get_calc_info_from_config(config = get_calc_config("berlin_f"), what)
}
