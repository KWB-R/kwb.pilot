
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
  get_calc_info_from_config(config = get_calc_config_berlin_f(), what)
}

# get_calc_config_berlin_f -----------------------------------------------------
get_calc_config_berlin_f <- function()
{
  config <- list(
    parameters = c(
      "Durchfluss_Rohwasser",
      "Durchfluss_Konzentrat",
      "Durchfluss_Rezirkulation",
      "Druck_Arbeitsdruck",
      "Druck_Rohwasser",
      "Druck_Konzentrat",
      "Druck_Permeat",
      "LF_Permeat",
      "LF_Rohwasser",
      "LF_Konzentrat",
      "Temperatur_Rohwasser"
    ),
    calculated = list(
      vfrPerm = list(
        name = "Durchfluss Permeat",
        unit = "l/h",
        expr = "`Durchfluss_Rohwasser` - `Durchfluss_Konzentrat`"
      ),
      yield = list(
        name = "Ausbeute",
        unit = "%",
        expr = "100*(`Durchfluss_Rohwasser` - `Durchfluss_Konzentrat`) / `Durchfluss_Rohwasser`"
      ),
      conLoop = list(
        name = "Leitf\u00E4higkeit Rezirkulation",
        unit = "\u03BCS/cm", # "\xB5S/cm",
        expr = "(`Durchfluss_Rohwasser`*`LF_Rohwasser` + `Durchfluss_Rezirkulation`*`LF_Konzentrat`)/(`Durchfluss_Rohwasser` + `Durchfluss_Rezirkulation`)"
      ),
      recovery = list(
        name = "R\u00FCckhalt",
        unit = "%",
        expr = "100*(1 - `LF_Permeat` / conLoop)"
      ),
      deltaPreProcConc = list(
        name = "Druckverlust (Feed - Konzentrat)",
        unit = "bar",
        expr = "`Druck_Arbeitsdruck` - `Druck_Konzentrat`"
      ),
      # Membranflaeche NF 4x in Reihe: #4 x NF 270-4040 mit 7,6 m2 aktiver Flaeche
      # surf = 4 * 7.6
      flux = list(
        name = "Flux",
        unit = "l/h/m2",
        expr = "vfrPerm / (4 * 7.6)"
      ),
      cfv = list(
        name = "\u00DCberstr\u00F6mungsgeschwindigkeit",
        unit = "m/s",
        expr = "(`Durchfluss_Rohwasser`+ `Durchfluss_Rezirkulation`) / ((pi * 0.0095^2) * 1000 * 3600)"
      ),
      tmp = list(
        name = "Transmembrandruck",
        unit = "bar",
        expr = "((`Druck_Arbeitsdruck` + `Druck_Konzentrat`) / 2) - `Druck_Permeat`"
      ),
      nwpt = list(
        name = "Normalisierter Permeatstrom",
        unit = "l/h",
        expr = "normalised_permeate_flow(tempFeed = `Temperatur_Rohwasser`,
                                     conLoop = `conLoop`,
                                     vfrPerm = `vfrPerm`,
                                     vfrLoop = `Durchfluss_Rezirkulation`,
                                     vfrFeed = `Durchfluss_Rohwasser`,
                                     prePerm = `Druck_Permeat`,
                                     preProc = `Druck_Arbeitsdruck`,
                                     preConc = `Druck_Konzentrat`,
                                     nwp0 = 1.429162,
                                     vfrPerm0 = 800)"
      ),
      nwpr = list(
        name = "Relativer Permeatstrom",
        unit = "%",
        expr = "- ((1 - (nwpt / vfrPerm))) * 100"
      )
    )
  )
  
  file <- "./inst/shiny/berlin_f/config/config.yml"
  stopifnot(identical(config, yaml::read_yaml(file)))
  
  config
}
