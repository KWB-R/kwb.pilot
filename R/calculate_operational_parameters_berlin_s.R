#' Calculate operational parameters for Berlin-Schoenerlinde
#'
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
#' myDat <- calculate_operational_parameters_berlin_s(df = raw_list)
#' }
#'
calculate_operational_parameters_berlin_s <- function(
  df,
  calc_list = get_calc_info_berlin_s(),
  calc_list_name = get_calc_info_berlin_s("name"),
  calc_list_unit = get_calc_info_berlin_s("unit"),
  calc_paras = get_calc_info_berlin_s("paras")
)
{
  res <- calculate_operational_parameters(
    df, calc_list, calc_list_name, calc_list_unit, calc_paras
  )
  
  res$SiteName <- "General"
  res$SiteName_ParaName_Unit <- paste("General (calculated):", res$ParameterLabel)
  res$DataType <- "calculated"
  res$Source <- "online"
  
  res
}

# get_calc_info_berlin_s -------------------------------------------------------
get_calc_info_berlin_s <- function(what = "expr")
{
  get_calc_info_from_config(config = get_calc_config_berlin_s(), what)
}

# get_calc_config_berlin_s -----------------------------------------------------
get_calc_config_berlin_s <- function()
{
  list(
    parameters = c(
      "SCAN_SAK_Ablauf",
      "SCAN_SAK_Zulauf",
      "C_O3_Zugas",
      "C_O3_Abgas",
      "Q_Gas",
      "Q_Ozonanlage"
    ),
    calculated = list(    
      deltaSAK = list(
        name = "delta SAK", 
        unit = "%", 
        expr = "(1-SCAN_SAK_Ablauf/SCAN_SAK_Zulauf)*100"),
      Ozoneintrag = list(
        name = "Ozoneintrag",
        unit = "mg-O3/L",
        expr = "(C_O3_Zugas - C_O3_Abgas)*Q_Gas/Q_Ozonanlage")
    )
  )
}

if (FALSE)
{
  file <- "./inst/shiny/berlin_s/config/config.yml"
  kwb.utils::createDirectory(dirname(file))
  config <- kwb.pilot:::get_calc_config_berlin_s()
  
  yaml::write_yaml(config, file)
  
  identical(config, yaml::read_yaml(file))
}
