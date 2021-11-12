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
#' myDat <- calculate_operational_parameters_berlin_t(df = raw_list)
#' }
#'
calculate_operational_parameters_berlin_t <- function(
  df,
  calc_list = get_calc_info_berlin_t(),
  calc_list_name = get_calc_info_berlin_t("name"),
  calc_list_unit = get_calc_info_berlin_t("unit"),
  calc_paras = get_calc_info_berlin_t("paras")
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

# get_calc_info_berlin_t -------------------------------------------------------
get_calc_info_berlin_t <- function(what = "expr")
{
  get_calc_info_from_config(config = get_calc_config_berlin_t(), what)
}

# get_calc_config_berlin_t -----------------------------------------------------
get_calc_config_berlin_t <- function()
{
  config <- list(
    parameters = c(
      "FY-20-01", 
      "FT-10-01"
    ),
    calculated = list(
      recovery = list(
        name = "recovery",
        unit = "%",
        expr = "100*`FY-20-01`/`FT-10-01`")
    )
  )
  
  file <- "./inst/shiny/berlin_t/config/config.yml"
  stopifnot(identical(config, yaml::read_yaml(file)))
  
  config
}
