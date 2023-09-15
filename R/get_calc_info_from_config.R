# get_calc_info_from_config ----------------------------------------------------
get_calc_info_from_config <- function(config, what = "expr")
{
  calculated <- kwb.utils::selectElements(config, "calculated")

  if (what == "paras") {
    return(kwb.utils::selectElements(config, "parameters"))
  }
  
  result <- lapply(calculated, kwb.utils::selectElements, what)
    
  if (what == "expr") {
    return(result)
  }
  
  unname(unlist(result))
}

# get_calc_config --------------------------------------------------------------
#' @importFrom yaml read_yaml
get_calc_config <- function(site)
{
  yaml::read_yaml(shiny_file(site, "config/config.yml"))
}
