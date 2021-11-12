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
