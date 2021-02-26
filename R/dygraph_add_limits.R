#' Dygraph: add (multiple) horizontal lines to plot
#'
#' @param dygraph a dygraph object where (possibly) multiple horizontal lines
#' should be added
#' @param limits_df dataframe containing the limits information to be added to
#' the dygraph (e.g. as retrieved by function get_thresholds())
#' @param col_limits column in limits_df containing the limits values (default:
#' "ParameterThreshold")
#' @param col_label column in limits_df containing the label values (default:
#' "label")
#' @param label_loc Location for horizontal dygraph labels (left or right).
#' (default: "left)
#' @param \dots further arguments passed to dygraphs::dyLimit()
#' @return add limits to existing dygraph object
#' @import dygraphs
#' @export
dygraph_add_limits <- function(
  dygraph,
  limits_df, # thresholds[thresholds$ParameterName %in% "Battery voltage",],
  label_loc = "left",
  col_limits = "ParameterThreshold",
  col_label = "label",
  ...
)
{
  if (nrow(limits_df) == 0) {
    return(dygraph)
  }
  
  for (i in seq_len(nrow(limits_df))) {
    
    dygraph <- dygraphs::dyLimit(
      dygraph = dygraph,
      limit = limits_df[i, col_limits],
      label = limits_df[i, col_label],
      labelLoc = label_loc,
      ...
    )
  }
  
  dygraph
}
