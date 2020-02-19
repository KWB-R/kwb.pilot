#' Remove duplicates in data.frame
#' @param df data.frame to be checked for duplicates
#' @param col_names  column names to be used for duplicate checking (default: names(df)).
#' can be defined by providing: c("col_name1", "col_name2")
#' @return data.frame without duplicates
#' @importFrom kwb.utils stringList
#' @export
remove_duplicates <- function(df, col_names = names(df))
{
  available <- col_names %in% names(df)
  
  if (! all(available)) clean_stop(
    "The following 'col_names' specified by the user are not defined ", 
    "in the 'df':\n", kwb.utils::stringList(col_names[! available])
  )
  
  cat(sprintf(
    "Checking for duplicates (in columns: %s) in %d rows...",
    kwb.utils::stringList(col_names), nrow(df)
  ))
  
  is_duplicated <- duplicated(df[, col_names])
  
  if (any(is_duplicated)) {
    
    warning(
      sprintf("Removing %d duplicates:\n", sum(is_duplicated)),
      print_to_text(df[is_duplicated, ])
    )
    
    df <- df[! is_duplicated, ]
  }
  
  df
}
