#' Remove duplicates in data.frame
#' @param df data.frame to be checked for duplicates
#' @param col_names  column names to be used for duplicate checking (default: names(df)).
#' can be defined by providing: c("col_name1", "col_name2")
#' @return data.frame without duplicates
#' @importFrom utils capture.output
#' @importFrom kwb.utils stringList
#' @export
remove_duplicates <- function(df,
                              col_names = names(df)) {
  col_names_in_df <- col_names %in% names(df)

  if (!all(col_names_in_df)) {
    clean_stop(sprintf(
      "The following 'col_names' specified by the user are not defined in the 'df': %s",
      kwb.utils::stringList(col_names[! col_names_in_df])
    ))
  } else {
    print(sprintf(
      "Checking if duplicated entries (for columns: %s) in %d rows...",
      kwb.utils::stringList(col_names),
      nrow(df)
    ))

    dups <- duplicated(df[, col_names])

    if (any(dups)) {
      dups_indices <- which(dups)
      warning(sprintf(
        "Removing %d duplicates:\n%s",
        length(dups_indices),
        paste(
          utils::capture.output(print(df[dups_indices, ])),
          collapse = "\n"
        )
      ))

      df <- df[!dups, ]
    }
  }

  return(df)
}
