# clean_stop -------------------------------------------------------------------
clean_stop <- function(...) {
  stop(..., call. = FALSE)
}

# column_to_date ---------------------------------------------------------------
column_to_date <- function(df, column) {
  dates_raw <- kwb.utils::selectColumns(df, column)
  janitor::excel_numeric_to_date(
    date_num = as.numeric(dates_raw),
    date_system = "modern"
  )
}

# comma_to_dot -----------------------------------------------------------------
comma_to_dot <- function(x) {
  gsub(",", ".", x)
}

# list_full_csv_files ----------------------------------------------------------
list_full_csv_files <- function(path) {
  list.files(path, pattern = "\\.csv", full.names = TRUE)
}

# list_full_xls_files ----------------------------------------------------------
list_full_xls_files <- function(path) {
  list.files(path, pattern = "\\.xls", full.names = TRUE)
}

# num_column_to_posix_cet ------------------------------------------------------
num_column_to_posix_cet <- function(df, column) {
  times_raw <- kwb.utils::selectColumns(df, column)
  times_num <- as.numeric(comma_to_dot(times_raw))
  as.POSIXct(times_num * 24 * 3600, origin = "1899-12-30", tz = "CET")
}

# package_file -----------------------------------------------------------------
package_file <- function(...) {
  system.file(..., package = "kwb.pilot")
}

# print_to_text ----------------------------------------------------------------
#' @importFrom utils capture.output
print_to_text <- function(x) {
  paste(utils::capture.output(print(x)), collapse = "\n")
}

# sprintf_columns ----------------------------------------------------------------
sprintf_columns <- function(fmt, df, columns) {
  do.call(sprintf, c(list(fmt), kwb.utils::selectColumns(df, columns)))
}

# to_list_items ----------------------------------------------------------------
to_list_items <- function(items) {
  paste("* ", items, collapse = "  \n")
}

# to_month_pattern -------------------------------------------------------------
to_month_pattern <- function(from, to) {
  to_pattern_or(c(from, to))
}

# to_pattern_or ----------------------------------------------------------------
to_pattern_or <- function(x) {
  paste0(x, collapse = "|")
}
