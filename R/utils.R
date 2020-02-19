# clean_stop -------------------------------------------------------------------
clean_stop <- function(...)
{
  stop(..., call. = FALSE)
}

# package_file -----------------------------------------------------------------
package_file <- function(...) 
{
  system.file(..., package = "kwb.pilot")
}

# print_to_text ----------------------------------------------------------------
#' @importFrom utils capture.output
print_to_text <- function(x)
{
  paste(utils::capture.output(print(x)), collapse = "\n")
}

# to_list_items ----------------------------------------------------------------
to_list_items <- function(items)
{
  paste("* ", items, collapse = "  \n")
}

# to_month_pattern -------------------------------------------------------------
to_month_pattern <- function(from, to)
{
  to_pattern_or(c(from, to))
}

# to_pattern_or ----------------------------------------------------------------
to_pattern_or <- function(x) 
{
  paste0(x, collapse = "|")
}
