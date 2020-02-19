# clean_stop -------------------------------------------------------------------
clean_stop <- function(...)
{
  stop(..., call. = FALSE)
}

# to_list_items ----------------------------------------------------------------
to_list_items <- function(items)
{
  paste("* ", items, collapse = "  \n")
}

# to_month_pattern -------------------------------------------------------------
to_month_pattern <- function(from, to)
{
  paste0(c(from, to), collapse = "|")
}
