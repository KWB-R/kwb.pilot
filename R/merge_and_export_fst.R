#' Helper function: group fst by pattern
#'
#' @param fst_pattern pattern to search for in fst filename (default: "raw")
#' @param time_pattern optional pattern to filter months to be imported (default: NULL),
#' for using it do e.g. "2017-06|2017-07" or c("2017-06", "2017-07")
#' @param fst_dir directory with fst files or subdirs to be imported (default:
#' kwb.pilot:::shiny_file("berlin_t/data/fst"))
#' @importFrom stringr str_detect
#' @importFrom data.table rbindlist
#' @return merged data.frame
#' @keywords internal

group_fst_by_pattern <- function(
  time_pattern = NULL,
  fst_pattern = "raw",
  fst_dir = shiny_file("berlin_t/data/fst")
)
{
  files <- list.files(fst_dir, fst_pattern, recursive = TRUE, full.names = TRUE)

  if (! is.null(time_pattern)) {
    
    if (length(time_pattern) > 1L) {
      time_pattern <- to_pattern_or(time_pattern)
    }

    files <- grep(time_pattern, files, value = TRUE)
  }

  kwb.utils::catAndRun(
    messageText = sprintf(
      "Importing the following fst files:\n%s\n",
      paste(files, collapse = "\n")
    ),
    expr = {
      data.table::rbindlist(lapply(files, read_fst))
    }
  )
}

#' Helper function: merge and export fst files into main shiny data folder
#'
#' @param time_pattern optional pattern to filter months to be imported (default: NULL),
#' for using it do e.g. "2017-06|2017-07" or c("2017-06", "2017-07")
#' @param compression compression for fst export (default: 100)
#' @param import_dir directory with fst files or subdirs to be imported (default:
#' kwb.pilot:::shiny_file("berlin_t/data/fst"))
#' @param export_dir directory with fst directory for export (default:
#' kwb.pilot:::shiny_file("berlin_t/data"))
#' @return imports multiple fst files and exports them to be used for app
#' @export
merge_and_export_fst <- function(
  time_pattern = NULL,
  compression = 100,
  import_dir = shiny_file("berlin_t/data/fst"),
  export_dir = shiny_file("berlin_t/data")
)
{
  if (! dir.exists(export_dir)) {
    kwb.utils::catAndRun(
      sprintf("Creating export path: %s", export_dir),
      dir.create(export_dir, recursive = TRUE)
    )
  }

  for (fst_pattern in c("raw", "10min", "hour", "day")) {
    
    site_data_list <- kwb.utils::catAndRun(
      paste("Grouping by", fst_pattern),
      group_fst_by_pattern(
        time_pattern = time_pattern,
        fst_pattern = fst_pattern,
        fst_dir = import_dir
      )
    )

    file <- file.path(export_dir, sprintf("siteData_%s_list.fst", fst_pattern))

    kwb.utils::catAndRun(
      sprintf("Writing fst: %s (with compression %d)\n", file, compression),
      fst::write.fst(site_data_list, path = file, compress = compression)
    )
  }
}
