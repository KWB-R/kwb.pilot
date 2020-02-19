#' Helper function: group fst by pattern
#' @param fst_pattern pattern to search for in fst filename (default: "raw")
#' @param time_pattern optional pattern to filter months to be imported (default: NULL),
#' for using it do e.g. "2017-06|2017-07" or c("2017-06", "2017-07")
#' @param fst_dir directory with fst files or subdirs to be imported (default:
#' system.file("shiny/berlin_t/data/fst",package = "kwb.pilot"))
#' @importFrom stringr str_detect
#' @importFrom data.table rbindlist
#' @return merged data.frame
#' @keywords internal

group_fst_by_pattern <- function(time_pattern = NULL,
                                 fst_pattern = "raw",
                                 fst_dir = system.file(
                                   "shiny/berlin_t/data/fst",
                                   package = "kwb.pilot"
                                 )) {
  fst_file_paths <- list.files(
    path = fst_dir,
    pattern = fst_pattern,
    recursive = TRUE,
    full.names = TRUE
  )

  if (!is.null(time_pattern)) {

    if (length(time_pattern)>1) time_pattern <- to_pattern_or(time_pattern)

    indices <- stringr::str_detect(fst_file_paths, time_pattern)
    fst_file_paths <- fst_file_paths[indices]
  }

  cat(sprintf(
    "Importing the following fst files:\n%s\n",
    paste(fst_file_paths, collapse = "\n")
  ))

  system.time(merge_dt <- data.table::rbindlist(lapply(
    fst_file_paths,
    kwb.pilot::read_fst
  )))

  return(merge_dt)
}

#' Helper function: merge and export fst files into main shiny data folder
#' @param time_pattern optional pattern to filter months to be imported (default: NULL),
#' for using it do e.g. "2017-06|2017-07" or c("2017-06", "2017-07")
#' @param compression compression for fst export (default: 100)
#' @param import_dir directory with fst files or subdirs to be imported (default:
#' system.file("shiny/berlin_t/data/fst",package = "kwb.pilot"))
#' @param export_dir directory with fst directory for export (default:
#' system.file("shiny/berlin_t/data",package = "kwb.pilot"))
#' @return imports multiple fst files and exports them to be used for app
#' @export
merge_and_export_fst <- function(time_pattern = NULL,
                                 compression = 100,
                                 import_dir = system.file(
                                   "shiny/berlin_t/data/fst",
                                   package = "kwb.pilot"
                                 ),
                                 export_dir = system.file(
                                   "shiny/berlin_t/data",
                                   package = "kwb.pilot"
                                 )) {
  if (!dir.exists(export_dir)) {
    print(sprintf("Creating export path: %s", export_dir))
    dir.create(export_dir, recursive = TRUE)
  }

  siteData_raw_list <- group_fst_by_pattern(
    time_pattern = time_pattern,
    fst_pattern = "raw",
    fst_dir = import_dir
  )

  exp_raw <- sprintf("%s/siteData_raw_list.fst", export_dir)
  cat(sprintf(
    "Writing fst: %s (with compression %d)\n",
    exp_raw,
    compression
  ))
  system.time(fst::write.fst(
    siteData_raw_list,
    path = exp_raw,
    compress = compression
  ))

  rm(siteData_raw_list)

  system.time(
    siteData_10min_list <- group_fst_by_pattern(
      time_pattern = time_pattern,
      fst_pattern = "10min",
      fst_dir = import_dir
    )
  )

  exp_10min <- sprintf("%s/siteData_10min_list.fst", export_dir)
  cat(sprintf(
    "Writing fst: %s (with compression %d)\n",
    exp_10min,
    compression
  ))
  fst::write.fst(
    siteData_10min_list,
    path = exp_10min,
    compress = compression
  )

  rm(siteData_10min_list)

  system.time(
    siteData_hour_list <- group_fst_by_pattern(
      time_pattern = time_pattern,
      fst_pattern = "hour",
      fst_dir = import_dir
    )
  )

  exp_hour <- sprintf("%s/siteData_hour_list.fst", export_dir)
  cat(sprintf(
    "Writing fst: %s (with compression %d)\n",
    exp_hour,
    compression
  ))

  fst::write.fst(
    siteData_hour_list,
    path = exp_hour,
    compress = compression
  )

  system.time(
    siteData_day_list <- group_fst_by_pattern(
      time_pattern = time_pattern,
      fst_pattern = "day",
      fst_dir = import_dir
    )
  )

  exp_day <- sprintf("%s/siteData_day_list.fst", export_dir)
  cat(sprintf(
    "Writing fst: %s (with compression %d)\n",
    exp_day,
    compression
  ))

  fst::write.fst(
    siteData_day_list,
    path = exp_day,
    compress = compression
  )
}
