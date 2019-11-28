#' Report config: generate template
#' @param df a dataframe as retrieved by import_data_haridwar()
#' @param temporal_aggregation Set the following values if data should be
#' summarised to e.g. 10 minutes (600) or hourly (3600), daily ("day")
#' or monthly ("month") median values
#' (default: "raw")
#' @param output_timezone into which timezone should the data be converted for
#' the report? (default: "UTC")
#' @return default list for report configuration template
#' @export

report_config_template <- function(df = NULL,
                                   temporal_aggregation = "raw",
                                   output_timezone = "UTC") {
  if (is.null(df)) {
    sitenames <- c(
      "General",
      "Tank water",
      "Well Water",
      "After Filter",
      "After AO Cell"
    )
    parameters_online <- "Redox potential"
    parameters_offline <- "Temperature"

    start_day <- sprintf("%s-01", format(Sys.Date(), format = "%Y-%m"))
    end_day <- as.character(Sys.Date())
    daterange <- c(start_day, end_day)
  } else {
    sitenames <- unique(df$SiteName)
    parameters_online <- unique(df$ParameterName[df$Source == "online"])
    parameters_offline <- unique(df$ParameterName[df$Source == "offline"])
    daterange <- as.character(c(as.Date(min(df$DateTime)), as.Date(max(df$DateTime))))
  }

  config <- list(
    report_sitenames = sitenames,
    report_aggregation = temporal_aggregation,
    report_parameters_online = parameters_online,
    report_parameters_offline = parameters_offline,
    report_parameters_calculated = c(
      "Specific energy demand of pump",
      "Specific energy demand of cell"
    ),
    report_add_thresholds = TRUE,
    report_daterange = daterange,
    report_timezone = output_timezone
  )
  return(config)
}

#' Report config: saves config to text file
#' @param config_list a report configuration list e.g. as retrieved by
#' report_config_template()
#' @param output_file absolute or relative path of where to save output file
#' (default: "report_config.txt")
#' @return saves report configuration list as text file
#' @export
#' @examples
#' \dontrun{
#' ### Creates a configuration template
#' config <- report_config_template()
#' ### Saves list config in text
#' report_config_to_txt(config_list = config,
#' output_file = "report_config.txt")
#' }
report_config_to_txt <- function(config_list,
                                 output_file = "report_config.txt") {

  ### Write config list to text file
  ### see http://stackoverflow.com/questions/8261590/write-list-to-a-text-file-preserving-names-r

  if (file.exists(output_file)) {
    file.remove(output_file)
  }

  if (!dir.exists(dirname(output_file))) {
    dir.create(path = dirname(output_file), showWarnings = FALSE)
  }
  # z <- deparse(substitute(config_list))
  # cat(z, "\n", file=output_file)
  nams <- names(config_list)
  for (i in seq_along(config_list)) {
    cat(
      nams[i], "=", paste(paste0("'", config_list[[i]], "'"), collapse = " "), "\n",
      sep = "",
      file = output_file,
      append = TRUE
    )
  }
}

#' Report config: imports text file to list
#' @param config_txt path to report configuration text file created by
#' a report configuration list e.g. as retrieved by function report_config_to_txt()
#' @return saves report configuration list as text file
#' @export
#' @examples
#' \dontrun{
#' ### Creates a configuration template
#' config <- report_config_template()
#' ### Saves list config in text
#' report_config_to_txt(config_list = config, output_file = "report_config.txt")
#' ### Reads config list from text file to
#' config_imported <- report_txt_to_config(config_txt = "report_config.txt")
#' ### Check whether both are identical
#' identical(x = config, y = config_imported)
#' }
report_txt_to_config <- function(config_txt = "report_config.txt") {
  x <- scan(
    config_txt,
    what = "",
    sep = "\n"
  )
  # Separate elements by one or more whitepace
  y <- strsplit(x, "=")
  # Extract the first vector element and set it as the list element name
  names(y) <- sapply(y, `[[`, 1)
  # names(y) <- sapply(y, function(x) x[[1]]) # same as above
  # Remove the first vector element from each list element
  y <- lapply(y, `[`, -1)


  ### Remove "'" from character strings
  y <- lapply(y, FUN = function(x) {
    gsub(pattern = "'", replacement = "", unlist(strsplit(x, split = "'\\s")))
  })

  if (!is.na(as.numeric(y$report_aggregation))) {
    y$report_aggregation <- as.numeric(y$report_aggregation)
  }

  return(y)
}
