#' Read MBR4.0 tsv data  
#' @param path path to tsv file to be imported 
#' @param locale locale (default: \code{\link[readr]{locale}}(tz = "CET",
#' decimal_mark = ",", grouping_mark = "."))
#' @param col_types col_types (default: \code{\link[readr]{cols}}(
#' .default = \code{\link[readr]{col_double}},  
#'  zustand  = \code{\link[readr]{col_character}},
#'  meldungen  = \code{\link[readr]{col_character}},
#'  Zeitstempel = \code{\link[readr]{col_datetime}}(format = "%d.%m.%Y %H:%M")
#'  )
#' @param dbg print debug messages (default: FALSE)
#' @param ... additional arguments passed to  \link[readr]{read_tsv}
#' @return Reads MBR4.0 tsv data
#' @importFrom readr read_tsv locale cols col_double col_character col_datetime

read_mbr4_tsv <- function(path,
                          locale = readr::locale(tz = "CET",
                                                 decimal_mark = ",",
                                                 grouping_mark = "."),
                          col_types = readr::cols(
                            .default = readr::col_double(),
                            zustand = readr::col_character(),
                            meldungen = readr::col_character(),
                            Zeitstempel = readr::col_datetime(format = "%d.%m.%Y %H:%M")
                          ),
                          dbg = FALSE,
                          ...) {

  import_raw <- function() {
  readr::read_tsv(
    file = path,
    locale = locale,
    col_types = col_types,
    trim_ws = TRUE,
    ...
  ) %>%
    dplyr::select(!tidyselect::matches("X[0-9]+"))
  }
  
  if (dbg) {
    import_raw()
  } else {
    ### suppress warnings
    suppressWarnings(import_raw())
  }
}


#' Read MBR4.0 data combining latest and archived data 
#' @description  Download latest data as 'tsv' from Martin Systems Webportal and 
#' combine with archived ('tsv') on Nextcloud'
#' @param latest_url url to download latest .tsv file (default: Sys.getenv("MBR40_URL"),
#' please use run \link[usethis]{edit_r_environ} and define a
#' key value pair "MBR40_URL" = "download-url-martin-systems") so
#' that this function works automatically
#' @param archived_file file name/pattern of XLSX file  (default: "MBR_export_")
#' @param archived_dir directory on Nextcloud containing file (default: 
#' "projects/MBR4.0/Exchange/Rohdaten/Online_export")
#' @param archived_url url of Nextcloud (default: Sys.getenv("NEXTCLOUD_URL"))
#' @param archived_user username of Nextcloud  (default: Sys.getenv("NEXTCLOUD_USER"))
#' @param archived_pw password of Nextcloud  (default: Sys.getenv("NEXTCLOUD_USER"))
#' @param target_dir directory to download data (default: tempdir())
#' @param dbg print debug messages (default: FALSE)
#' @param ... additional arguments passed to  \link[readr]{read_tsv}
#' @return tibble with imported MBR4.0 xlsx data (archived on Nextcloud since 
#' start of operation)
#' @export
#'
#' @examples
#' \dontrun{
#' mbr4_data <- read_mbr4()
#' str(mbr4_data)
#' }
read_mbr4 <- function(latest_url = Sys.getenv("MBR40_URL"),
                      archived_file = "MBR_export_",
                      archived_dir = "projects/MBR4.0/Exchange/Rohdaten/Online_export",
                      archived_url = Sys.getenv("NEXTCLOUD_URL"),
                      archived_user = Sys.getenv("NEXTCLOUD_USER"),
                      archived_pw = Sys.getenv("NEXTCLOUD_USER"),
                      target_dir = tempdir(),
                      dbg = FALSE,
                      ...) {
  
  

  mbr4_data_latest <- read_mbr4_latest(url = latest_url,
                                       target_dir = target_dir,
                                       dbg = dbg,
                                       ...)
  
  
  mbr4_data_archived <- read_mbr4_archived(file = archived_file,
                                           dir = archived_dir,
                                           target_dir = target_dir,
                                           url = archived_url,
                                           user = archived_user,
                                           pw = archived_pw,
                                           dbg = dbg,
                                           ...)
  
  dplyr::bind_rows(mbr4_data_latest,
                   mbr4_data_archived) %>%
    remove_duplicates()
  

}

#' Read MBR4.0 data from Martin Systems Webportal (As "tsv")
#'
#' @param url url to download site (default: Sys.getenv("MBR4.0_URL"),
#' please use run \link[usethis]{edit_r_environ} and define a
#' key value pair "MBR40_URL" = "download-url-martin-systems") so
#' that this function works automatically
#' @param target_dir directory to download data (default: tempdir())
#' @param locale locale (default: \code{\link[readr]{locale}}(tz = "CET",
#' decimal_mark = ".", grouping_mark = ","))
#' @param col_types col_types (default: \code{\link[readr]{cols}}(
#' .default = \code{\link[readr]{col_double}},  
#'  zustand  = \code{\link[readr]{col_character}},
#'  meldungen  = \code{\link[readr]{col_character}},
#'  Zeitstempel = \code{\link[readr]{col_datetime}}(format = "%Y-%m-%d %H:%M:%S")
#'  )
#' @param dbg print debug messages (default: FALSE)
#' @param ... additional arguments passed to  \link[readr]{read_tsv}
#' @return tibble with imported MBR4.0 tsv data (~ last four weeks)
#' @importFrom readr read_tsv locale cols col_double col_character col_datetime
#' @importFrom  utils download.file
#' @export
#'
#' @examples
#' url_mbr40 <- Sys.getenv("MBR40_URL")
#' if(url_mbr40 != "") {
#' mbr4_data_latest <- read_mbr4_latest(url = url_mbr40)
#' str(mbr4_data_latest)
#' }
read_mbr4_latest <- function(url = Sys.getenv("MBR40_URL"),
                      target_dir = tempdir(),
                      locale = readr::locale(tz = "CET",
                                             decimal_mark = ".",
                                             grouping_mark = ","),
                      col_types = readr::cols(
                        .default = readr::col_double(),
                        zustand = readr::col_character(),
                        meldungen = readr::col_character(),
                        Zeitstempel = readr::col_datetime(format = "%Y-%m-%d %H:%M:%S")
                      ),
                      dbg = FALSE,
                      ...) {
  
  target_path <- file.path(target_dir, "mbr4.tsv")
  
  utils::download.file(
    url = url,
    destfile = target_path, quiet = !dbg
  )
  
  read_mbr4_tsv(path = target_path,
                locale = locale,
                col_types = col_types,
                dbg = dbg, 
                ...)

}

#' Read MBR4.0 archived data from Nextcloud 
#' @param file file name/pattern of tsv file  (default: "MBR_export_")
#' @param dir directory on Nextcloud containing file (default: 
#' "projects/MBR4.0/Exchange/Rohdaten/Online_export")
#' @param target_dir directory to download data (default: tempdir())
#' @param locale locale (default: \code{\link[readr]{locale}}(tz = "CET",
#' decimal_mark = ",", grouping_mark = "."))
#' @param col_types col_types (default: \code{\link[readr]{cols}}(
#' .default = \code{\link[readr]{col_double}},  
#'  zustand  = \code{\link[readr]{col_character}},
#'  meldungen  = \code{\link[readr]{col_character}},
#'  Zeitstempel = \code{\link[readr]{col_datetime}}(format = "%d.%m.%Y %H:%M")
#'  )
#' @param url url of Nextcloud (default: Sys.getenv("NEXTCLOUD_URL"))
#' @param user username of Nextcloud  (default: Sys.getenv("NEXTCLOUD_USER"))
#' @param pw password of Nextcloud  (default: Sys.getenv("NEXTCLOUD_USER"))
#' @param dbg print debug messages (default: FALSE)
#' @param ... additional arguments passed to  \link[readr]{read_tsv}
#' @return tibble with imported archived MBR4.0 xlsx data from Nextcloud
#' @importFrom kwb.nextcloud list_files download_files
#' @importFrom stringr str_detect
#' @export
#'
#' @examples
#' if(check_env_nextcloud()) {
#' mbr4_data_archived <- read_mbr4_archived()
#' str(mbr4_data_archived)
#' }
read_mbr4_archived <- function(
  file = "MBR_export_",
  dir = "projects/MBR4.0/Exchange/Rohdaten/Online_export",
  target_dir = tempdir(),
  locale = readr::locale(tz = "CET",
                         decimal_mark = ",",
                         grouping_mark = "."),
  col_types = readr::cols(
    .default = readr::col_double(),
    zustand = readr::col_character(),
    meldungen = readr::col_character(),
    Zeitstempel = readr::col_datetime(format = "%d.%m.%Y %H:%M")
  ),
  url = Sys.getenv("NEXTCLOUD_URL"),
  user = Sys.getenv("NEXTCLOUD_USER"),
  pw = Sys.getenv("NEXTCLOUD_USER"),
  dbg = FALSE,
  ...) {
  
  stopifnot(all(c(url, user, pw) != "")) 
  
  
  archived_file <- kwb.nextcloud::list_files(
    path = dir,
    full_info = TRUE) %>%
    dplyr::filter(stringr::str_detect(.data$file,
                                      pattern = sprintf("^%s",  
                                                        file))) %>%
    dplyr::filter(stringr::str_detect(.data$file,
                                      pattern = "\\.tsv$")) %>% 
    dplyr::arrange(dplyr::desc(.data$lastmodified))
  
  if (nrow(archived_file) > 1) {
    message(sprintf(paste("Multiple '.tsv' files (%s) found on Nextcloud.", 
    "Using newest one '%s' (last modified: %s"),
                paste(archived_file$file, collapse = ", "), 
                archived_file$file[1], 
                archived_file$lastmodified))
    archived_file <- archived_file[1,]
  }
  
  archived_path <- kwb.nextcloud::download_files(hrefs = archived_file$href, 
                                             target_dir = target_dir)
  
  read_mbr4_tsv(path = archived_path, 
                locale = locale,
                col_types = col_types,
                dbg = dbg,
                ...)
}

