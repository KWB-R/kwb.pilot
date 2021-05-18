#' Read MBR4.0 data from Martin Systems
#'
#' @param url url to download site (default: Sys.getenv("MBR4.0_URL"), 
#' please use run \link[usethis]{edit_r_environ} and define a 
#' key value pair "MBR4.0_URL" = "download-url-martin-systems") so 
#' that this function works automatically
#' @param target_path to download data (default: file.path(tempdir(), "mbr4.tsv"))
#' @param debug print debug messages (default: FALSE)
#' @param ... additional arguments passed to  \link[readr]{read_tsv}
#' @return tibble with imported MBR4.0 data
#' @importFrom readr read_tsv locale cols col_double col_character col_datetime
#' @importFrom  utils download.file
#' @export
#'
#' @examples
#' mbr4_data <- read_mbr4()
#' str(mbr4_data)
read_mbr4 <- function(url = Sys.getenv("MBR4.0_URL"), 
                      target_path = file.path(tempdir(), "mbr4.tsv"), 
                      dbg = FALSE,
                      ...
                      ) {
  
  
  utils::download.file(url = url,
                destfile = target_path, quiet = !dbg)
  
  import_raw <- function() {
  readr::read_tsv(file = target_path, 
                  locale = readr::locale(date_format = "en",
                                         time_format = "en",
                                         tz = "CET",
                                         decimal_mark = ",", 
                                         grouping_mark = "."),
                  col_types = readr::cols(.default = readr::col_double(),
                                          zustand = readr::col_character(),
                                          meldungen = readr::col_character(),
                                          Zeitstempel = readr::col_datetime(format = "")),
                  trim_ws = TRUE,
                  ...
                  ) %>% 
    dplyr::select(- tidyselect::starts_with("X[0-9]+"))
  }
  
  if(dbg) {
    import_raw()
  } else {
    ### suppress warnings 
    suppressWarnings(import_raw())
  } 

}

