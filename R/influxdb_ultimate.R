#' InfluxDB: Get Pivot Data from ultimate_mean_ bucket
#'
#' @param agg_interval aggregation interval (default: 1h)
#' @param date_start default: "2021-07-05"
#' @param date_stop default: Sys.Date()
#'
#' @return pivot data for desired period and aggregation
#' @export
#' @importFrom influxdbclient InfluxDBClient
#' @importFrom data.table rbindlist
#' @importFrom dplyr select mutate relocate
get_pivot_data <- function(agg_interval = "1d",
                           date_start = "2021-07-05",
                           date_stop = Sys.Date()) {
  config <- get_env_influxdb_ultimate()
  
  #stopifnot(agg_interval %in% c("1d", "1h", "10m", "1m"))
  
  if (agg_interval %in% c("1d", "1h", "10m", "1m")) {
    bucket_source <- sprintf("ultimate_mean_%s", agg_interval)
  } else {
    message("use raw data")
    bucket_source <- "ultimate"
  }
  
  flux_qry  <- paste(
    sprintf('from(bucket: "%s")', bucket_source),
    '|> range(start:',
    sprintf('%sT00:00:00Z,', date_start),
    sprintf('stop: %sT00:00:00Z)', date_stop),
    '|> drop(columns: ["_start", "_stop"])',
    '|> pivot(rowKey: ["_time"], columnKey: ["_measurement", "_field"], valueColumn: "_value")',
    '|> sort(columns: ["_time"])'
  )
  
  client <- influxdbclient::InfluxDBClient$new(
    url = config[[1]],
    token = config[[2]],
    org = config[[3]],
    retryOptions = TRUE
  )
  
  tables <- client$query(text = flux_qry)
  
  data.table::rbindlist(tables) %>%
    dplyr::select(order(colnames(.data))) %>%
    dplyr::relocate(.data$time, .after = "_time") %>%
    dplyr::select(-.data$`_time`)
}


#' InfluxDB: write aggregated time series to Ultimate target bucket in loop
#' @description wrapper for \code{\link{write_aggr_to_influxdb}}
#'
#' @param agg_interval aggregation interval (default: "1h")
#' @param agg_function aggregation function (default: "mean")
#' @param bucket_source bucket source (default: "ultimate")
#' @param bucket_target bucket target (default: <bucket_source_<agg_function>_<agg_interval>))
#' @param bucket_org bucket organisation (default: "kwb")
#' @param date_start date start (default: "2021-07-05")
#' @param date_end date end (default: Sys.Date())
#' @param hour_start (default: 0)
#' @param hour_end (default: 12)
#' @param max_days maximum time period in days that should be sent within one
#' query to influxdb (default: 5)
#'
#' @return writes aggregated time series to InfluxDB target bucket in loop
#' @export
#' @importFrom lubridate ymd
#' @importFrom kwb.utils catAndRun
write_aggr_to_influxdb_loop <- function(agg_interval = "1h",
                                        agg_function = "mean",
                                        bucket_source = "ultimate",
                                        bucket_target = sprintf("%s_%s_%s",
                                                                bucket_source,
                                                                agg_function,
                                                                agg_interval),
                                        bucket_org = "kwb",
                                        date_start = "2021-07-05",
                                        date_end = Sys.Date(),
                                        hour_start = 0,
                                        hour_end = 12,
                                        max_days = 5) {
  if (max_days > 0) {
    dates_start <- sprintf("%sT00:00:00Z",
                           seq(
                             lubridate::ymd(date_start),
                             lubridate::ymd(date_end) - max_days,
                             by = sprintf('%d days', max_days)
                           ))
    
    dates_end <- sprintf("%sT00:00:00Z",
                         seq(
                           lubridate::ymd(date_start) + max_days,
                           lubridate::ymd(date_end),
                           by = sprintf('%d days', max_days)
                         ))
    
    
  } else {
    dates_start <-
      seq(lubridate::ymd(date_start), lubridate::ymd(date_end), 1)
    
    if (hour_end == 0) {
      dates_end <- dates_start + 1
    } else {
      dates_end <- dates_start
    }
    
    dates_start <-
      sprintf("%sT%02d:00:00Z", dates_start, hour_start)
    
    dates_end <- sprintf("%sT%02d:00:00Z", dates_end, hour_end)
  }
  
  
  periods_df <- data.frame(start =  dates_start,
                           end =  dates_end)
  
  sapply(
    seq_len(nrow(periods_df)),
    FUN = function(idx) {
      period <- periods_df[idx,]
      
      msg_txt <-
        sprintf(
          "Aggregate raw data (func: '%s', intervall: %s, period: %s - %s) from raw bucket '%s' and write to '%s'",
          agg_function,
          agg_interval,
          period$start,
          period$end,
          bucket_source,
          bucket_target
        )
      kwb.utils::catAndRun(messageText = msg_txt,
                           expr = {
                             write_aggr_to_influxdb(
                               start = period$start,
                               end = period$end,
                               agg_interval = agg_interval,
                               agg_function = agg_function,
                               bucket_source = bucket_source,
                               bucket_target = bucket_target,
                               bucket_org = bucket_org
                             )
                           })
    }
  )
}


#' InfluxDB: write aggregated time series to Ultimate target bucket
#'
#' @param start date start
#' @param end date end
#' @param agg_interval aggregation interval (default: "1h")
#' @param agg_function aggregation function (default: "mean")
#' @param bucket_source bucket source (default: "ultimate")
#' @param bucket_target bucket target (default: <bucket_source_<agg_function>_<agg_interval>))
#' @param bucket_org bucket organisation (default: "kwb")
#' @return writes aggregated time series to InfluxDB target bucket in loop
#' @export
write_aggr_to_influxdb <- function(start,
                                   end,
                                   agg_interval = "1h",
                                   agg_function = "mean",
                                   bucket_source = "ultimate",
                                   bucket_target = sprintf("%s_%s_%s",
                                                           bucket_source,
                                                           agg_function,
                                                           agg_interval),
                                   bucket_org = "kwb") {
  config <- get_env_influxdb_ultimate()
  
  client <- influxdbclient::InfluxDBClient$new(
    url = config[[1]],
    token = config[[2]],
    org = config[[3]],
    retryOptions = TRUE
  )
  
  flux_qry  <- paste0(
    'from(bucket: "ultimate") ',
    '|> range(start: ',
    start,
    ', stop: ',
    end,
    ') ',
    # '|> filter(fn: (r) => r["_measurement"] == "Pilot_B") ',
    '|> aggregateWindow(every: ',
    agg_interval,
    ', fn: ',
    agg_function,
    ', createEmpty: false)',
    '|> to(bucket: "',
    bucket_target,
    '", org: "',
    bucket_org,
    '")'
  )
  
  
  tables <- client$query(text = flux_qry)
  
}


#' InfluxDB: write to InfluxDB in Loop
#'
#' @description wrapper function for \code{\link{write_to_influxdb}}
#'
#' @param tsv_paths vector with tsv_paths with files to be imported by
#' \code{\link{write_to_influxdb}} which relies on \code{\link{read_pentair_data}}
#' @param paths paths list with elements \code{raw_data_dir} and \code{site_code}
#' @param changed_only TRUE if only columns with changing data points 
#' within time series of provided tsv_paths (limited by parameter \code{max_tsv_files},
#' i.e. changes between different tsv splits are not detected!) should be written 
#' to InfluxDB, otherwise FALSE (default: TRUE)
#' @param max_tsv_files maximum number of tsv files to read at once (should be
#' limited due to high RAM demand), default: 5
#' @param batch_size number of data points that are written in one query (default:
#' 5000)
#'
#' @return writes imported data to InfluxDB in Loop
#' @export

write_to_influxdb_loop <- function(tsv_paths,
                                   paths,
                                   changed_only = TRUE,
                                   max_tsv_files = 5,
                                   batch_size = 5000) {
  splits_full <- floor(length(tsv_paths) / max_tsv_files)
  splits_partial <- ceiling(length(tsv_paths) / max_tsv_files)
  
  idx_start <- 1 - max_tsv_files
  idx_end <- 0
  
  for (split in seq_len(splits_full)) {
    idx_start <- idx_start + max_tsv_files
    idx_end <- idx_end + max_tsv_files
    cat(sprintf(
      "Split: %d (tsv_paths_idx: %d - %d)\n",
      split,
      idx_start,
      idx_end
    ))
    write_to_influxdb(tsv_paths = tsv_paths[idx_start:idx_end],
                      paths = paths,
                      changed_only = changed_only,
                      batch_size = batch_size)
  }
  if (splits_partial - splits_full == 1) {
    idx_start <- idx_start + max_tsv_files
    idx_end <- length(tsv_paths)
    cat(
      sprintf(
        "Split partial: %d (tsv_paths_idx: %d - %d)\n",
        splits_partial,
        idx_start,
        idx_end
      )
    )
    write_to_influxdb(tsv_paths = tsv_paths[idx_start:idx_end],
                      paths = paths,
                      batch_size = batch_size)
  }
}


#' InfluxDB: write to InfluxDB
#'

#' @param tsv_paths vector with tsv_paths with files to be imported by
#' a modification of \code{\link{read_pentair_data}}
#' @param paths paths list with elements \code{raw_data_dir} and \code{site_code}
#' @param changed_only TRUE if only columns with changing data points 
#' within time series of  provided tsv_path should be written to InfluxDB, otherwise 
#' FALSE (default: TRUE)
#' @param batch_size number of data points that are written in one query (default:
#' 5000)
#' @return writes imported data to InfluxDB
#' @export
#' @importFrom dplyr select group_by summarise pull
#' @importFrom tidyselect all_of
#' @importFrom janitor clean_names
#' @importFrom tidyr pivot_wider
write_to_influxdb <- function(tsv_paths,
                              paths,
                              changed_only = TRUE,
                              batch_size = 5000) {
  config <- get_env_influxdb_ultimate()
  
  tmp_wide <- read_pentair_data(
    raw_data_dir = paths$raw_data_dir,
    raw_data_files = tsv_paths,
    meta_file_path = ""
  ) %>%
    dplyr::select(tidyselect::all_of(c(
      "DateTime", "ParameterCode", "ParameterValue"
    ))) %>%
    dplyr::group_by(.data$DateTime, .data$ParameterCode) %>%
    dplyr::summarise(ParameterValue = mean(.data$ParameterValue)) %>%
    dplyr::filter(!is.na(.data$ParameterValue),!is.infinite(.data$ParameterValue)) %>%
    tidyr::pivot_wider(names_from = .data$ParameterCode,
                       values_from = .data$ParameterValue) %>%
    janitor::clean_names() %>%
    dplyr::mutate(site_code = paths$site_code) %>%
    as.data.frame()
  
  
  
  field_cols <-
    setdiff(names(tmp_wide), c("date_time", "site_code"))
  
  tmp_long <- tmp_wide %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(field_cols),
      names_to = "ParameterCode",
      values_to = "ParameterValue"
    ) %>%
    dplyr::filter(!is.na(.data$ParameterValue),
                  !is.infinite(.data$ParameterValue))
  
  
  fieldnames <- tmp_long %>%
    dplyr::group_by(.data$ParameterCode) %>%
    dplyr::summarise(
      min = min(.data$ParameterValue),
      max = max(.data$ParameterValue),
      diff = max - min
    )
  
  if(changed_only) {
  fieldnames <- fieldnames %>%
    dplyr::filter(diff != 0) %>%
    dplyr::pull(.data$ParameterCode)
   
  tmp_long <- tmp_long %>%
    dplyr::filter(.data$ParameterCode %in% fieldnames)
  } else {
    fieldnames <- fieldnames %>% dplyr::pull(.data$ParameterCode)
  }
  
  ### R Client
  
  #remotes::install_github("influxdata/influxdb-client-r")
  
  client <-
    influxdbclient::InfluxDBClient$new(
      url = config[[1]],
      token = config[[2]],
      org = config[[3]],
      retryOptions = TRUE
    )
  
  # Ready status
  #ready <- client$ready()
  
  # Health info
  
  #client$health()
  
  system.time(expr = {
    sapply(fieldnames, function(field_col) {
      tmp_dat <- tmp_long %>%
        dplyr::filter(.data$ParameterCode == field_col) %>%
        tidyr::pivot_wider(names_from = .data$ParameterCode,
                           values_from = .data$ParameterValue) %>%
        as.data.frame()
      
      ids <- seq(ceiling(nrow(tmp_dat) / batch_size))
      requests <- tibble::tibble(
        id = ids,
        idx_start = 1 + (ids - 1) * batch_size,
        idx_end = ids * batch_size
      )
      requests$idx_end[nrow(requests)] <- nrow(tmp_dat)
      sapply(ids, function(id) {
        tmp_dat_split <-
          tmp_dat[requests$idx_start[id]:requests$idx_end[id], ]
        
        msg_txt <-
          sprintf(
            "'%s' (%d/%d), write request %d/%d (%s - %s, data points: %d, temporal resolution (avg): %ds) to InfluxDB",
            field_col,
            which(fieldnames == field_col),
            length(fieldnames),
            id,
            length(ids),
            min(tmp_dat_split$date_time),
            max(tmp_dat_split$date_time),
            nrow(tmp_dat_split),
            round(as.numeric(
              difftime(
                max(tmp_dat_split$date_time),
                min(tmp_dat_split$date_time),
                units = "secs"
              )
            ) / nrow(tmp_dat_split),
            0)
          )
        kwb.utils::catAndRun(messageText = msg_txt, expr = {
          client$write(
            tmp_dat_split,
            bucket = "ultimate",
            precision = "s",
            measurementCol = "site_code",
            tagCols = NULL,
            #"site_code",
            fieldCols = field_col,
            timeCol = "date_time"
          )
        })
      })
    })
  })
}



#' Helper Function: Download Nextcloud Files from a Directory
#'
#' @param dir_cloud directory on Nextcloud
#' @param dir_local directory on local computer. If not existing it will be created
#' @param file_pattern file pattern to be used as download filter
#' (default: "Project\\.xls$")
#' @return downloads all files from cloud into local folder fullfilling file_pattern 
#' and returns the \code{file}, i.e. filename 
#' @export
#' @importFrom fs dir_create
#' @importFrom kwb.nextcloud list_files download_files
#' @importFrom stringr str_detect
#' @importFrom dplyr filter
#' @examples
#' \dontrun{
#' #1 Open RStudio and run usethis::edit_r_environ()
#' #2 In the opened window add the required environment variables
#' ### NEXTCLOUD_URL = "https://<replace-with-nextcloud-cloud-url>"
#' ### NEXTCLOUD_USER = "<your-nextcloud-username>" # your username
#' ### NEXTCLOUD_PASSWORD = "your-nextcloud-app-password" ### see details below
#' #3 For creating <your-nextcloud-app-password>:
#' #3.1 go to: https://replace-with-nextcloud-url/index.php/settings/user/security
#' #3.2 scroll down to create new app password
#' #3.3 select a name e.g. r-script and copy the token and replace your-nextcloud-app-password
#' #4 Finally you need to restart Rstudio and proceed with the code below:
#' paths_list <- list(site_code = "Pilot_A",
#' common_path = "ultimate/raw_data_pilots/<site_code>/data",
#' dir_cloud = "projects/<common_path>",
#' dir_local = "C:/kwb/projects/<common_path>")
#'
#' paths <- kwb.utils::resolve(paths_list)

#' download_nextcloud_files(dir_cloud = paths$dir_cloud,
#' dir_local = paths$dir_local,
#' file_pattern = "Project\\.xls$"
#' )
#' }
download_nextcloud_files <- function(dir_cloud,
                                     dir_local,
                                     file_pattern = "Project\\.xls$")
{
  if(!check_env_nextcloud()) {
    env_vars <- paste0(sprintf("NEXTCLOUD_%s", c("URL", "USER", "PASSWORD")),
                       collapse = ", ")
    message(sprintf(paste0("Not all NEXTCLOUD environment variables are defined. ",
                           "Please define all of them '%s' with usethis::edit_r_environ()"),
                           env_vars))
  } else {
  
  if (!dir.exists(dir_local)) {
    fs::dir_create(dir_local, recurse = TRUE)
  }
  
  cloud_files <- kwb.nextcloud::list_files(dir_cloud,
                                           full_info = TRUE) %>%
    dplyr::filter(stringr::str_detect(.data$file,
                                      pattern = file_pattern))
  
  
  kwb.nextcloud::download_files(href = cloud_files$href,
                                target_dir = dir_local)
  cloud_files$file
}
}

#' Helper Function: check if all environment variables for Nextcloud are defined
#'
#' @return TRUE if all defined, FALSE otherwise
#' @export
#'
check_env_nextcloud <- function() {
  all(sapply(Sys.getenv(sprintf(
    "NEXTCLOUD_%s", c("URL", "USER", "PASSWORD")
  )), nchar) > 0)
}

#' Helper Function: check if all environment variables for ULTIMATE InfluxDB are
#' defined
#'
#' @return TRUE if all defined, FALSE otherwise
#' @export
#'
check_env_influxdb_ultimate <- function() {
  all(sapply(Sys.getenv(sprintf(
    "ULTIMATE_INFLUXDB_%s",
    c("URL", "TOKEN", "ORG")
  )), nchar) > 0)
}

#' Helper Function: get influxdb config for Ultimate if defined
#' defined
#'
#' @return list with influxdb config
#' @export
#'
get_env_influxdb_ultimate <- function() {
  stopifnot(check_env_influxdb_ultimate())
  
  as.list(Sys.getenv(sprintf(
    "ULTIMATE_INFLUXDB_%s",
    c("URL", "TOKEN", "ORG")
  )))
}
