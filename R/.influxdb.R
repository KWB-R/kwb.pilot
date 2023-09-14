if(FALSE) {
  library(kwb.pilot)
  
  ################################################################################
  ### 1. Write Pilot Plant Raw Data to InfluxDB Cloud
  ################################################################################
  
  ################################################################################
  ### 1.1 Pilot A
  ################################################################################
  
  paths_list <- list(
    influx_url = Sys.getenv("ULTIMATE_INFLUXDB_URL"),
    influx_token = Sys.getenv("ULTIMATE_INFLUXDB_TOKEN"),
    influx_org = Sys.getenv("ULTIMATE_INFLUXDB_ORG"),
    project_root = "C:/kwb/projects",
    common_root = "ultimate/raw_data_pilots/<site_code>/data",
    site_code = "Pilot_A",
    cloud_raw_data_dir = "projects/<common_root>",
    raw_data_dir = "<project_root>/<common_root>"
  )
  
  paths <- kwb.utils::resolve(paths_list)
  
  download_nextcloud_files(dir_cloud = paths$cloud_raw_data_dir, 
                           dir_local = paths$raw_data_dir)
  
  tsv_paths <- list.files(
    path = paths$raw_data_dir,
    full.names = TRUE,
    pattern = "xls$"
  )
  
  write_to_influxdb_loop(
    tsv_paths = tsv_paths,
    paths = paths,
    max_tsv_files = 10,
    batch_size = 5000
  )
  
  ################################################################################
  ### 1.2 Pilot B
  ################################################################################
  
  paths_list <- list(
    influx_url = Sys.getenv("ULTIMATE_INFLUXDB_URL"),
    influx_token = Sys.getenv("ULTIMATE_INFLUXDB_TOKEN"),
    influx_org = Sys.getenv("ULTIMATE_INFLUXDB_ORG"),
    project_root = "C:/kwb/projects",
    common_root = "ultimate/raw_data_pilots/<site_code>/data",
    site_code = "Pilot_B",
    cloud_raw_data_dir = "projects/<common_root>",
    raw_data_dir = "<project_root>/<common_root>"
  )
  
  paths <- kwb.utils::resolve(paths_list)
  
  download_nextcloud_files(dir_cloud = paths$cloud_raw_data_dir, 
                           dir_local = paths$raw_data_dir)
  
  tsv_paths <- list.files(
    path = paths$raw_data_dir,
    full.names = TRUE,
    pattern = "xls$"
  )
  
  write_to_influxdb_loop(
    tsv_paths = tsv_paths,
    paths = paths,
    max_tsv_files = 5,
    batch_size = 5000
  )
  
  
  
  ################################################################################
  ### 2. Aggregate "raw-data" (bucket: "ultimate") and save in new buckets
  ### 'ultimate_mean_1d': mean 'daily' values
  ### 'ultimate_mean_1h': mean 'hourly' values
  ################################################################################
  
  date_start <- "2021-07-05"
  date_end <- Sys.Date()
  
  # write to bucket: 'ultimate_mean_1d': mean 'daily' values
  write_aggr_to_influxdb(start = date_start,
                         end = date_end,
                         agg_interval = "1d")
  
  # write to bucket: 'ultimate_mean_1h': mean 'hourly' values
  write_aggr_to_influxdb(start = date_start,
                         end = date_end,
                         agg_interval = "1h")
  
  # write to bucket: 'ultimate_mean_10m': mean '10 minutes' values
  write_aggr_to_influxdb(start = date_start,
                         end = date_end,
                         agg_interval = "10m")
  
  write_aggr_to_influxdb_loop(agg_interval = "10m",
                              max_days = 2,
                              date_start = date_start)
  
  # write to bucket: 'ultimate_mean_1m': mean '1 minutes' values
  write_aggr_to_influxdb(start = date_start,
                         end = date_end,
                         agg_interval = "1m")
  
  mean_1m_1 <- get_pivot_data(agg_interval = "1m",
                              date_stop = "2021-10-31")
  mean_1m_2 <- get_pivot_data(agg_interval = "1m",
                              date_start = "2021-11-01")
  
  mean_1m <- dplyr::bind_rows(mean_1m_1,
                              mean_1m_2)
  rm(mean_1m_1)
  rm(mean_1m_2)
  
  cols_pilot_a <-
    c("time", stringr::str_subset(names(mean_1m), "Pilot_A"))
  
  cols_pilot_b <-
    c("time", stringr::str_subset(names(mean_1m), "Pilot_B"))
  
  
  pilotplants_1m <- list(`mean_1m_pilot-a` = mean_1m[, ..cols_pilot_a],
                         `mean_1m_pilot-b` = mean_1m[, ..cols_pilot_b])
  
  
  
  period <-
    paste(stringr::str_replace(as.character(range(mean_1m$time)), " ", "T"),
          collapse = "_") %>% stringr::str_remove_all(":|-")
  
  openxlsx::write.xlsx(
    pilotplants_1m,
    file = sprintf("ultimate_pilots_1m_%s.xlsx", period),
    overwrite = TRUE
  )
  
  
  mean_1d <- get_pivot_data(agg_interval = "1d")
  mean_1h <- get_pivot_data(agg_interval = "1h")
  mean_10m <- get_pivot_data(agg_interval = "10m")
  mean_1m_1 <- get_pivot_data(agg_interval = "1m",
                              date_stop = "2021-09-01")
  mean_1m_2 <- get_pivot_data(agg_interval = "1m",
                              date_start = "2021-10-02",
                              date_stop = "2021-12-31")
  mean_1m_3 <- get_pivot_data(agg_interval = "1m",
                              date_start = "2022-01-01")
  mean_1m <- dplyr::bind_rows(mean_1m_1,
                              mean_1m_2) %>%
    dplyr::bind_rows(mean_1m_3)
  rm(mean_1m_1)
  rm(mean_1m_2)
  rm(mean_1m_3)
  
  pilots_to_csv <- function(dataset) {
    dataset_string <- deparse(substitute(dataset))
    
    period <-
      paste(stringr::str_replace(as.character(range(dataset$time)), " ", "T"),
            collapse = "_") %>% stringr::str_remove_all(":|-")
    
    readr::write_csv2(dataset,
                      file = sprintf(
                        "ultimate_pilots_%s_%s.csv",
                        stringr::str_replace(dataset_string , "_", "-"),
                        period
                      ))
  }
  
  pilots_to_csv(mean_1d)
  pilots_to_csv(mean_1h)
  pilots_to_csv(mean_10m)
  pilots_to_csv(mean_1m)
  
  pilotplants <- list(mean_1d = mean_1d,
                      mean_1h = mean_1h,
                      mean_10m = mean_10m)
  
  period <-
    paste(stringr::str_replace(as.character(range(
      pilotplants$mean_10m$time
    )), " ", "T"),
    collapse = "_") %>% stringr::str_remove_all(":|-")
  
  
  openxlsx::write.xlsx(
    pilotplants,
    file = sprintf("ultimate_pilots_1d-1h-10m_%s.xlsx", period),
    overwrite = TRUE
  )
  
  period <-
    paste(stringr::str_replace(as.character(range(mean_1m$time)), " ", "T"),
          collapse = "_") %>% stringr::str_remove_all(":|-")
  
  
  openxlsx::write.xlsx(
    mean_1m,
    file = sprintf("ultimate_pilots_mean-1m_%s.xlsx", period),
    overwrite = TRUE
  )
  
  
  
}
