if(FALSE) {

library(kwb.pilot)

################################################################################  
### 1. Write Pilot Plant Raw Data to InfluxDB Cloud 
################################################################################  
  
################################################################################  
### 1.1 Pilot A
################################################################################

paths_list <- list(influx_url = Sys.getenv("ULTIMATE_INFLUXDB_URL"),
                   influx_token = Sys.getenv("ULTIMATE_INFLUXDB_TOKEN"),
                   influx_org = Sys.getenv("ULTIMATE_INFLUXDB_ORG"),
                   project_root = "C:/kwb/projects/ultimate",
                   site_code = "Pilot_A",
                   raw_data_dir = "<project_root>/raw_data_pilots/<site_code>/data"
)

paths <- kwb.utils::resolveAll(paths_list)

tsv_paths <- list.files(path = paths$raw_data_dir,
                        full.names = TRUE,
                        pattern = "xls$")

write_to_influxdb_loop(tsv_paths = tsv_paths,
                       paths = paths, 
                       max_tsv_files = 17, 
                       batch_size = 5000)

################################################################################  
### 1.2 Pilot B
################################################################################

paths_list <- list(influx_url = Sys.getenv("ULTIMATE_INFLUXDB_URL"),
                   influx_token = Sys.getenv("ULTIMATE_INFLUXDB_TOKEN"),
                   influx_org = Sys.getenv("ULTIMATE_INFLUXDB_ORG"),
                   project_root = "C:/kwb/projects/ultimate",
                   site_code = "Pilot_B",
                   raw_data_dir = "<project_root>/raw_data_pilots/<site_code>/data"
)

paths <- kwb.utils::resolveAll(paths_list)

tsv_paths <- list.files(path = paths$raw_data_dir,
                        full.names = TRUE,
                        pattern = "xls$")

write_to_influxdb_loop(tsv_paths = tsv_paths,
                       paths = paths, 
                       max_tsv_files = 5, 
                       batch_size = 5000)
}


write_to_influxdb_loop <- function(tsv_paths,
                                   paths,
                                   max_tsv_files = 5,
                                   batch_size = 5000) {
splits_full <- floor(length(tsv_paths)/max_tsv_files)
splits_partial <- ceiling(length(tsv_paths)/max_tsv_files)

idx_start <- 1 - max_tsv_files
idx_end <- 0 

for(split in seq_len(splits_full)) {
  idx_start <- idx_start + max_tsv_files
  idx_end <- idx_end + max_tsv_files
  cat(sprintf("Split: %d (tsv_paths_idx: %d - %d)\n", 
              split,
              idx_start, 
              idx_end))
  write_to_influxdb(tsv_paths = tsv_paths[idx_start:idx_end], 
                    paths = paths,
                    batch_size = batch_size)
} 
if (splits_partial - splits_full == 1) {
  idx_start <- idx_start + max_tsv_files
  idx_end <- length(tsv_paths)
  cat(sprintf("Split partial: %d (tsv_paths_idx: %d - %d)\n", 
              splits_partial,
              idx_start, 
              idx_end))
  write_to_influxdb(tsv_paths = tsv_paths[idx_start:idx_end], 
                    paths = paths,
                    batch_size = batch_size)
}
}

write_to_influxdb <- function(tsv_paths, 
                              paths, 
                              batch_size = 5000) {

tmp_wide <- kwb.pilot::read_pentair_data(
  raw_data_dir = paths$raw_data_dir,
  raw_data_files = tsv_paths,
  meta_file_path = "") %>% 
  dplyr::select(tidyselect::all_of(c("DateTime", "ParameterCode", "ParameterValue"))) %>%
  dplyr::group_by(.data$DateTime, .data$ParameterCode) %>%
  dplyr::summarise(ParameterValue = mean(.data$ParameterValue)) %>%  
  dplyr::filter(!is.na(ParameterValue),
                !is.infinite(ParameterValue)) %>%
  tidyr::pivot_wider(names_from = "ParameterCode",
                     values_from = "ParameterValue") %>%
  janitor::clean_names() %>% 
  dplyr::mutate(site_code = paths$site_code) %>% 
  as.data.frame() 



field_cols <- setdiff(names(tmp_wide), c("date_time", "site_code"))

tmp_long <- tmp_wide %>%
  tidyr::pivot_longer(cols = tidyselect::all_of(field_cols), 
                      names_to = "ParameterCode",
                      values_to = "ParameterValue") %>% 
  dplyr::filter(!is.na(ParameterValue),
                !is.infinite(ParameterValue)) 


fieldnames_with_changing_data <- tmp_long %>%  
  dplyr::group_by(.data$ParameterCode) %>% 
  dplyr::summarise(min = min(ParameterValue), 
                   max = max(ParameterValue),
                   diff = max-min) %>% 
  dplyr::filter(diff != 0) %>% 
  dplyr::pull(.data$ParameterCode)

tmp_long <- tmp_long %>% 
  dplyr::filter(.data$ParameterCode %in% fieldnames_with_changing_data)


### R Client 
  
  #remotes::install_github("influxdata/influxdb-client-r")
  
  client <- influxdbclient::InfluxDBClient$new(url = paths$influx_url,
                                               token = paths$influx_token,
                                               org = paths$influx_org,
                                               retryOptions = TRUE)
  
  # Ready status
  #ready <- client$ready()
  
# Health info

#client$health() 

system.time(expr = {
  sapply(fieldnames_with_changing_data, function(field_col) {
    
    tmp_dat <- tmp_long %>%  
      dplyr::filter(ParameterCode == field_col) %>% 
      tidyr::pivot_wider(names_from = "ParameterCode", 
                         values_from = "ParameterValue") %>% 
      as.data.frame()
    
    ids <- seq(ceiling(nrow(tmp_dat)/batch_size))
    requests <- tibble::tibble(id = ids,
                               idx_start = 1+(ids-1)*batch_size,
                               idx_end = ids*batch_size
    )
    requests$idx_end[nrow(requests)] <- nrow(tmp_dat)
    sapply(ids, function(id) {
      
      tmp_dat_split <-  tmp_dat[requests$idx_start[id]:requests$idx_end[id], ]
      
      msg_txt <- sprintf("'%s' (%d/%d), write request %d/%d (%s - %s, data points: %d, temporal resolution (avg): %ds) to InfluxDB",
                         field_col,
                         which(fieldnames_with_changing_data == field_col), 
                         length(fieldnames_with_changing_data),
                         id, 
                         length(ids),
                         min(tmp_dat_split$date_time),
                         max(tmp_dat_split$date_time),
                         nrow(tmp_dat_split),
                         round(as.numeric(difftime(max(tmp_dat_split$date_time),
                                                   min(tmp_dat_split$date_time),
                                                   units = "secs"))/nrow(tmp_dat_split),
                               0)
      )
      kwb.utils::catAndRun(messageText = msg_txt,expr = {
        client$write(tmp_dat_split, 
                     bucket = "ultimate", 
                     precision = "s",
                     measurementCol = "site_code",
                     tagCols = NULL, #"site_code",
                     fieldCols = field_col,
                     timeCol = "date_time")
      })
    }
    )
  })
})
}

#tables <- client$query('from(bucket: "ultimate") |> range(start: -30000h)')
#tables

# ### Python Client (alternative, but unused for now as R client works)
# 
# env_name <- "influxdb"
# kwb.python::conda_py_install(env_name = env_name, 
#                              pkgs = list(conda = c("python=3.9"),
#                                          py = "influxdb-client==1.23.0")
# )
# kwb.python::conda_export(env_name, export_dir = ".")
# 
# reticulate::use_condaenv(env_name)

