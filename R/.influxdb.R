paths_list <- list(influx_url = Sys.getenv("ULTIMATE_INFLUXDB_URL"),
                   influx_token = Sys.getenv("ULTIMATE_INFLUXDB_TOKEN"),
                   influx_org = Sys.getenv("ULTIMATE_INFLUXDB_ORG")
)

paths <- kwb.utils::resolveAll(paths_list)

tsv_paths <- list.files("C:/kwb/projects/ultimate/raw_data_pilots/Pilot_B/data", 
                        full.names = TRUE,
                        pattern = "xls$")


pilot_b <- kwb.pilot::read_pentair_data(#raw_data_dir = "C:/kwb/projects/ultimate/raw_data_pilots/Pilot_B/",
  raw_data_files = tsv_paths[51:100],
  meta_file_path = "") %>% 
  dplyr::select(tidyselect::all_of(c("DateTime", "ParameterCode", "ParameterValue"))
  )

tmp_wide <- pilot_b %>% 
  dplyr::filter(!is.na(ParameterValue),
                !is.infinite(ParameterValue)) %>%
  tidyr::pivot_wider(names_from = "ParameterCode",
                     values_from = "ParameterValue") %>%
  janitor::clean_names() %>% 
  dplyr::mutate(site_code = "Pilot_B") %>% 
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


+### R Client 

remotes::install_github("influxdata/influxdb-client-r")

client <- influxdbclient::InfluxDBClient$new(url = paths$influx_url,
                                             token = paths$influx_token, 
                                             org = paths$influx_org,
                                             retryOptions = TRUE)

# Ready status
ready <- client$ready()

# Health info

client$health() 

sapply(fieldnames_with_changing_data, function(field_col) {

tmp_dat <- tmp_long %>%  
  dplyr::filter(ParameterCode == field_col) %>% 
  tidyr::pivot_wider(names_from = "ParameterCode", 
                     values_from = "ParameterValue") %>% 
  as.data.frame()

batch_size <- 5000
ids <- seq(ceiling(nrow(tmp_dat)/batch_size))
requests <- tibble::tibble(id = ids,
                           idx_start = 1+(request_id-1)*batch_size,
                           idx_end = request_id*batch_size
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


#tables <- client$query('from(bucket: "ultimate") |> range(start: -30000h)')
#tables

### Python Client (alternative, but unused for now as R client works)

env_name <- "influxdb"
kwb.python::conda_py_install(env_name = env_name, 
                             pkgs = list(conda = c("python=3.9"),
                                         py = "influxdb-client==1.23.0")
)
kwb.python::conda_export(env_name, export_dir = ".")

reticulate::use_condaenv(env_name)

