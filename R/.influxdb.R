paths_list <- list(influx_url = Sys.getenv("ULTIMATE_INFLUXDB_URL"),
                   influx_token = Sys.getenv("ULTIMATE_INFLUXDB_TOKEN"),
                   influx_org = Sys.getenv("ULTIMATE_INFLUXDB_ORG")
)

paths <- kwb.utils::resolveAll(paths_list)

tsv_paths <- list.files("C:/kwb/projects/ultimate/raw_data_pilots/Pilot_B/data", 
                        full.names = TRUE,
                        pattern = "xls$")


pilot_b <- kwb.pilot::read_pentair_data(#raw_data_dir = "C:/kwb/projects/ultimate/raw_data_pilots/Pilot_B/",
  raw_data_files = tsv_paths[1:50],
  meta_file_path = "") 

tmp <- pilot_b[, c("DateTime", "ParameterCode", "ParameterValue", "Source", "SiteCode")] %>% 
  dplyr::filter(!is.na(ParameterValue))


tmp_wide <- tmp %>% 
  tidyr::pivot_wider(names_from = "ParameterCode",
                     values_from = "ParameterValue") %>%
  janitor::clean_names() %>% 
  dplyr::mutate(site_code = "Pilot_B") %>% 
  as.data.frame() 


field_cols <- setdiff(names(tmp_wide), c("date_time", "source", "site_code"))

### R Client 

remotes::install_github("influxdata/influxdb-client-r")

client <- influxdbclient::InfluxDBClient$new(url = paths$influx_url,
                                             token = paths$influx_token, 
                                             org = paths$influx_org,
                                             retryOptions = TRUE)

# Ready status
ready <- client$ready()

# Health info

client$health() 

client$write(tmp_wide[,1:177],
             bucket = "tmp", 
             precision = "s",
             measurementCol = "source",
             tagCols = "site_code",
             fieldCols = field_cols[1:174],
             timeCol = "date_time")


tmp_wide[20001:30000,1:177] %>% 
  dplyr::select(
    tidyselect::(
      ~!all(is.na(.x))
    )
  )


tables <- client$query('from(bucket: "tmp") |> range(start: -30000h)')
tables


data <- data.frame(
  name = replicate(2, "sensors"),
  sensor_id = c("LM101", "LM102"),
  temperature = c(71.4, 67.3),
  humidity = c(47, 59),
  time = c(Sys.time(),Sys.time())
)

client$write(data,bucket = "tmp", precision = "ms",
             measurementCol = "name",
             tagCols = c("sensor_id"),
             fieldCols = c("temperature", "humidity"),
             timeCol = "time")

pilot_b


tables <- client$query('from(bucket: "tmp") |> range(start: -6h)')
tables


data <- readr::read_csv("influxtest.txt", col_names = TRUE) %>% as.data.frame()



response <- client$write(x = data, 
                         bucket = "tmp",
                         precision = "us",
                         measurementCol = "name",
                         tagCols = c("region", "sensor_id"),
                         fieldCols = c("altitude", "temperature"),
                         timeCol = "time")



#tmp <- pilot_b[1:10,c("DateTime", "ParameterCode", "ParameterValue", "ParameterName", "Source")] #%>% 
#  tidyr::pivot_wider(names_from = "ParameterCode", 
#                     values_from = "ParameterValue") 
data <- readr::read_csv("influxtest.txt", col_names = TRUE) %>% as.data.frame()

response <- client$write(data, 
                         bucket = "tmp", 
                         precision = "us",
                         measurementCol = "name",
                         tagCols = c("region", "sensor_id"),
                         fieldCols = c("altitude", "temperature"),
                         timeCol = "time")

data <- client$query('from(bucket: "tmp") |> range(start: -1h) ')
data

### Python Client 

env_name <- "influxdb"
kwb.python::conda_py_install(env_name = env_name, 
                             pkgs = list(conda = c("python=3.9"),
                                         py = "influxdb-client==1.23.0")
)
kwb.python::conda_export(env_name, export_dir = ".")

reticulate::use_condaenv(env_name)


client <- influxdbclient::InfluxDBClient$new(url = paths$influx_url,
                                             token = paths$influx_token, 
                                             org = paths$influx_org)

write_api = client$writeApi$)
query_api = client.query_api()



write_apiwrite(bucket="tmp", record=data[1,])
p = write_api Point("my_measurement").tag("location", "Prague").field("temperature", 25.3)

write_api.write(bucket=bucket, record=p)


response <- client$write(x = data, 
                         bucket = "ultimate",
                         precision = "s",
                         measurementCol = "name",
                         tagCols = c("region", "sensor_id"),
                         fieldCols = c("altitude", "temperature"),
                         timeCol = "time")

client$health()

