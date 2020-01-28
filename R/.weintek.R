if(FALSE) {
  pkgs_cran <- c("dplyr", "remotes", "tidyr")
  
  install.packages(pkgs_cran)
  
  
  remotes::install_github("kwb-r/aquanes.report")
  remotes::install_github("kwb-r/kwb.utils")
}


paths_list <- list(
  servername = "server_name",
  rawdata = "<servername>/projekte$/WWT_Department/Projects/SULEMAN/Exchange/10_rawdata",
  online =  "<rawdata>/online_data"
)

paths <- kwb.utils::resolve(paths_list)


files <- fs::dir_ls(paths$online,recurse = TRUE, regexp = "^[^~].*\\.xlsx$")

paraname_site <- basename(dirname(files))

`%>%` <- magrittr::`%>%`

# read_weintek -----------------------------------------------------------------
read_weintek <- function(path, tz = "CET", dbg = TRUE)
{
  if(dbg) message(sprintf("Importing file: %s", path))
  df <- kwb.utils::renameColumns(readxl::read_xlsx(path), renamings = list(
    Datum = "DateTime",
    Date = "DateTime",
    Time = "time",
    Zeit = "time",
    Millisekunde = "Millisecond",
    "32-bit Float" = "ParameterValue"
  ))
  
  df <- df[, intersect(names(df), c("DateTime", "Millisecond", "ParameterValue"))]
  
  aquanes.report:::set_timezone(df,
                                tz = tz,
                                col_datetime = "DateTime")
}


read_weintek_batch <- function(files, tz = "CET", dbg = TRUE) {
  
  data_list <- setNames(lapply(files, function(file) {
    read_weintek(file, tz, dbg)}), nm = paraname_site)
  
  
  
  data_df <- data.table::rbindlist(data_list, fill = TRUE, idcol = "paraname_site") %>%
    tidyr::separate(col = "paraname_site",
                    into = c("ParameterName", "SiteName"),
                    sep = "_") %>%
    dplyr::mutate(DataType = "raw") %>%
    dplyr::select(- Millisecond) %>%
    aquanes.report::remove_duplicates()
  
  data_df
}



long_to_wide <- function(df) {
  df  %>%  
    dplyr::mutate(ParameterName_SiteName = sprintf("%s_%s", 
                                                   .data$ParameterName, 
                                                   .data$SiteName)) %>%  
    dplyr::select(.data$DateTime, 
                  .data$ParameterName_SiteName, 
                  .data$ParameterValue) %>%  
    tidyr::spread(key = .data$ParameterName_SiteName, 
                  value = .data$ParameterValue)
}


weintek_data_raw <- read_weintek_batch(files)

# weintek_data_raw  %>%
#   dplyr::group_by(DateTime) %>%
#   dplyr::summarise(n = dplyr::n())


export_data <- function(df_long, 
                        export_dir = "//medusa/processing/suleman", 
                        dbg = TRUE) {
  
  fs::dir_create(sprintf("%s/data", export_dir))
  
  df_name <- deparse(substitute(df_long))
  
  df_file <- sprintf("%s/data/%s.csv", 
                     export_dir, 
                     df_name)
  
  
  kwb.utils::catAndRun(sprintf("Export data to %s", df_file), 
                       expr = {
                         df_wide <- long_to_wide(df_long)
                         readr::write_csv2(df_wide, path = df_file)
                       }, 
                       dbg = dbg)
}

plot_data <- function(df_long, 
                      export_dir = "//medusa/processing/suleman", 
                      dbg = TRUE) {
  
  fs::dir_create(sprintf("%s/plots", export_dir))
  
  
  df_name <- deparse(substitute(df_long))
  
  
  plot_file <- sprintf("%s/plots/%s.html", 
                       export_dir, 
                       df_name)
  
  
  
  kwb.utils::catAndRun(sprintf("Export plot: %s", plot_file), 
                       expr = {
                         g1 <- df_long %>%
                           ggplot2::ggplot(mapping = ggplot2::aes(x = DateTime,
                                                                  y = ParameterValue,
                                                                  col = SiteName)) +
                           ggplot2::facet_wrap(~ParameterName, scales = "free_y", ncol = 1) +
                           ggplot2::geom_point() +
                           ggplot2::theme_bw()
                         
                         
                         withr::with_dir(sprintf("%s/plots", export_dir), 
                                         code = { 
                                           plotly::ggplotly(g1) %>%  htmlwidgets::saveWidget(basename(plot_file),
                                                                                             selfcontained = FALSE,
                                                                                             title = df_name)})}, 
                       dbg = dbg
  )
}


weintek_data_10min <- weintek_data_raw %>%  
  aquanes.report::group_datetime(by = 600)

weintek_data_1hour <- weintek_data_raw %>%  
  aquanes.report::group_datetime(by = 3600)

export_data(weintek_data_raw)
export_data(weintek_data_10min)
export_data(weintek_data_1hour)


plot_data(weintek_data_1hour)
plot_data(weintek_data_10min)
#plot_data(weintek_data_raw)


