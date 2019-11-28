paths_list <- list(
  servername = "server_name",
  rawdata = "//<servername>/projekte$/WWT_Department/Projects/SULEMAN/Exchange/10_rawdata",
  online =  "<rawdata>/online_data"
)

paths <- kwb.utils::resolve(paths_list, servername = "xxx")


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

weintek_data_raw <- read_weintek_batch(files)

weintek_data_raw  %>%
  dplyr::group_by(DateTime) %>%
  dplyr::summarise(n = dplyr::n())



g1 <- weintek_data_raw %>%
#aquanes.report::group_datetime(by = 3600) %>%
ggplot2::ggplot(mapping = ggplot2::aes(x = DateTime,
                                       y = ParameterValue,
                                       col = SiteName)) +
  ggplot2::facet_wrap(~ParameterName, scales = "free_y", ncol = 1) +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

plotly::ggplotly(g1) %>%  htmlwidgets::saveWidget("weintek_raw.html",
                                                  title = "weintek",)

