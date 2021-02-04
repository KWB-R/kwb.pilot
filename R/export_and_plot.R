#' Helper function: transform "long" to "wide"
#' @param df data frame in long format (as retrieved by \code{kwb.pilot::group_datetime})
#' @importFrom dplyr select mutate
#' @importFrom tidyr spread
#' @export
#'
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




#' CSV data export in "wide" format
#'
#' @param df_long data frame in long format (as retrieved by \code{kwb.pilot::group_datetime})
#' @param export_dir path to export directory
#' @param dbg debug messages (default: TRUE)
#'
#' @return tranforms data in "long" into "wide" format and writes into CSV file
#' @export
#' @importFrom fs dir_create
#' @importFrom kwb.utils catAndRun
export_data <- function(df_long,
                        export_dir,
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

#' Export interactive HTML plot with "plotly"
#'
#' @param df_long data frame in long format (as retrieved by \code{kwb.pilot::group_datetime})
#' @param export_dir path to export directory
#' @param dbg debug messages (default: TRUE)
#' @return interactive HTML plots in subdirectory "<export_dir>/plots/"
#' @export
#' @importFrom withr with_dir
#' @importFrom htmlwidgets saveWidget
#' @importFrom plotly ggplotly
#'
plot_data <- function(df_long,
                      export_dir,
                      dbg = TRUE) {
  fs::dir_create(sprintf("%s/plots", export_dir))
  
  
  df_name <- deparse(substitute(df_long))
  
  
  plot_file <- sprintf("%s/plots/%s.html",
                       export_dir,
                       df_name)
  
  
  
  kwb.utils::catAndRun(sprintf("Export plot: %s", plot_file),
                       expr = {
                         g1 <- df_long %>%
                           ggplot2::ggplot(mapping = ggplot2::aes_string(x = "DateTime",
                                                                  y = "ParameterValue",
                                                                  col = "SiteName")) +
                           ggplot2::facet_wrap( ~ ParameterName, scales = "free_y", ncol = 1) +
                           ggplot2::geom_point() +
                           ggplot2::theme_bw()
                         
                         
                         withr::with_dir(sprintf("%s/plots", export_dir),
                                         code = {
                                           plotly::ggplotly(g1) %>%  
                                          htmlwidgets::saveWidget(basename(plot_file),
                                                                  selfcontained = FALSE,
                                                                  title = df_name)
                                         })
                       },
                       dbg = dbg)
}