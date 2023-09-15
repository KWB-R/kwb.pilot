#' Helper function: transform "long" to "wide"
#' @param df data frame in long format (as retrieved by \code{kwb.pilot::group_datetime})
#' @importFrom dplyr select mutate
#' @importFrom tidyr spread
#' @export
#'
long_to_wide <- function(df)
{
  df %>%
    dplyr::mutate(
      ParameterName_SiteName = sprintf(
        "%s_%s", 
        .data$ParameterName,
        .data$SiteName
      )
    ) %>%
    dplyr::select(
      .data$DateTime,
      .data$ParameterName_SiteName,
      .data$ParameterValue
    ) %>%
    tidyr::spread(
      key = .data$ParameterName_SiteName,
      value = .data$ParameterValue
    )
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
export_data <- function(df_long, export_dir, dbg = TRUE)
{
  fs::dir_create(sprintf("%s/data", export_dir))
  
  name <- deparse(substitute(df_long))
  
  file <- sprintf("%s/data/%s.csv", export_dir, name)
  
  kwb.utils::catAndRun(
    sprintf("Export data to %s", file),
    dbg = dbg,
    expr = {
      df_wide <- long_to_wide(df_long)
      readr::write_csv2(df_wide, path = file)
    }
  )
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
plot_data <- function(df_long, export_dir, dbg = TRUE)
{
  fs::dir_create(sprintf("%s/plots", export_dir))
  
  name <- deparse(substitute(df_long))
  
  file <- sprintf("%s/plots/%s.html", export_dir, name)
  
  kwb.utils::catAndRun(
    sprintf("Export plot: %s", file),
    dbg = dbg,
    expr = {
      g1 <- df_long %>%
        ggplot2::ggplot(mapping = ggplot2::aes_string(
          x = "DateTime",
          y = "ParameterValue",
          col = "SiteName"
        )) +
        ggplot2::facet_wrap(~ParameterName, scales = "free_y", ncol = 1) +
        ggplot2::geom_point() +
        ggplot2::theme_bw()
      
      withr::with_dir(sprintf("%s/plots", export_dir), code = {
        plotly::ggplotly(g1) %>%
          htmlwidgets::saveWidget(
            basename(file), 
            selfcontained = FALSE, 
            title = name
          )
      })
    }
  )
}
