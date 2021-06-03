#' Calculate operational parameters
#' @param df a data frame as retrieved by import_data_haridwar()
#' @param calc_list list with calculation operations to be carried out
#' (default: list(Redox_Out = "(Redox_Out1+Redox_Out2)/2",
#' Redox_Diff = "Redox_Out - Redox_In", Power_pump = "Up*Ip",
#' Power_cell = "Uz*Iz", Pump_WhPerCbm = "Power_pump/Flux/1000",
#' Cell_WhPerCbm = "Power_cell/Flux/1000"))
#' @param calc_list_name full names of parameters to be used for plotting for each
#' calculation specified wit 'calc_list'. default: c('Tank water: Mean redox potential",
#' 'Difference (outflow - inflow) of redox potential',
#' 'Power demand of pump', 'Power demand of cell',
#' 'Specific energy demand of pump', Specific energy demand of cell')
#' @param calc_list_unit units of parameters to be used for plotting for each
#' calculation specified wit 'calc_list'. default: c('mV', 'mV', 'Wh', 'Wh',
#' 'Wh/m3', 'Wh/m3')
#' @param calc_paras a vector with parameter codes used for performing calculations
#' defined in 'calc_list' (default: c('Redox_Out1', 'Redox_Out2', 'Redox_In',
#' 'Flux', 'Up', 'Ip', 'Uz', 'Iz'))
#' @return dataframe with calculated operational parameters
#' @importFrom kwb.utils stringList
#' @export
#' @examples
#' \dontrun{
#' haridwar_raw_list <- import_data_haridwar()
#' myDat <- calculate_operational_parameters(df = haridwar_raw_list)
#' }
#'
calculate_operational_parameters <- function(df,
                                             calc_list = list(
                                               Redox_Out = "(Redox_Out1+Redox_Out2)/2",
                                               Redox_Diff = "Redox_Out - Redox_In",
                                               Power_pump = "Up*Ip",
                                               Power_cell = "Uz*Iz",
                                               Pump_WhPerCbm = "Power_pump/(Flux/1000)",
                                               Cell_WhPerCbm = "Power_cell/(Flux/1000)"
                                             ),
                                             calc_list_name = c(
                                               "Mean redox potential in tank",
                                               "Difference (outflow - inflow) of redox potential",
                                               "Power demand of pump",
                                               "Power demand of cell",
                                               "Specific energy demand of pump",
                                               "Specific energy demand of cell"
                                             ),
                                             calc_list_unit = c(
                                               "mV",
                                               "mV",
                                               "W",
                                               "W",
                                               "Wh/m3",
                                               "Wh/m3"
                                             ),
                                             calc_paras = c(
                                               "Redox_Out1",
                                               "Redox_Out2",
                                               "Redox_In",
                                               "Flux",
                                               "Up",
                                               "Ip",
                                               "Uz",
                                               "Iz"
                                             )) {
  print(sprintf(
    "Calculating %d operational parameter(s): %s",
    length(calc_list_name),
    kwb.utils::stringList(calc_list_name)
  ))

  meta_data <- data.frame(
    ParameterCode = names(calc_list),
    ParameterName = calc_list_name,
    ParameterUnit = calc_list_unit,
    ParameterLabel = sprintf("%s (%s)", calc_list_name, calc_list_unit),
    stringsAsFactors = FALSE
  )



  operation <- df[df$ParameterCode %in% calc_paras, ] %>%
    filter_("!is.na(ParameterValue)") %>%
    select_("DateTime", "ParameterCode", "ParameterValue")

  operation_matrix <- operation %>%
    tidyr::spread_(
      key_col = "ParameterCode",
      value_col = "ParameterValue"
    )


  ### Calculate additional parameters:
  operation_calc <- operation_matrix %>%
    dplyr::mutate_(.dots = calc_list) %>%
    dplyr::select_(.dots = c("DateTime", names(calc_list)))

  operation_calc_tidy <- tidyr::gather_(
    operation_calc,
    key_col = "ParameterCode",
    value_col = "ParameterValue",
    gather_cols = dplyr::setdiff(names(operation_calc), "DateTime")
  ) %>%
    dplyr::filter_("!is.na(ParameterValue)") %>%
    dplyr::left_join(y = meta_data)
  # dplyr::mutate_(DataType = "'calculated'")


  return(operation_calc_tidy)
}


#' Plot calculate operational time series
#' @param df a data frame as retrieved by calculate_operational_parameters()
#' @return plots time series for calculated operational parameters
#' @export
#' @examples
#' \dontrun{
#' haridwar_raw_list <- import_data_haridwar()
#' myDat <- calculate_operational_parameters(df = haridwar_raw_list)
#' plot_calculated_operational_timeseries(myDat)
#' }
#'
plot_calculated_operational_timeseries <- function(df) {
  calculated_paras <- unique(df$ParameterLabel)


  for (i in seq_along(calculated_paras)) {
    sel_par1 <- df$ParameterLabel[order(calculated_paras)][i]

    n_measurements <- nrow(df[df[, "ParameterLabel"] == sel_par1, ])

    if (n_measurements > 0) {
      g1 <- ggplot2::ggplot(df, ggplot2::aes_string(
        x = "DateTime",
        y = "ParameterValue"
      )) +
        ggforce::facet_wrap_paginate(
          ~ParameterLabel,
          nrow = 1,
          ncol = 1,
          scales = "free_y",
          page = i
        ) +
        ggplot2::geom_point() +
        ggplot2::theme_bw(base_size = 20) +
        ggplot2::theme(
          legend.position = "top",
          strip.text.x = element_text(face = "bold"),
          legend.title = element_blank()
        ) +
        ggplot2::labs(x = "", y = "")

      print(g1)
    }
  }
}


if (FALSE) {
  myDat <- calculate_operational_parameters(df = haridwar_raw_list)


  plot_calculated_operational_timeseries(df = myDat)

  ### Plot it

  #
  # backwash <- operation[operation$Anlauf == 90,"DateTime"]
  #
  # p4 <- ggplot(data = operation_grouped %>% filter(DiffPressure < 10), aes(x = DateTime, y = DiffPressure)) +
  #   geom_point() +
  #   geom_vline(xintercept = as.numeric(backwash), col = "red") +
  #   labs(list(x = "Datetime (UTC)",
  #             y = "Pressure difference (out - in)")) +
  #   theme_bw() +
  #   theme(legend.position = "top")
  # print(p4)
  #
  #
  # energy_tidy <- operation %>%  select(DateTime, Pump_WhPerCbm, Cell_WhPerCbm) %>% gather(key = "Key", value = "Value",-DateTime)
  # energy_tidy <- tidyr::separate(energy_tidy,
  #                                col = "Key",
  #                                into = c("System component", "Unit"),
  #                                sep = "_")
  #
  # energy_title <- sprintf("Based on 15 minute median values of online data\n(period: %s to %s)",
  #                         min(energy_tidy$DateTime),
  #                         max(energy_tidy$DateTime))
  #
  # p5 <- ggplot(energy_tidy , aes_string(x = "DateTime",
  #                                       y = "Value",
  #                                       col = "`System component`")) +
  #   geom_point() +
  #   geom_vline(xintercept = as.numeric(backwash), col = "red") +
  #   #facet_wrap(~ `System component`) +
  #   labs(list(x = "Datetime (UTC)",
  #             y = "Specific energy demand (Wh/m3)",
  #             title = energy_title)) +
  #   theme_bw() +
  #   theme(legend.position = "top")
  # print(p5)
  #
  # p6 <- ggplot(energy_tidy , aes_string(x = "`System component`",
  #                                       y = "Value",
  #                                       col = "`System component`")) +
  #   geom_jitter(height = 0, width = 0.3, alpha = 0.5) +
  #   #facet_wrap(~ `System component`) +
  #   labs(list(x = "System component",
  #             y = "Specific energy demand (Wh/m3)",
  #             title = energy_title)) +
  #   theme_bw() +
  #   theme(legend.position = "top")
  # print(p6)
}
