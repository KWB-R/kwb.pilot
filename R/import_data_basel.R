#' Imports operational data for Basel (without metadata and only for one site
#' at once, e.g. "rhein" or "wiese")
#' @param xlsx_dir Define directory with raw data in EXCEL spreadsheet (.xlsx) to
#' be imported (default: sema.pilot:::package_file("shiny/basel/data/operation/wiese"))
#' @return returns data frame with imported raw operational data
#' @importFrom  readxl read_excel
#' @importFrom  tidyr gather_
#' @export

import_operation_basel <- function(
  xlsx_dir = package_file("shiny/basel/data/operation/wiese")
) 
{
  xlsx_files <- list_full_xls_files()

  for (xlsx_file in xlsx_files) {
    print(sprintf("Importing: %s", xlsx_file))
    tmp <- readxl::read_excel(path = xlsx_file)

    if (xlsx_file == xlsx_files[1]) {
      raw_data <- tmp
    } else {
      raw_data <- rbind(raw_data, tmp)
    }
  }

  names(raw_data)[1] <- "DateTime"

  print(sprintf("Setting time zone to 'CET'"))
  raw_data <- set_timezone(raw_data, tz = "CET")

  raw_data_tidy <- tidyr::gather_(
    data = raw_data,
    key_col = "Parameter_Site_Unit",
    value_col = "ParameterValue",
    gather_cols = setdiff(names(raw_data), "DateTime")
  )


  raw_data_tidy$Source <- "online"
  raw_data_tidy$DataType <- "raw"


  return(raw_data_tidy)
}


#' Imports analytical data for Basel (without metadata)
#' @param csv_dir Define directory with raw analytical data in CSV (.csv) format to
#' be imported (default: sema.pilot:::package_file("shiny/basel/data/analytics"))
#' @return returns data frame with imported raw analytics data
#' @importFrom janitor clean_names
#' @importFrom  readxl read_excel
#' @importFrom  utils read.csv2
#' @import dplyr
#' @export


import_analytics_basel <- function(
  csv_dir = package_file("shiny/basel/data/analytics")
)
{
  csv_files <- list_full_csv_files(csv_dir)

  for (csv_file in csv_files) {
    print(sprintf("Importing: %s", csv_file))
    tmp <- read.csv2(
      file = csv_file,
      na.strings = "",
      stringsAsFactors = FALSE
    ) %>%
      janitor::clean_names()


    ### Correct manually "prufpunkt_bezeichnung" for all "prufpunkt >= 94000" in
    ### case these are different from the "prufpunkt_bezeichnung" compared to
    ### "prufpunkt < 94000"


    rep_strings <- list("Metolachlor OA" = "Metolachlor-OXA",
                        "Metolachlor ESA" = "Metolachlor-ESA",
                        "N-Acetyl-4-aminoantipyri" = "N-Acetyl-4-Aminoantipyri",
                        "\\<Cyprosulfamid\\>" = "Cyprosulfamide")

    tmp$prufpunkt_bezeichnung_cor <-
      kwb.utils::multiSubstitute(strings = tmp$prufpunkt_bezeichnung,
                      replacements = rep_strings)



    correction_df <- tmp %>%
      dplyr::group_by_("prufpunkt_bezeichnung",
                       "prufpunkt") %>%
      dplyr::summarise(total.count = n()) %>%
      dplyr::select_("prufpunkt_bezeichnung",
                     "prufpunkt") %>%
      dplyr::filter_("prufpunkt < 94000 | prufpunkt %in% c(94004,94006)") %>%
      dplyr::rename_("prufpunkt_bezeichnung_cor" = "prufpunkt_bezeichnung",
                     "prufpunkt_cor" = "prufpunkt")

    tmp <- tmp %>%
      dplyr::left_join(correction_df) %>%
      dplyr::mutate(DateTime = as.POSIXct(strptime(
        x = paste(
          tmp$datum,
          tmp$uhrzeit
        ),
        format = "%d.%m.%Y %H:%M"
      ))) %>%
      dplyr::rename(
        SiteCode = "probestelle",
        ParameterCode_Org = "prufpunkt",
        ParameterCode = "prufpunkt_cor",
        ParameterName_Org = "prufpunkt_bezeichnung",
        ParameterName_Cor = "prufpunkt_bezeichnung_cor",
        ParameterOperator = "operator",
        ParameterValue = "messwert",
        ParameterUnit_Org = "einheit",
        Method_Org = "methode",
        MethodName_Org = "methoden_bezeichnung"
      ) %>%
      dplyr::select(
        "DateTime",
        "SiteCode",
        "ParameterCode_Org",
        "ParameterCode",
        "ParameterName_Org",
        "ParameterName_Cor",
        "ParameterOperator",
        "ParameterValue",
        "ParameterUnit_Org",
        "Method_Org",
        "MethodName_Org"
      )



    if (csv_file == csv_files[1]) {
      raw_data <- tmp
    } else {
      raw_data <- rbind(raw_data, tmp)
    }
  }

  print(sprintf("Setting time zone to 'CET'"))
  raw_data <- set_timezone(raw_data, tz = "CET")

  raw_data$ParameterValue <- as.numeric(raw_data$ParameterValue)
  raw_data$Source <- "offline"
  raw_data$DataType <- "raw"

  return(raw_data)
}


#' Helper function: add site metadata
#' @param df data frame containing at least a column "SiteCode"
#' @param df_col_sitecode column in df containing site code (default: "SiteCode")
#' @param meta_site_path Define path of "meta_site.csv" to be imported
#' (default: sema.pilot:::package_file("shiny/basel/data/metadata/meta_site.csv"))
#' @return returns input data frame with joined metadata
#' @importFrom  tidyr separate_
#' @export

add_site_metadata <- function(
  df, 
  df_col_sitecode = "SiteCode", 
  meta_site_path = package_file("shiny/basel/data/metadata/meta_site.csv")
)
{
  meta_site <- read.csv(
    file = meta_site_path,
    stringsAsFactors = FALSE,
    na.strings = ""
  )

  res <- df %>%
    tidyr::separate_(
      col = df_col_sitecode,
      sep = "-",
      into = paste0("SiteName", 1:3),
      remove = FALSE
    )


  for (siteID in 1:3) {
    print(sprintf(
      "Replacing SiteCode%d with SiteName%d",
      siteID,
      siteID
    ))
    col_sitename <- paste0("SiteName", siteID)
    sites <- meta_site[meta_site$SiteID == siteID, ]

    if (nrow(sites) > 0) {
      for (site_idx in 1:nrow(sites)) {
        sel_site <- sites[site_idx, ]
        strings_to_replace <- !is.na(res[, col_sitename]) & res[, col_sitename] == sel_site$SiteLocation
        if (sum(strings_to_replace) > 0) {
          res[strings_to_replace, col_sitename] <- sel_site$SiteLocationName
        }
      }
    }
    res[is.na(res[, col_sitename]), col_sitename] <- ""
  }

  res$SiteName <- sprintf(
    "%s (%s %s)",
    res$SiteName1,
    res$SiteName2,
    res$SiteName3
  )

  return(res)
}


#' Helper function: add parameter metadata
#' @param df data frame containing at least a column "ParameterCode"
#' @param meta_parameter_path Define path of "meta_parameter.csv" to be imported
#' (default: sema.pilot:::package_file("shiny/basel/data/metadata/meta_parameter.csv"))
#' @return returns input data frame with joined metadata (parameter codes/ methods
#' not included in meta_parameter file will not be imported!!!!)
#' @importFrom  dplyr left_join
#' @export
add_parameter_metadata <- function(
  df,
  meta_parameter_path = package_file("shiny/basel/data/metadata/meta_parameter.csv")
)
{
  meta_parameter <- read.csv(
    file = meta_parameter_path,
    stringsAsFactors = FALSE,
    na.strings = ""
  )


  res <- df %>%
    dplyr::inner_join(meta_parameter)

  return(res)
}

#' Helper function: add label ("SiteName_ParaName_Unit_Method")
#' @param df data frame containing at least a columns "SiteName", "ParameterName",
#' "ParameterUnit" and optionally "Method_Org" (if not existent no "Method_Org" will be
#' available!)
#' @param col_sitename column in df containing site name (default: "SiteName")
#' @param col_parametername column in df containing parameter name (default: "ParameterName")
#' @param col_parameterunit column in df containing parameter unit (default: "ParameterUnit")
#' @param col_method column in df containing method code (default: "Method_Org")
#' @return returns input data frame with added column "SiteName_ParaName_Unit_Method"
#' @export

add_label <- function(df,
                      col_sitename = "SiteName",
                      col_parametername = "ParameterName",
                      col_parameterunit = "ParameterUnit",
                      col_method = "Method_Org") {


  col_method_exists <- col_method %in% names(df)

  if(col_method_exists) {
  boolean_no_method <- is.na(df[,col_method]) | df[,col_method] == ""
  } else {
  boolean_no_method <- rep(TRUE, times = nrow(df))
  }

  df$SiteName_ParaName_Unit_Method <- ""


  if(any(boolean_no_method)) {
    ind <- which(boolean_no_method)
    df$SiteName_ParaName_Unit_Method[ind] <- sprintf(
      "%s: %s (%s)",
      df[ind , col_sitename],
      df[ind , col_parametername],
      df[ind , col_parameterunit]) }

  if(any(!boolean_no_method)) {
    ind <- which(!boolean_no_method)
        df$SiteName_ParaName_Unit_Method[!boolean_no_method] <- sprintf(
      "%s: %s (%s, Method: %s)",
      df[ind , col_sitename],
      df[ind , col_parametername],
      df[ind , col_parameterunit],
      df[ind , col_method]
    )
  }
  return(df)
}


#' Imports operational data for Basel (with metadata for
#' both sites at once, i.e. "rhein" and "wiese")
#' @param raw_dir_rhein Define directory for site "rhein" with raw data in
#' EXCEL spreadsheet format (.xlsx) to be imported (default:
#' sema.pilot:::package_file("shiny/basel/data/operation/rhein"))
#' @param raw_dir_wiese Define directory for site "rhein" with raw data in
#' EXCEL spreadsheet format (.xlsx) to be imported (default:
#' sema.pilot:::package_file("shiny/basel/data/operation/wiese"))
#' @param meta_online_path path to file containing metadata for online data
#' (default: sema.pilot:::package_file("shiny/basel/data/metadata/meta_online.csv"))
#' @param meta_site_path Define path of "meta_site.csv" to be imported
#' (default: sema.pilot:::package_file("shiny/basel/data/metadata/meta_site.csv"))
#' @param meta_parameter_path Define path of "meta_parameter.csv" to be imported
#' (default: sema.pilot:::package_file("shiny/basel/data/metadata/meta_parameter.csv"))
#' @return returns data frame with imported raw operational data with metadata
#' for both sites (i.e."rhein" and "wiese")
#' @importFrom  dplyr left_join
#' @return data.frame with operational data for Basel sites including metadata
#' @export
import_operation_meta_basel <- function(
  raw_dir_rhein = package_file("shiny/basel/data/operation/rhein"),
  raw_dir_wiese = package_file("shiny/basel/data/operation/wiese"),
  meta_online_path = package_file("shiny/basel/data/metadata/meta_online.csv"),
  meta_site_path = package_file("shiny/basel/data/metadata/meta_site.csv"),
  meta_parameter_path = package_file("shiny/basel/data/metadata/meta_parameter.csv")
)
{
  meta_online <- read.csv(
    file = meta_online_path,
    stringsAsFactors = FALSE,
    na.strings = ""
  )


  online_meta <- add_site_metadata(
    df = meta_online,
    meta_site_path = meta_site_path
  ) %>%
    add_parameter_metadata(meta_parameter_path = meta_parameter_path) %>%
    add_label()

  ### 1.1) Wiese: Import XLSX data and join with metadata
  print("###################################################################")
  print("######## Importing operational data with metadata for site 'Wiese'")
  print("###################################################################")

  wiese <- import_operation_basel(xlsx_dir = raw_dir_wiese) %>%
    dplyr::left_join(online_meta[grep(
      pattern = "WF",
      online_meta$SiteCode
    ), ])


  ### 1.2) Rhein: Import XLSX data and join with metadata
  print("###################################################################")
  print("######## Importing operational data with metadata for site 'Rhein'")
  print("###################################################################")

  rhein <- import_operation_basel(xlsx_dir = raw_dir_rhein) %>%
    dplyr::left_join(online_meta[grep(
      pattern = "RF",
      online_meta$SiteCode
    ), ])

  basel <- rbind(wiese, rhein)

  return(basel)
}

#' Imports analytical data for Basel (with metadata for both sites at once, i.e.
#' "rhein" and "wiese")
#' @param analytics_dir Define directory with raw analytical data in CSV (.csv) format to
#' be imported (default: sema.pilot:::package_file("shiny/basel/data/analytics"))
#' @param meta_site_path Define path of "meta_site.csv" to be imported
#' (default: sema.pilot:::package_file("shiny/basel/data/metadata/meta_site.csv"))
#' @param meta_parameter_path Define path of "meta_parameter.csv" to be imported
#' (default: sema.pilot:::package_file("shiny/basel/data/metadata/meta_parameter.csv"))
#' @return data.frame with analytics data for Basel sites including metadata
#' @export
import_analytics_meta_basel <- function(
  analytics_dir = package_file("shiny/basel/data/analytics"),
  meta_site_path = package_file("shiny/basel/data/metadata/meta_site.csv"),
  meta_parameter_path = package_file("shiny/basel/data/metadata/meta_parameter.csv")
)
{
  print("###################################################################")
  print("###### Importing analytics data with metadata for sites 'Wiese' and Rhein'")
  print("###################################################################")

  analytics_meta_data <- import_analytics_basel(csv_dir = analytics_dir) %>%
    add_site_metadata(meta_site_path = meta_site_path) %>%
    add_parameter_metadata(meta_parameter_path = meta_parameter_path) %>%
    add_label()

  return(analytics_meta_data)
}

#' Imports operational & analytical data for Basel (with metadata for both sites
#' at once, i.e. "rhein" and "wiese")
#' @param analytics_dir Define directory with raw analytical data in CSV (.csv) format to
#' be imported (default: sema.pilot:::package_file("shiny/basel/data/analytics"))
#' @param raw_dir_rhein Define directory for site "rhein" with raw data in
#' EXCEL spreadsheet format (.xlsx) to be imported (default:
#' sema.pilot:::package_file("shiny/basel/data/operation/rhein"))
#' @param raw_dir_wiese Define directory for site "rhein" with raw data in
#' EXCEL spreadsheet format (.xlsx) to be imported (default:
#' sema.pilot:::package_file("shiny/basel/data/operation/wiese"))
#' @param meta_online_path path to file containing metadata for online data
#' (default: sema.pilot:::package_file("shiny/basel/data/metadata/meta_online.csv"))
#' @param meta_parameter_path Define path of "meta_parameter.csv" to be imported
#' (default: sema.pilot:::package_file("shiny/basel/data/metadata/meta_parameter.csv"))
#' @param meta_site_path Define path of "meta_site.csv" to be imported
#' (default: sema.pilot:::package_file("shiny/basel/data/metadata/meta_site.csv"))
#' @return data.frame with analytical & operational data for Basel
#' @importFrom plyr rbind.fill
#' @export
import_data_basel <- function(
  analytics_dir = package_file("shiny/basel/data/analytics"),
  raw_dir_rhein = package_file("shiny/basel/data/operation/rhein"),
  raw_dir_wiese = package_file("shiny/basel/data/operation/wiese"),
  meta_online_path = package_file("shiny/basel/data/metadata/meta_online.csv"),
  meta_parameter_path = package_file("shiny/basel/data/metadata/meta_parameter.csv"),
  meta_site_path = package_file("shiny/basel/data/metadata/meta_site.csv")
)
{
  operation_meta <- import_operation_meta_basel(
    raw_dir_rhein = raw_dir_rhein,
    raw_dir_wiese = raw_dir_wiese,
    meta_online_path = meta_online_path,
    meta_site_path = meta_site_path,
    meta_parameter_path = meta_parameter_path
  )
  analytics_meta <- import_analytics_meta_basel(
    analytics_dir = analytics_dir,
    meta_site_path = meta_site_path,
    meta_parameter_path = meta_parameter_path
  )


  print("###################################################################")
  print("######## Add analytical to the operational data (including metadata)")
  print("###################################################################")
  data_basel <- plyr::rbind.fill(operation_meta, analytics_meta)

  return(data_basel)
}
