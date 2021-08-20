## code to prepare `mbr4.0` dataset goes here
library(readxl)
library(dplyr)
metadata_mbr4_wide <- readxl::read_excel("inst/extdata/MBR4.0_Kopfzeile.xlsx")

metadata_mbr4_wide %>%
  dplyr::rename(key = "Abkürzung") %>%
  tidyr::pivot_longer(
    cols = !tidyselect::matches("key"),
    names_to = "ParameterCode_SiteCode"
  ) %>%
  tidyr::separate("ParameterCode_SiteCode",
    into = c("ParameterCode", "SiteCode"),
    sep = "_", extra = "merge", remove = FALSE
  ) %>%
  tidyr::pivot_wider(names_from = "key", values_from = value) %>%
  tidyr::separate("Beschreibung",
    into = c("ParamaterName", "SiteName"),
    sep = "\\s", extra = "merge", remove = FALSE
  ) %>%
  dplyr::rename(
    "ParameterName_SiteName" = Beschreibung,
    "Unit" = Einheit,
    "Comment" = Bemerkung
  ) %>%
  dplyr::mutate(
    Source = "online",
    DataType = dplyr::if_else(!is.na(Comment) & Comment == "SUMME",
      "calculated", "raw"
    )
  ) %>%
  ## first script based on XLSX (not needed anymore after "cleaning by Jette)
  write.csv2("inst/extdata/metadata_mbr4_untidy.csv",
    row.names = FALSE,
    na = ""
  )

mbr4.0_metadata <- readr::read_csv2("inst/extdata/metadata_mbr4.csv")
readr::write_csv(mbr4.0_metadata, "inst/shiny/mbr4.0/data/metadata.csv")

selected_cols <- c("ParameterCode",
                   "ParameterName",
                   "ParameterUnit",
                   "SiteCode",
                   "SiteName")

ignore_paras <- c("Zustand", "Meldungen", "Laufende Nr.", "Zeitstempel")

mbr4.0_metadata[,selected_cols] %>% 
  dplyr::filter(!ParameterName %in% ignore_paras) %>% 
  dplyr::mutate(ParameterThresholdComparison = NA_character_,
                ParameterThreshold = NA_character_,
                ParameterThresholdSource = NA_character_) %>% 
  readr::write_csv2("inst/shiny/mbr4.0/data/thresholds.csv")  

