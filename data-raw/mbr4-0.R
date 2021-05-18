## code to prepare `mbr4.0` dataset goes here
library(readxl)
library(dplyr)
metadata_mbr4_wide <- readxl::read_excel("inst/extdata/MBR4.0_Kopfzeile.xlsx")

metadata_mbr4_wide %>%
  dplyr::rename(key = "Abkürzung") %>% 
  tidyr::pivot_longer(cols = !tidyselect::matches("key"), 
                      names_to = "ParameterCode_SiteCode") %>% 
  tidyr::separate("ParameterCode_SiteCode", 
                  into = c("ParameterCode", "SiteCode"), 
                  sep = "_", extra = "merge", remove = FALSE) %>%   
  tidyr::pivot_wider(names_from = "key", values_from = value) %>% 
  tidyr::separate("Beschreibung", 
                  into = c("ParamaterName", "SiteName"), 
                  sep = "\\s", extra = "merge", remove = FALSE) %>% 
  dplyr::rename("ParameterName_SiteName" = Beschreibung,
                "Unit" = Einheit, 
                "Comment" = Bemerkung) %>% 
  dplyr::mutate(Source = "online", 
                DataType = dplyr::if_else(!is.na(Comment) & Comment == "SUMME", 
                                          "calculated", "raw")) %>% 
  write.csv2("inst/extdata/metadata_mbr4.csv", 
             row.names = FALSE, 
             na = "")

metadata_tidy <- readr::read_csv2("inst/extdata/metadata_mbr4.csv"")
usethis::use_data(mbr4.0_metadata, overwrite = TRUE)
