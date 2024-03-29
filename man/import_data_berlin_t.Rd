% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_operation_berlin_t.R
\name{import_data_berlin_t}
\alias{import_data_berlin_t}
\title{Import data for Berlin Tiefwerder}
\usage{
import_data_berlin_t(
  raw_data_dir = package_file("shiny/berlin_t/data/operation"),
  raw_data_files = NULL,
  analytics_path = package_file("shiny/berlin_t/data/analytics.xlsx"),
  meta_file_path = package_file("shiny/berlin_t/data/parameter_site_metadata.csv")
)
}
\arguments{
\item{raw_data_dir}{path of directory containing PENTAIR xls files
(default: kwb.pilot:::package_file("shiny/berlin_t/data/operation"))}

\item{raw_data_files}{vector with full path to operational raw data files that
allows to limit import to specific files (default: NULL). If specified parameter
"raw_data_dir" will not be used}

\item{analytics_path}{full path to lab data EXCEL file in xlsx format
(default: kwb.pilot:::package_file("shiny/berlin_t/data/analytics.xlsx"))}

\item{meta_file_path}{path to metadata file (default:
kwb.pilot:::package_file("shiny/berlin_t/data/parameter_site_metadata.csv"))}
}
\value{
data.frame with imported operational data (analytics´data to be added as
soon as available)
}
\description{
Import data for Berlin Tiefwerder
}
