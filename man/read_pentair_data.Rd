% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_operation_berlin_t.R
\name{read_pentair_data}
\alias{read_pentair_data}
\title{Read PENTAIR operational data}
\usage{
read_pentair_data(
  raw_data_dir = package_file("shiny/berlin_t/data/operation"),
  raw_data_files = NULL,
  meta_file_path = package_file("shiny/berlin_t/data/parameter_site_metadata.csv")
)
}
\arguments{
\item{raw_data_dir}{path of directory containing PENTAIR xls files 
(default: kwb.pilot:::package_file("shiny/berlin_t/data/operation"))}

\item{raw_data_files}{vector with full path to operational raw data files that
allows to limit import to specific files (default: NULL). If specified parameter
"raw_data_dir" will not be used}

\item{meta_file_path}{path to metadata file (default:
kwb.pilot:::package_file("shiny/berlin_t/data/parameter_site_metadata.csv"))}
}
\value{
data.frame with imported PENTAIR operational data
}
\description{
Read PENTAIR operational data
}
