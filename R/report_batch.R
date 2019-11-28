#' Report batch: creates batch file for report
#' @param rmd_name name of Rmarkdown document to render (default: "report.Rmd")
#' @param output_dir outputDirectory (default: getwd())
#' @keywords internal
write_report <- function(rmd_name = "report.Rmd",
                         output_dir = getwd()) {
  code <- 'if (!require("rmarkdown")) {
    install.packages("rmarkdown",repos = "https://cloud.r-project.org")
  }
  if (packageVersion("rmarkdown") < "1.3") {
    install.packages("rmarkdown", repos = "https://cloud.r-project.org")
  }
  print("Generating reports (HTML, PDF & WORD")
  rmarkdown::render(
    input = "input/%s",
    output_format = paste0(c("html", "pdf", "word"), "_document"),
    output_dir = "output"
  )'

  outputPath <- file.path(output_dir, "create_report.R")
  writeLines(sprintf(code, rmd_name), con = outputPath)
}



#' Report batch: creates batch file for report
#' @param batchDir path to report batch directory (default: tempdir())
#' @param batchName name for report batch file(default: "create_report.bat")
#' @param report_path (default: NULL)
#' @param report_config_path (default: NULL)
#' @param open_in_explorer open batchDir in Windows explorer (default: TRUE).
#' Only working on a Windows system!
#' @export
create_report_batch <- function(batchDir = file.path(tempdir(), "batch_report"),
                                batchName = "create_report.bat",
                                report_path = NULL,
                                report_config_path = NULL,
                                open_in_explorer = TRUE) {
  batchDir <- gsub(pattern = "\\\\", replacement = .Platform$file.sep, batchDir)


  if (dir.exists(batchDir) == FALSE) {
    dir.create(batchDir, showWarnings = FALSE)
  }

  owdir <- setwd(batchDir)
  on.exit(owdir)


  if (is.null(report_path)) {
    report_path <- system.file(
      "shiny/haridwar/report/report.Rmd",
      package = "kwb.pilot"
    )
  }

  report_name <- basename(report_path)

  dir.create(
    path = file.path(batchDir, "input"),
    showWarnings = FALSE
  )


  file.copy(
    from = report_path,
    to = file.path(batchDir, "input", report_name),
    overwrite = TRUE
  )

  write_report(
    rmd_name = report_name,
    output_dir = batchDir
  )

  if (!is.null(report_config_path)) {
    if (file.exists(report_config_path)) {
      file.copy(
        from = report_config_path,
        to = file.path(batchDir, "input", "report_config.txt"),
        overwrite = TRUE
      )
    }
  }




  batchName_noExt <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(batchName))
  batch_r_script <- paste0(batchName_noExt, ".R")



  bat_1 <- "set mypath=%cd%\n"
  bat_2 <- "@echo \"Generating reports (HTML, PDF & WORD) and  saving to '\\output' folder .....\"\n"


  r_path <- R.home("bin")

  r_script <- list.files(r_path)[grep(pattern = "Rscript", list.files(r_path))]
  r_exe <- normalizePath(file.path(r_path, r_script))
  bat_3 <- sprintf("\"%s\" \"%s\"\n", r_exe, batch_r_script)

  bat_4 <- "@echo \"Report generation finished. Please check subfolder '\\output'!\"\n"

  bat_5 <- "Pause"

  batch_text <- sprintf(
    "%s%s%s%s%s",
    bat_1,
    bat_2,
    bat_3,
    bat_4,
    bat_5
  )

  batch_path <- file.path(batchDir, batchName)

  writeLines(batch_text, con = batch_path)
  print(paste("Batch file & data structure created at:", normalizePath(batch_path)))
  if (open_in_explorer & .Platform$OS.type == "windows") {
    shell(paste("explorer", normalizePath(batchDir)))
  }

  created_files <- list.files(getwd(), recursive = TRUE)
  return(created_files)
}
