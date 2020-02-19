#' Runs Shiny app for an AQUANES site
#' @param siteName site name for shiny app (default: "haridwar")
#' @param use_live_data should live data be used (default: FALSE)
#' @param mySQL_conf file path to mySQL config file (.my.cnf). Only used if
#' parameter use_live_data is TRUE and there is no .my.cnf in the app folder for
#' the selected site (default: NULL)
#' @param launch.browser If true, the system's default web browser will be
#' launched automatically after the app is started (default: TRUE)
#' @param ... further arguments passed to shiny::runApp()
#' @importFrom shiny runApp
#' @importFrom kwb.utils stringList
#' @export
run_app <- function(
  siteName = "haridwar", use_live_data = FALSE, mySQL_conf = NULL, 
  launch.browser = TRUE, ...
)
{
  use_live_data <- toupper(use_live_data)

  shinyDir <- package_file("shiny")
  appDir <- file.path(shinyDir, siteName)

  if (!siteName %in% dir(shinyDir)) {
    msg <- sprintf(
      "Could not find shiny app directory for %s.\n
                    Please select for parameter 'siteName' one of:\n'%s'",
      siteName,
      kwb.utils::stringList(dir(shinyDir))
    )

    clean_stop(msg)
  }

  if (siteName == "haridwar") {
    mySQL_conf_path <- file.path(appDir, ".my.cnf")

    if (use_live_data) {
      if (!is.null(mySQL_conf)) {
        file.copy(
          from = mySQL_conf,
          to = mySQL_conf_path
        )
      }

      if (!file.exists(mySQL_conf_path)) {
        msg <- sprintf("No '.my.cnf' file located under: %s\n.
           Please once specify the path to a valid MySQL config file with parameter
              'mySQL_conf'", appDir)
        clean_stop(msg)
      }
    }
  }

  global_path <- file.path(appDir, "global.R")


  if (file.exists(global_path) == FALSE) {
    clean_stop("Could not find a 'global.R' in: ", appDir)
  }


  ### adapt "global.R" to use live data or not
  global_string <- readLines(global_path)
  replace_line <- grep(pattern = "use_live_data\\s*<-", global_string)
  global_string[replace_line] <- sprintf("use_live_data <- %s", use_live_data)
  
  writeLines(global_path, text = global_string)

  shiny::runApp(
    appDir, display.mode = "normal", launch.browser = launch.browser, ...
  )
}
