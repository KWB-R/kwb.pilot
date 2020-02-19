
ui_report <- function(...) {
  fluidPage(
    titlePanel("Parameterise report"),

    sidebarLayout(
      sidebarPanel(
        selectInput("report_aggregation",
                    label = "Select temporal aggregation",
                    choices = c("raw", "10min", "hour", "day"),
                    selected = "10min"),
        selectInput("report_timezone",
                    label = "Select a timezone",
                    choices = c("CET", "UTC"),#kwb.pilot::get_valid_timezones()$TZ.,
                    selected = "CET"),
        selectInput("report_period",
                    label = "Select reporting period",
                    choices = c("monthly", "user defined"),
                    multiple = FALSE,
                    selected = "monthly"),
        conditionalPanel(condition = "input.report_period == 'monthly'",
                         selectInput("report_period_monthly",
                                     label = "Select a month for reporting",
                                     choices = report_months$label,
                                     multiple = FALSE,
                                     selected =  report_months$label[report_months$start == "2017-06-01"])),
        conditionalPanel(condition = "input.report_period != 'monthly'",
                         dateRangeInput('report_period_userdefined',
                                        label = 'Date range input: yyyy-mm-dd',
                                        start = "2017-06-01",
                                        end = "2017-06-30")),
        selectInput("report_sitenames", label = "Select sampling points",
                    choices = unique(siteData_10min_list$SiteName),
                    multiple = TRUE,
                    selected = unique(siteData_10min_list$SiteName)),
        h3("Select parameters"),
        selectInput("report_parameters_online", label = "Online",
                    choices = unique(siteData_10min_list$ParameterName[siteData_10min_list$Source == "online"]),
                    multiple = TRUE),
        selectInput("report_parameters_offline", label = "Offline",
                    choices = unique(siteData_10min_list$ParameterName[siteData_10min_list$Source == "offline"]),
                    multiple = TRUE),
        selectInput("report_parameters_calculated", label = "Calculated",
                    choices = report_calc_paras,
                    multiple = TRUE),
        checkboxInput('report_add_thresholds', "Add thresholds to offline/online parameters", value = FALSE),
        radioButtons("report_format", "Report format", c("HTML", "PDF", "Word"),
                      inline = TRUE),
        downloadButton("report_download", "Generate & download report"),
        downloadButton("report_zip", "Download standalone report (zip)")),
      mainPanel(
        actionButton("report_preview_update", "Update report preview"),
        #h1("Report preview"),
        #downloadButton("report_zip", "Download report zip"),
        htmlOutput("report_preview")
      )
      )
    )
  }

server_report <- function(...) {


   report_daterange <- reactive({

    if (input$report_period == "monthly") {
     sel_month <- report_months[report_months$label == input$report_period_monthly,]
     res <- c(sel_month$start,sel_month$end)
    }
     else {
     res <- input$report_period_userdefined
     }
     return(res)
  })


  report_agg <- reactive({

    withProgress(message = sprintf("1. Loading %s data", input$report_aggregation),
                 value = 0, {

    object_name <- sprintf("siteData_%s_list", input$report_aggregation)

    if (!exists(object_name)) {
      dat <- kwb.pilot::read_fst(path = sprintf("data/%s.fst", object_name))
      assign(x = object_name,
             value = dat)

    }
    incProgress(amount = 1,message = "Completed!")

    })
   get(object_name)
  })


  report_tz <- reactive({

    # withProgress(message = sprintf("Changing time zone to %s",
    #                                 input$report_timezone),
    #              value = 0.3)
    #
    kwb.pilot::change_timezone(df = report_agg(),
                                    tz = input$report_timezone)

  })


  report_tz_daterange <- reactive({

    date_idx <- as.Date(report_tz()[,"DateTime"]) >= report_daterange()[1] & as.Date(report_tz()[,"DateTime"]) <= report_daterange()[2]
    report_tz()[date_idx,]
  })

  report_data <- reactive({


    site_idx <- report_tz_daterange()[,"SiteName"] %in% input$report_sitenames
    para_idx <- report_tz_daterange()[,"ParameterName"] %in%  c(input$report_parameters_online, input$report_parameters_offline)
    row_idx <- site_idx & para_idx
    report_tz_daterange()[row_idx, c("DateTime",
                           "SiteName",
                           "ParameterName",
                           "ParameterUnit",
                           "ParameterValue",
                           "DataType")]
  })



  output$report_zip <- downloadHandler(
    filename = function() {
      datetime <- format(Sys.time(), format = "%Y%m%d%H%M%S")
      paste("report_", datetime, ".zip", sep = "")
    },

    content = function(zfile) {
      tdir <- tempdir()


      #conf_list <- kwb.pilot::report_config_template()
      agg_para <- switch(input$report_aggregation,
						"raw" = "raw",
						"10min" = 600,
						"hour" = 3600,
						"day" = "day",
						"month" = "month")



      # Set up config parameters & save in text file
      conf_list <- list(report_sitenames = input$report_sitenames,
                        report_aggregation = agg_para,
                        report_parameters_online = input$report_parameters_online,
                        report_parameters_offline = input$report_parameters_offline,
                        report_parameters_calculated = input$report_parameters_calculated,
                        report_add_thresholds = input$report_add_thresholds,
                        report_daterange = report_daterange(),
                        report_timezone = input$report_timezone)

      conf_name <- "report_config.txt"
      conf_file <- file.path(tdir, conf_name)

      kwb.pilot::report_config_to_txt(config_list = conf_list,
                                           output_file = conf_file)


      batchDir <- file.path(tdir, "batch")
      reportPath <- file.path(getwd(), "report/report.Rmd")
      files_to_zip <- create_report_batch(batchDir = batchDir,
                                          report_path = reportPath,
                                          report_config_path = conf_file,
                                          open_in_explorer = FALSE)

      dir.old <- setwd(batchDir)
      on.exit(dir.old)


      #cat(tdir, file = stderr())
      zip(zipfile = zfile,
          files = files_to_zip)

    },
    contentType = "application/zip")





  create_report <- eventReactive(eventExpr = input$report_preview_update,
                                 valueExpr = {
    tdir <- tempdir()
    tempReport <- file.path(tdir, "report.Rmd")
    file.copy(from = "report/report.Rmd",
              to = tempReport,
              overwrite = TRUE)

    # Set up parameters to pass to Rmd document
    params <- list(run_as_standalone = FALSE,
                   report_tz = report_tz_daterange(),
                   report_data = report_data(),
                   report_aggregation = input$report_aggregation,
                   report_sitenames = input$report_sitenames,
                   report_parameters_online = input$report_parameters_online,
                   report_parameters_offline = input$report_parameters_offline,
                   report_parameters_calculated = input$report_parameters_calculated,
                   report_add_thresholds = input$report_add_thresholds,
                   report_daterange = report_daterange(),
                   report_timezone = input$report_timezone)

    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    ofile <- file.path(tdir, "automated_report.html")
    rmarkdown::render(tempReport,
                      output_file = ofile,
                      params = params,
                      envir = new.env(parent = globalenv()))
    #includeHTML(ofile)
    ofile
  })


  output$report_preview <- renderUI(
    includeHTML(create_report())
  )

  # output$report_download <- downloadHandler(filename = "automated_report.html",
  #                                           content = function(file) {
  #                                             file.copy(from = create_report(),
  #                                                       to =  file)
  #                                             })


  output$report_download <- downloadHandler(
    filename = function() {
      paste('automated_report', sep = '.', switch(
        input$report_format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },

    content = function(file) {

      tdir <- tempdir()
      tempReport <- file.path(tdir, "report.Rmd")
      file.copy(from = "report/report.Rmd",
                to = tempReport,
                overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(run_as_standalone = FALSE,
                     report_tz = report_tz_daterange(),
                     report_data = report_data(),
                     report_aggregation = input$report_aggregation,
                     report_sitenames = input$report_sitenames,
                     report_parameters_online = input$report_parameters_online,
                     report_parameters_offline = input$report_parameters_offline,
                     report_parameters_calculated = input$report_parameters_calculated,
                     report_add_thresholds = input$report_add_thresholds,
                     report_daterange = report_daterange(),
                     report_timezone = input$report_timezone)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      ofile <-  file.path(tdir, paste('automated_report', sep = '.',
                      switch(input$report_format,
                             PDF = 'pdf',
                             Word = 'docx')))


      output_format <- switch(input$report_format,
                              PDF = "pdf_document",
                              Word = "word_document")

      rmarkdown::render(input = tempReport,
                        output_file = ofile,
                        output_format = output_format,
                        params = params,
                        envir = new.env(parent = globalenv()))
      file.copy(from = ofile,
                  to = file)

    }
  )
}
