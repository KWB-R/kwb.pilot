server_timeSeries <- function(...) {



  ts_agg <- reactive({

    object_name <- sprintf("siteData_%s_list", input$temporal_aggregation)

    if (exists(object_name)) {
      get(object_name)
    } else {
      dat <- kwb.pilot::read_fst(path = sprintf("data/%s.fst", object_name))
      assign(x = object_name,
             value = dat)
      dat
    }

  })



  ts_tz <- reactive({
    kwb.pilot::change_timezone(df = ts_agg(),
                                    tz = input$timezone)
  })



  # ts_errors <- reactive({
  #   condi <- ts_tz()[, "ParameterCode"] == "errcode" & ts_tz()[,"ParameterValue"] != 0
  #   ts_tz()[ts_tz()$ParameterCode == "errcode" & ts_tz()$ParameterValue != 0,]
  # })

  ts_data1 <- reactive({

    req(input$parameter1)

    date_idx <- as.Date(ts_tz()[,"DateTime"]) >= input$daterange[1] & as.Date(ts_tz()[,"DateTime"]) <= input$daterange[2]
    site_idx <- ts_tz()[,"SiteName"] %in% input$sitename
    para_idx <- ts_tz()[,"ParameterName"] %in%  input$parameter1
    row_idx <- date_idx & site_idx & para_idx
    ts_tz()[row_idx, c("DateTime",
                       "SiteName_ParaName_Unit",
                       "ParameterValue")] %>%
      tidyr::spread_(key_col = "SiteName_ParaName_Unit",
                     value_col = "ParameterValue")


  })

  ts_data2 <- reactive({

    req(input$parameter2)

    date_idx <- as.Date(ts_tz()[,"DateTime"]) >= input$daterange[1] & as.Date(ts_tz()[,"DateTime"]) <= input$daterange[2]
    site_idx <- ts_tz()[,"SiteName"] %in% input$sitename
    para_idx <- ts_tz()[,"ParameterName"] %in%  input$parameter2
    row_idx <- date_idx & site_idx & para_idx
    ts_tz()[row_idx, c("DateTime",
                       "SiteName_ParaName_Unit",
                       "ParameterValue")] %>%
      tidyr::spread_(key_col = "SiteName_ParaName_Unit",
                     value_col = "ParameterValue")


  })




ts_data1_xts <- reactive({


  xts::xts(x = ts_data1()[,c(-1), drop = FALSE],
           order.by = ts_data1()$DateTime,
           tzone = base::attr(ts_data1()$DateTime,
                              "tzone"))

  })



  output$dygraph1 <- renderDygraph({


    dy1 <- dygraph(data = ts_data1_xts(),
            group = "dy_group",
           # main = unique(ts_data()$LocationName),
                    ylab = "Parameter value") %>%
             dyLegend(show = "always",
                      hideOnMouseOut = FALSE,
                      width = 900) %>%
             dyRangeSelector(dateWindow = input$daterange) %>%
             dyOptions(useDataTimezone = TRUE,
                       retainDateWindow = input$fix_daterange,
                       connectSeparatedPoints = TRUE,
                       drawPoints = TRUE,
                       pointSize = 2)

  if (input$add_thresholds == TRUE) {
    kwb.pilot::dygraph_add_limits(dygraph = dy1,
                                       limits = thresholds[thresholds$ParameterName %in% input$parameter1,])
  } else {
    dy1
  }

  })


  ts_data2_xts <- reactive({


    xts::xts(x = ts_data2()[,c(-1), drop = FALSE],
             order.by = ts_data2()$DateTime,
             tzone = base::attr(ts_data2()$DateTime,
                                "tzone"))

  })



  output$dygraph2 <- renderDygraph({

    dy2 <- dygraph(data = ts_data2_xts(),
            group = "dy_group",
            # main = unique(ts_data()$LocationName),
            ylab = "Parameter value") %>%
      dyLegend(show = "always",
               hideOnMouseOut = FALSE,
               width = 900) %>%
      dyRangeSelector(dateWindow = input$daterange) %>%
      dyOptions(useDataTimezone = TRUE,
                retainDateWindow = input$fix_daterange,
                connectSeparatedPoints = TRUE,
                drawPoints = TRUE,
                pointSize = 2)

    if (input$add_thresholds == TRUE) {
    kwb.pilot::dygraph_add_limits(dygraph = dy2,
               limits = thresholds[thresholds$ParameterName %in% input$parameter2,])
    } else {
      dy2
    }
  })



  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "dygraph.Rmd")
      file.copy("report/dygraph.Rmd", tempReport, overwrite = TRUE)


      if (all(is.null(input$parameter1))) {
        myData1 <- NA
      } else {
        myData1 <- ts_data1_xts()
      }

      if (all(is.null(input$parameter2))) {
        myData2 <- NA
      } else {
        myData2 <- ts_data2_xts()
      }

      # Set up parameters to pass to Rmd document
      params <- list(myData1 = myData1,
                     myData2 = myData2,
                     para1 = input$parameter1,
                     para2 = input$parameter2,
                     add_thresholds = input$add_thresholds,
                     myDateRange = input$daterange,
                     myTimezone = input$timezone)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    })




    export_df <- reactive({
      switch(input$dataset,
             "data_plot1" = ts_data1(),
             "data_plot2" = ts_data2())})


    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataset, "_", input$timezone, ".csv",
              sep = "")
      },
      content = function(file) {
        write.csv(ggplot2::fortify(export_df()), file, row.names = FALSE)
      }
    )


}

ui_timeSeries <- function(...) {
  fluidPage(
  titlePanel("Time series"),

  sidebarLayout(
    sidebarPanel(
      tags$head(tags$style(type="text/css", "
             #loadmessage {
                           position: fixed;
                           top: 0px;
                           left: 0px;
                           width: 100%;
                           padding: 5px 0px 5px 0px;
                           text-align: center;
                           font-weight: bold;
                           font-size: 100%;
                           color: #000000;
                           background-color: #CCFF66;
                           z-index: 105;
}
")),
      selectInput("temporal_aggregation", label = "Select temporal aggregation",
                  choices = c("raw", "10min", "hour", "day"),
                  selected = "10min"),
      selectInput("timezone", label = "Select a timezone",
                  choices = c("CET", "UTC"),#kwb.pilot::get_valid_timezones()$TZ.,
                  selected = "CET"),
      dateRangeInput('daterange',
                     label = 'Date range input: yyyy-mm-dd',
                     start = "2019-11-01",
                     end = Sys.Date()),
      checkboxInput('fix_daterange', "Fix daterange", value = FALSE),
      selectInput("sitename", label = "Select a sampling point",
                  choices = unique(siteData_10min_list$SiteName),
                  multiple = TRUE,
                  selected = unique(siteData_10min_list$SiteName)),
      selectInput("parameter1", label = "Select a parameter(s) for plot 1",
                  choices = list(Online =  unique(siteData_10min_list$ParameterName[siteData_10min_list$Source == "online"]),
                                 Offline = unique(siteData_10min_list$ParameterName[siteData_10min_list$Source == "offline"])),
                  multiple = TRUE),
      selectInput("parameter2", label = "Select a parameter(s) for plot 2",
                  choices = list(Online =  unique(siteData_10min_list$ParameterName[siteData_10min_list$Source == "online"]),
                                 Offline = unique(siteData_10min_list$ParameterName[siteData_10min_list$Source == "offline"])),
                  multiple = TRUE),
      checkboxInput('add_thresholds', "Add thresholds to plots 1+2", value = FALSE),
      downloadButton("report", "Download plot"),
      selectInput("dataset", "Choose a dataset to download:",
                  choices = c("data_plot1", "data_plot2")),
      downloadButton('downloadData', 'Download data'),
      conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                       tags$div("Loading... (this may take ~ 1 minute)",
                                id = "loadmessage"))
    ),
    mainPanel(
       dygraphOutput("dygraph1"),
       h1(textOutput("")),
       h1(textOutput("")),
       dygraphOutput("dygraph2")
    )
  )
)
}

