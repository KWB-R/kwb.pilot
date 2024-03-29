library(shiny)
library(shinythemes)
library(digest)
library(leaflet)
library(rmarkdown)
library(ggplot2)
library(ggforce)
library(dplyr)
library(tidyr)
library(dygraphs)
library(xts)
library(kwb.pilot)

Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2, encoding = 'UTF-8')

# Global settings (for all sessions)
my_theme <<- shinythemes::shinytheme("readable")


#Read user table for deployment from user table
exists_userTable <- file.exists("tools/userTable.csv")

if (exists_userTable) {
  userTable <<- read.csv(file = "tools/userTable.csv",
                         header = TRUE,
                         sep = ",")
}

#theme <<- "bootstrap.css"

# logo -------------------------------------------------------------------------
logo <<- function
(
  src="project_logo.jpg",
  target = "_blank", ### opens new tab/window
  href="https://www.kompetenz-wasser.de/en/project/mbr40/",
  align="middle",
  label = "MBR4.0_homepage",
  add_div = TRUE,
  ... ### add. arguments passed to img(), e.g. height=40
)
{
  x <- a(
    target = target,
    href = href,
    onclick = sprintf("ga('send', 'event', 'click', 'link', '%s', 1)", label),
    img(src = src, align = align, ...)
  )

  if (add_div) {
    x <- div(x)
  }

  return(x)
}

# siteLogo ---------------------------------------------------------------------
siteLogo <- logo(
  src = "project_website.png",
  href = "https://www.kompetenz-wasser.de/en/project/mbr40/",
  label = "Site: Stahnsdorf",
  add_div = FALSE
)

# footer -----------------------------------------------------------------------
footer <- function
(
  startCol = 9,
  txt = "\u00A9 Kompetenzzentrum Wasser Berlin gGmbH 2021"
)
{
  footerTxt <- tags$footer(tags$h6(txt))
  x <- fixedRow(column(width = 12-startCol, footerTxt, offset = startCol))

  return(x)
}


# reference --------------------------------------------------------------------
reference <- tabPanel("Reference", tags$div(siteLogo))

# shinyServer ------------------------------------------------------------------
shinyServer(function(input, output, session) {

  # Local settings (for each session)
  # Tools ----
  source("tools/login.R", local = TRUE)

  # Modules ----
  source("module/timeSeries.R", local = TRUE)
  source("module/report.R", local = TRUE)
  source("module/site.R", local = TRUE)
  source("module/kwb.R", local = TRUE)
  # Data ----
  #Read user table

  # main page ----
  output$mainPage <- renderUI({

    doLogin()

    if (loginData$LoggedIn == exists_userTable) {

      doLogout()

      server_timeSeries(input, output, session)
      server_report(input, output, session)
      server_site(input, output)
      server_kwb(input, output)

      div(
        class = "",
        fluidPage(
          fluidRow(column(12, column(4, br(), loginInfo()), br(), br(), logo(align = "right"))),
          navbarPage(
            title = "Interactive reporting",
            windowTitle = "kwb.pilot",
            tabPanel(
              "Explore", br(),
              div(class = " ", ui_timeSeries()),
              id = "timeSeries"
            ),
            tabPanel(
              "Report", br(),
              div(class = " ", ui_report()),
              id = "report"),
            tabPanel(
              "Background", br(),
              div(class = " ", reference),
              # tags$iframe(src="http://www.aquanes-h2020.eu/Default.aspx?t=1668",
              #             height = 800,
              #             width = 1500,
              #             frameborder = 0,
              #             seamless = "seamless"),
              id = "hintergrund"
            ),
            tabPanel(
              "Site", br(),
              div(class = " ", ui_site(output)),
              id = "site"
            ),
            tabPanel(
              "KWB", br(),
              div(class = " ", ui_kwb(output)),
              id = "kwb"
            ),
            #navbarMenu("More",
            #            reference,
            #            ui_kwb(output)),
            theme = my_theme,
            footer = footer()
          )
        )
      )
    } else {
      fluidPage(
        fluidRow(
          column(
            1, offset = 5,
            br(), br(), br(), br(),
            h5("Login"),
            loginUI(), br()
          )
        ),
        header = tags$style(type = "text/css", "well { width: 100%; }"),
        theme = my_theme
      )
    }
  })
})
