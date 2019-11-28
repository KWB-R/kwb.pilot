#jQuery <- includeScript("http://code.jquery.com/jquery-2.1.3.js")
exists_ga <- file.exists("tools/google-analytics.js")

if (exists_ga) {
googleAnalytics <- includeScript("tools/google-analytics.js")
}

shinyUI(
  fluidPage(
    #tags$head(jQuery),
    if (exists_ga) tags$head(googleAnalytics),
    uiOutput("mainPage")
  )
)
