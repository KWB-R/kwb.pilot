site_info <- read.csv("data/site_info.csv",
                              header = TRUE)



popup_text <- sprintf('%s:<br><b>%s</b><br><a target="_blank" href="%s">%s</a>',
                      site_info$description,
                      site_info$name,
                      site_info$link,
                      site_info$link_text)

map_site <- function(zoom) {
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::setView( lng = site_info$lng,
                      lat = site_info$lat, zoom = zoom) %>%
    leaflet::addPopups(lng = site_info$lng,
                       lat = site_info$lat,
                       popup = popup_text)
}


ui_site <- function(output) {
tabPanel("Site",
         leafletOutput("siteMap_world"),
         br(),
         leafletOutput("siteMap_local")
         )
}

server_site <- function(input, output) {
  output$siteMap_world <- leaflet::renderLeaflet({map_site(zoom = 3)})
  output$siteMap_local <- leaflet::renderLeaflet({map_site(zoom = 15)})
}

# runApp(list(
#   ui = ui_site,
#   server = server_site
# ))

