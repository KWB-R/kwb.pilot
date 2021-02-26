
map_kwb <- function (zoom) {
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::setView( lng = 13.301067, lat = 52.491697, zoom = zoom) %>%
    leaflet::addPopups(lng = 13.301067,
          lat = 52.491697,
          popup = 'You can find us here:<br><b>Kompetenzzentrum Wasser Berlin gGmbH</b>
          <br>Address: Cicerostrasse 24, 10709 Berlin (Germany)<br>
           <a target="_blank" href="http://www.kompetenz-wasser.de">http://www.kompetenz-wasser.de</a>')
}

# ui_kwb <- function (...) {
#   navbarPage(title="Interactive Hantush",
#   navbarMenu("More",
#              tabPanel("Leaflet",
#                       leafletOutput("kwbLocation"))
#      ))
# }

ui_kwb <- function (output) {
tabPanel("KWB",
         leafletOutput("kwbMap_world"),
         br(),
         leafletOutput("kwbMap_local"))
}

server_kwb <- function(input, output) {
  output$kwbMap_world <- leaflet::renderLeaflet({map_kwb(zoom=1)})
  output$kwbMap_local <- leaflet::renderLeaflet({map_kwb(zoom=15)})
}

# runApp(list(
#   ui = ui_kwb,
#   server = server_kwb
# ))
