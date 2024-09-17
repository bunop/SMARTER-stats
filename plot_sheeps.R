
library(leaflet)
library(smarterapi)

# collect all sheeps (foreground + background) as GeoJSON
sheep_data <- get_smarter_geojson("Sheep")

# get the first point from a multipoint object
to_point <- function(geometry) {
  sf::st_point(geometry[1:2])
}

# lapply and convert a list into a geometry list
sheep_data$geometry <- sf::st_sfc(lapply(sheep_data$geometry, to_point))

# draw sheep data into a map
leaflet(data = sheep_data) %>%
  leaflet::addTiles() %>%
  addMarkers(
    clusterOptions = markerClusterOptions(), label = ~smarter_id
  )

