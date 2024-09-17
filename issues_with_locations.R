
library(smarterapi)

# read a summary about animal locations
data <- read.table("animals_locations6.txt", header = TRUE, sep = " ")

# select row where there's a mismatch between the assigned country and
# the GPS coordinates
issues <- data[data$comp == FALSE, ]

# remove NA rows from issues dataframe
issues <- issues[complete.cases(issues), ]

# select samples with France as assigned country
france_issues <- issues[issues$country == "France", ]

# collect geo data from merino sheeps
merino_sheep <- get_smarter_geojson(
  "Sheep", 
  query = list(breed = "Merino")) %>%
    sf::st_cast("POINT", do_split = FALSE)

# display data with leaflet
leaflet::leaflet(
  data = merino_sheep
  ) %>%
    leaflet::addTiles() %>%
    leaflet::addMarkers(
      clusterOptions = leaflet::markerClusterOptions(), label = ~smarter_id
    )

# collect geo data from Romanov sheeps
romanov_sheep <- get_smarter_geojson(
  "Sheep", 
  query = list(breed = "Romanov")) %>%
    sf::st_cast("POINT", do_split = FALSE)

# display data with leaflet
leaflet::leaflet(
  data = romanov_sheep
  ) %>%
    leaflet::addTiles() %>%
    leaflet::addMarkers(
      clusterOptions = leaflet::markerClusterOptions(), label = ~smarter_id
    )
