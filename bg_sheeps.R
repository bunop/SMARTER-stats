
library(smarterapi)
library(leaflet)
library(sf)
library(dplyr)
library(elevatr)
library(maps)

sheep_data <- get_smarter_geojson(species="Sheep")

sheep_data <- sheep_data %>% st_cast("POINT", do_split=FALSE)

bg_sheep_data <- filter(sheep_data, type == "background" )

prj_dd <- "EPSG:4326"
df_elev_aws <- elevatr::get_elev_point(sheep_data, prj = prj_dd, src = "aws")

world <- st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))

test <- select(df_elev_aws, breed, breed_code, country, elevation) %>% count(breed, country, elevation)

factpal <- colorFactor(topo.colors(length(levels(as.factor(test$country)))), test$country)

leaflet(data = df_elev_aws) %>%
  # leaflet::addTiles() %>%
  addPolygons(data = world, weight = 1, label = ~ID) %>%
  # addMarkers(
  #   clusterOptions = markerClusterOptions(), label = ~smarter_id
  # )
  addCircleMarkers(
    data = test,
    label =~ breed,
    color =~ factpal(country),
    radius = ~sqrt(n)
  )

