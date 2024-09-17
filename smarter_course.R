
require(sf)
require(dplyr)
require(terra)
require(geodata)
require(smarterapi)

smarter_goats <- get_smarter_geojson(
  species = "Goat",
  query = list(
    country="Italy",
    type="background",
    breed="Orobica",
    breed="Aspromontana",
    breed="Bionda dell'Adamello",
    breed="Argentata"
  )
)

# transform into POINT
smarter_goats <- smarter_goats %>% sf::st_cast("POINT", do_split=FALSE)

leaflet::leaflet(data = smarter_goats) %>% leaflet::addTiles() %>% leaflet::addMarkers(
  clusterOptions = leaflet::markerClusterOptions(), label = ~smarter_id
)

# downloaded and extracted by hand
worldclim_bio <- geodata::worldclim_global(var = "bio", res = 0.5, path = "/home/core/worldclim/bio")
worldclim_elev <- geodata::worldclim_global(var = "elev", res = 0.5, path = "/home/core/worldclim/elev")

bio_data <- terra::extract(worldclim_bio, terra::vect(smarter_goats))
elev_data <- terra::extract(worldclim_elev, terra::vect(smarter_goats))

# rename columns
names(bio_data) <- c("ID", paste0("bio", seq(1,19)))
names(elev_data) <- c("ID", "elev")

smarter_training <- cbind(smarter_goats, merge(elev_data, bio_data))

st_write(smarter_training, "smarter_training.shp")
st_write(smarter_training, "smarter_training.csv")
st_write(smarter_training, "smarter_training.geojson")
st_write(smarter_training, "smarter_training.xlsx")
