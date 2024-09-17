
library(smarterapi)
library(sf)
library(terra)
library(geodata)


polygons <- st_read("~/Desktop/shapes.shp")
goat_data <- get_smarter_geojson("Goat", query = list(type = "background"))
goat_data <- goat_data %>% sf::st_cast("POINT", do_split = FALSE)
sel_sgbp <- st_intersects(goat_data, polygons)
sel_logical <- lengths(sel_sgbp) > 0
selected_goats <- goat_data[sel_logical, ]

worldclim_bio <- worldclim_global(var = "bio", res = 10, path = tempdir())
bio_data <- terra::extract(worldclim_bio, vect(selected_goats))
cropped_bio = crop(worldclim_bio, vect(selected_goats))

worldclim_elev <- worldclim_global(var = "elev", res = 10, path = tempdir())
elev_data <- terra::extract(worldclim_elev, vect(selected_goats))

# enlarge spatextent
bbox <- ext(vect(selected_goats)) + 1

cropped_elev = crop(worldclim_elev, bbox)

plot(cropped_elev)
points(vect(selected_goats))

smarter_goats <- cbind(selected_goats, merge(elev_data, bio_data))
