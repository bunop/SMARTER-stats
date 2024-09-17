
library(geodata)
library(data.table)
library(sf)
library(terra)

locationFile <- "/home/paolo/.cache/R/renv/cache/v5/R-4.2/x86_64-pc-linux-gnu/R.SamBada/0.1.3/1f9d215581c37b41ef876813560d4365/R.SamBada/extdata/uganda-subset.csv"
data <- fread(locationFile)
sf_data <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

worldclim_bio <- worldclim_global(var = "bio", res = 10, path = tempdir())
bio_data <- terra::extract(worldclim_bio, vect(sf_data))
cropped_bio = crop(worldclim_bio, vect(sf_data))

worldclim_elev <- worldclim_global(var = "elev", res = 10, path = tempdir())
elev_data <- terra::extract(worldclim_elev, vect(sf_data))
cropped_elev = crop(worldclim_elev, vect(sf_data))

plot(cropped_elev)
points(vect(sf_data))

uganda_data <- cbind(sf_data, merge(elev_data, bio_data))
