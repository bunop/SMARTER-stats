
require(dplyr)
require(smarterapi)

# get dataset
sheep_bg_dst <- smarterapi::get_smarter_datasets(query = list(species = "Sheep", type = "background"))
sheep_fg_dst <- smarterapi::get_smarter_datasets(query = list(species = "Sheep", type = "foreground"))

# get samples
sheep_bg_samples <- smarterapi::get_smarter_samples("Sheep", query = list(type = "background"))
sheep_fg_samples <- smarterapi::get_smarter_samples("Sheep", query = list(type = "foreground"))

# merge tables
sheep_bg_data <- merge(sheep_bg_dst, sheep_bg_samples, by.x = "_id.$oid", by.y = "dataset_id.$oid")
sheep_fg_data <- merge(sheep_fg_dst, sheep_fg_samples, by.x = "_id.$oid", by.y = "dataset_id.$oid")

# remove and rename columns
sheep_bg_data <- sheep_bg_data %>%
  dplyr::select(-c(breed.x, chip_name.x, country.x, species.x, type.x, contents)) %>%
  dplyr::rename(
    "dataset_id$oid" = "_id.$oid",
    "breed" = "breed.y",
    "country" = "country.y",
    "sample_id$oid" = "_id.$oid.y",
    "species" = "species.y",
    "type" = "type.y",
    "chip_name" = "chip_name.y"
  )

sheep_fg_data <- sheep_fg_data %>%
  dplyr::select(-c(breed.x, chip_name.x, country.x, species.x, type.x, contents)) %>%
  dplyr::rename(
    "dataset_id$oid" = "_id.$oid",
    "breed" = "breed.y",
    "country" = "country.y",
    "sample_id$oid" = "_id.$oid.y",
    "species" = "species.y",
    "type" = "type.y",
    "chip_name" = "chip_name.y"
  )

# track how many samples have coordinates and purpose
sheep_bg_data$with_coordinates <- !is.na(sheep_bg_data$locations.type)
sheep_bg_data$with_purpose <- !is.na(sheep_bg_data$phenotype.purpose)

sheep_fg_data$with_coordinates <- !is.na(sheep_fg_data$locations.type)
sheep_fg_data$with_purpose <- !is.na(sheep_fg_data$phenotype.purpose)

# group and count animals
sheep_bg_breed_count <- sheep_bg_data %>%
  dplyr::group_by(partner, breed, type, with_coordinates, with_purpose) %>%
  dplyr::tally()
sheep_fg_breed_count <- sheep_fg_data %>%
  dplyr::group_by(partner, breed, type, with_coordinates, with_purpose) %>%
  dplyr::tally()

# write output tables
write.csv(sheep_bg_breed_count, "sheep_background_counts.csv")
write.csv(sheep_fg_breed_count, "sheep_foreground_counts.csv")
