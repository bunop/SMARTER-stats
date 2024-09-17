
require(dplyr)
require(smarterapi)

# get dataset
goat_bg_dst <- smarterapi::get_smarter_datasets(query = list(species = "Goat", type = "background"))
goat_fg_dst <- smarterapi::get_smarter_datasets(query = list(species = "Goat", type = "foreground"))

# get samples
goat_bg_samples <- smarterapi::get_smarter_samples("Goat", query = list(type = "background"))
goat_fg_samples <- smarterapi::get_smarter_samples("Goat", query = list(type = "foreground"))

# merge tables
goat_bg_data <- merge(goat_bg_dst, goat_bg_samples, by.x = "_id.$oid", by.y = "dataset_id.$oid")
goat_fg_data <- merge(goat_fg_dst, goat_fg_samples, by.x = "_id.$oid", by.y = "dataset_id.$oid")

# remove and rename columns
goat_bg_data <- goat_bg_data %>%
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

goat_fg_data <- goat_fg_data %>%
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
goat_bg_data$with_coordinates <- !is.na(goat_bg_data$locations.type)
goat_bg_data$with_purpose <- !is.na(goat_bg_data$phenotype.purpose)

goat_fg_data$with_coordinates <- !is.na(goat_fg_data$locations.type)
goat_fg_data$with_purpose <- !is.na(goat_fg_data$phenotype.purpose)

# group and count animals
goat_bg_breed_count <- goat_bg_data %>% 
  dplyr::group_by(partner, breed, type, with_coordinates, with_purpose) %>%
  dplyr::tally()
goat_fg_breed_count <- goat_fg_data %>% 
  dplyr::group_by(partner, breed, type, with_coordinates, with_purpose) %>%
  dplyr::tally()

# write output tables
write.csv(goat_bg_breed_count, "goat_background_counts.csv")
write.csv(goat_fg_breed_count, "goat_foreground_counts.csv")
