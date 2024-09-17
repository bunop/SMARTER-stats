
require(dplyr)
require(tidyr)
require(tibble)
require(writexl)
require(rcrossref)
require(smarterapi)

# get all datasets
all_datasets <- smarterapi::get_smarter_datasets()

# get all samples
all_sheep_samples <- smarterapi::get_smarter_samples(species = "Sheep")
all_goat_samples <- smarterapi::get_smarter_samples(species = "Goat")

# get all samples in one table
all_samples <- dplyr::bind_rows(all_sheep_samples, all_goat_samples)

# merge datasets and samples using dplyr
all_samples_with_doi <- dplyr::inner_join(
  all_datasets,
  all_samples,
  by = dplyr::join_by(`_id.$oid` == `dataset_id.$oid`)
)

# select only the columns I need. The first column is the dataset_id, which I need
# to use while grouping if I want to refer samples to their original dataset
columns <- c(
  "_id.$oid",
  "breed.y",
  "country.y",
  "species.y")

# I need to group tmp dataframe by distinct values of breed.y, country.y, species.y and _id.$oid
# and count the number of rows in each group in a new column called "count"
tmp <- all_samples_with_doi %>%
  dplyr::select(all_of(columns)) %>%
  dplyr::group_by(`_id.$oid`, species.y, breed.y, country.y, ) %>%
  dplyr::summarise(count = n())

# create the summary table: join the group table with datasets to collect
# additional info (like the DOI of the dataset, the chip name, the gene array, etc.)
supplementary_table <- left_join(tmp, all_datasets, by = "_id.$oid") %>%
  dplyr::select(all_of(c(
    "_id.$oid", 
    "breed.y", 
    "country.y", 
    "file",
    "gene_array",
    "count",
    "species.y",
    "species",
    "chip_name",
    "doi"))) %>%
  dplyr::rename(
    "dataset_id" = "_id.$oid",
    "breed" = "breed.y",
    "country" = "country.y",
    "scientific_name" = "species.y"
  )

# Function to strip the URL prefix if it exists
strip_doi_url <- function(doi) {
  # Use sub() to remove the https://doi.org/ part if it exists
  stripped_doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi)
  return(stripped_doi)
}

get_first_non_na_author <- function(authors) {
  # Loop through each row of authors (assuming authors is a data frame)
  for (i in seq_len(nrow(authors))) {
    # Access each author as a row (a list or named vector)
    author <- authors[i, ]
    
    # Check if family name exists and is not NA
    if (!is.null(author[["family"]]) && !is.na(author[["family"]])) {
      return(author[["family"]])  # Return the first valid family name
    }
  }
  
  # If no valid family name found, return "Unknown Author"
  return("Unknown Author")
}

# Create an empty cache environment
doi_cache <- new.env(parent = emptyenv())

# define an helper function to collect DOI information using cache
get_citation_from_doi <- function(doi) {
  # First strip any URL prefix from the DOI
  doi <- strip_doi_url(doi)
  
  # Silently return NA for NA input
  if (is.na(doi)) {
    return(NA)
  }
  
  # Check if DOI is already cached
  if (exists(doi, envir = doi_cache)) {
    # Return cached result
    return(doi_cache[[doi]])
  }
  
  # If not cached, retrieve metadata and cache the result
  tryCatch({
    # Retrieve metadata from CrossRef
    metadata <- rcrossref::cr_cn(doi, format = "citeproc-json")
    
    # Check if author information exists and use the helper function to get the first valid author
    if (!is.null(metadata$author) && length(metadata$author) > 0) {
      first_author <- get_first_non_na_author(metadata$author)
    } else {
      first_author <- "Unknown Author"
    }
    
    # Extract the year
    if (!is.null(metadata$issued$`date-parts`[[1]][1])) {
      year <- metadata$issued$`date-parts`[[1]][1]
    } else {
      year <- "Unknown Year"
    }
    
    # Create citation in "Author et al. Year" format
    citation <- paste(first_author, "et al.", year)
    
    # Store the result in the cache
    doi_cache[[doi]] <- citation
    
    # Return the citation
    return(citation)
  }, error = function(e) {
    # Handle errors (e.g., invalid DOI) and cache the result as NA
    doi_cache[[doi]] <- NA
    return(NA)
  })
}

# Apply the function to the DOI column and create a new column 'citation'
supplementary_table <- supplementary_table %>%
  dplyr::mutate(citation = sapply(doi, get_citation_from_doi)) %>%
  dplyr::mutate(citation = tidyr::replace_na(citation, "unpublished")) %>%
  dplyr::select(c(
    "dataset_id",
    "species",
    "scientific_name",
    "breed",
    "country",
    "gene_array",
    "chip_name",
    "count",
    "file", 
    "citation", 
    "doi")) %>%
  dplyr::arrange(species, dataset_id, breed, country)

# Assuming you already have an environment named doi_cache
# with DOIs as names and short citations as values

# Helper function to get full citation in apa style
get_full_citation_apa <- function(doi) {
  tryCatch({
    # Fetch the full citation in apa style using rcrossref
    full_citation <- rcrossref::cr_cn(doi, format = "text", style = "apa")
    return(full_citation)
  }, error = function(e) {
    # If there's an error (e.g., invalid DOI), return NA
    return(NA)
  })
}

# Convert doi_cache environment to a data frame
doi_list <- ls(envir = doi_cache)  # Get DOIs (names in the environment)
short_citations <- mget(doi_list, envir = doi_cache)  # Get short citations (values in the environment)

# Create a data frame with DOIs and short citations
bibliography <- data.frame(
  doi = doi_list,  # First column: DOI
  short_citation = unlist(short_citations)  # Second column: Short citation
)

# Add a column for the full citation using the apa style
bibliography <- bibliography %>%
  mutate(full_citation = sapply(doi, get_full_citation_apa)) %>%
  dplyr::select(c("short_citation", "full_citation", "doi")) %>%
  tibble::as_tibble()

# Write to an Excel file
writexl::write_xlsx(
  list(
    datasets = supplementary_table,
    bibliography = bibliography),
  "smarter_datasets_v0.4.10.xlsx")
