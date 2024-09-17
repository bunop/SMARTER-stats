
library(httr)
library(jsonlite)
library(dplyr)

read_url <- function(url, query = list()) {
  # make a GET request to the API by combining parameters (if any)
  resp <-
    GET(url, query = query)

  # check errors: SMARTER-backend is supposed to return JSON objects
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  # parse a JSON response. fromJSON to flatten results
  parsed <-
    jsonlite::fromJSON(
      content(resp, "text", encoding = "utf-8"),
      flatten = TRUE
    )

  # deal with API errors: not "200 Ok" status
  if (http_error(resp)) {
    stop(
      sprintf(
        "SMARTER API returned an error [%s]: '%s'",
        status_code(resp),
        parsed$message
      ),
      call. = FALSE
    )
  }

  return(parsed)
}


# a generic function to get data from the SMARTER API and deal with pagination
get_smarter_data <- function(url, query = list()) {
  # do the request and parse data with our function
  parsed <- read_url(url, query)

  # track results in df
  results <- parsed$items

  # check for pagination
  while (!is.null(parsed$`next`)) {
    # append next value to base url
    next_url <- httr::modify_url(base_url, path = parsed$`next`)

    # query arguments are already in url: get next page
    parsed <- read_url(next_url)

    # append new results to df. Deal with different columns
    results <- dplyr::bind_rows(results, parsed$items)
  }

  # return an S3 obj with the data we got
  structure(list(
    content = parsed,
    url = url,
    results = results
  ),
  class = "smarter_api")
}

base_url <- "http://localhost:27080"

get_smarter_datasets <- function(query=list()) {
  url <-
    modify_url(base_url, path = "/smarter-api/datasets")
  
  data <- get_smarter_data(url, query)
  
  # returning only the results dataframe
  data$results
}

all_datasets <- get_smarter_datasets()

get_smarter_breeds <- function(query = list()) {
  # setting the URL endpoint
  url <- httr::modify_url(base_url, path = "/smarter-api/breeds")
  
  # reading our data
  data <- get_smarter_data(url, query)
  
  # returning only the results dataframe
  data$results
}

goat_breeds <-
  get_smarter_breeds(query = list(species = "Goat"))

genotypes_datasets <- get_smarter_datasets(query = list(type="genotypes"))

foreground_genotypes_datasets <- get_smarter_datasets(
  query = list(type="genotypes", type="foreground"))

search_goat_breeds <-
  get_smarter_breeds(query = list(
    species = "Goat", search = "land")
  )

search_goat_breeds <- search_goat_breeds %>% select(name, code)

get_smarter_samples <- function(species, query = list()) {
  # mind that species is lowercase in endpoint url
  species <- tolower(species)
  
  url <-
    modify_url(base_url, path = sprintf("/smarter-api/samples/%s", species))
  
  data <- get_smarter_data(url, query)
  
  # returning only the results dataframe
  data$results
}

landrace_samples <- get_smarter_samples(
  species = "Goat",
  query = list(breed_code = "LNR")
)

selected_landrace_samples <- get_smarter_samples(
  species = "Goat",
  query = list(
    breed_code = "LNR",
    locations__exists = TRUE,
    phenotype__exists = TRUE)
)

selected_landrace_samples %>% select(smarter_id, breed_code)

get_smarter_variations <- function(species, assembly, query = list()) {
  # mind that species is lowercase in endpoint url
  species <- tolower(species)
  assembly <- toupper(assembly)
  
  url <-
    modify_url(base_url, path = sprintf("/smarter-api/variants/%s/%s", species, assembly))
  
  data <- get_smarter_data(url, query)
  
  # returning only the results dataframe
  data$results
}

selected_goat_variations <- get_smarter_variations(
  species = "Goat",
  assembly = "ARS1",
  query = list(
    size = 100,
    region = "1:1-1000000"
  )
)
