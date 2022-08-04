# HEADER ----------------------------------------------------------------------
#
# Title:
# Description:
#
#
#
# Authors:      Hugo Tameirao Seixas
# Contact:      seixas.hugo@protonmail.com
# Date:         2022-07-07
#
# Notes:
#
#
#
#
#
# LIBRARIES -------------------------------------------------------------------
#
library(sf)
library(sfarrow)
library(rhdf5)
library(geobr)
library(tidyverse)
library(fs)
library(glue)
library(httr)
library(jsonlite)
#
# OPTIONS ---------------------------------------------------------------------
#
#
# SET DATA IDENTIFICATION -----------------------------------------------------

# GEDI L4A DOI
doi <- "10.3334/ORNLDAAC/2056"

# CMR API base url
cmr_url <- "https://cmr.earthdata.nasa.gov/search/"

# Search earthdata collection url
doi_search <- glue(cmr_url, "collections.json?doi=", doi)

# Get the concept id of the GEDIL4A data
concept_id <-
  GET(url = doi_search) %>%
  parse_json()

concept_id <- concept_id$feed$entry[[1]]$id

# SET GEOGRAPHIC AND TEMPORAL EXTENT ------------------------------------------

# Load amazon biome limits
amazon <- geobr::read_biomes(year = 2019) %>%
  filter(code_biome == 1) %>%
  st_transform(crs = "EPSG:4326")

# Set bounding box
bounding_box <- st_bbox(amazon)

# Set the time range
time_range <- "2019-01-01T00:00:00Z,2023-01-01T00:00:00Z"

# RETRIEVE DATA URL TO PERFORM DOWNLOAD ---------------------------------------

# Iterate search result pages to retireve all the files
url_table <-
  map_dfr(
    .x = c(1:3),
    function(page) {

      # Set the search filters
      response <-
        httr::GET(
          url = glue(cmr_url, "granules.json"),
          query = list(
            collection_concept_id = concept_id,
            page_size = 2000,
            page_num = page,
            temporal = time_range,
            `bounding_box[]` = str_c(bounding_box, collapse = ",")
          )
        )

      # Get information from the granules
      granules <- jsonlite::parse_json(response)$feed$entry

      # Get the URL from each granule
      granule_url_list <- map_chr(granules, ~ { .x["links"][[1]][[1]]$href })

      # Get the name of each granule
      granule_title_list <- map_chr(granules, ~ { .x["title"][[1]] })

      # Merge file names and URL in a table
      tibble(
        granule_url = granule_url_list,
        granule_title = granule_title_list
      )

    }
  )

# DOWNLOAD SUBSET AND WRITE DATA ----------------------------------------------

# Create dir to store data
dir_create("data/gedi_04A")

# Iterate trough all the URL and files
walk2(
  .x = url_table$granule_url,
  .y = url_table$granule_title,
  function(url_link, file_name) {

    # Download granule
    GET(
      url = url_link,
      write_disk(glue("./data/gedi_04A/{file_name}"), overwrite = TRUE),
      config(netrc = TRUE, netrc_file = "auth/.netrc"),
      set_cookies("LC" = "cookies")
    )

    # Get the main groups names of the HDF5 file
    h5f_names <-
      h5ls(glue("data/gedi_04A/{file_name}"), recursive = FALSE) %>%
      filter(str_detect(name, 'BEAM')) %>%
      pull(name)

    # Iterate trough each GEDI beam to retrieve data
    walk(
      .x = h5f_names,
      function(beam) {

        # Open HDF5 file
        h5f <-
          h5read(
            glue("data/gedi_04A/{file_name}"),
            name = beam,
            compoundAsDataFrame = FALSE,
            bit64conversion = "bit64"
          )

        # Extract useful data into a table and apply filters
        h5f_table <-
          tibble(
            shot_number = h5f$shot_number,
            time = h5f$delta_time,
            lat = h5f$lat_lowestmode,
            lon = h5f$lon_lowestmode,
            elev = h5f$elev_lowestmode,
            agbd = h5f$agbd,
            agbd_pi_lower = h5f$agbd_pi_lower,
            agbd_pi_upper = h5f$agbd_pi_upper,
            agbd_se = h5f$agbd_se,
            l4_qf = h5f$l4_quality_flag,
            pft = h5f$land_cover_data$pft_class
          ) %>%
          filter(
            l4_qf == 1,
            pft == 02 | pft == 04
          )

        # Include geographic information to the table
        h5f_geo_table <- h5f_table %>%
          st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>%
          st_join(amazon, left = FALSE)

        # Create the name of the file to be written
        product <- str_sub(glue("./data/gedi_04A/{file_name}"), 43, 84)

        # Write file as a parquet table
        st_write_parquet(
          h5f_geo_table,
          glue("data/gedi_04A/{product}_{beam}.parquet")
        )

      }
    )

    # Delete HDF5 file (too big to store in the computer)
    file_delete(glue("./data/gedi_04A/{file_name}"))

  }
)
