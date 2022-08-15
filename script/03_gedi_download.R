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
library(terra)
library(sf)
library(arrow)
library(sfarrow)
library(rhdf5)
library(geobr)
library(fs)
library(glue)
library(httr)
library(jsonlite)
library(tidyverse)
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

sb <- st_read_parquet("data/samples.parquet") %>%
  st_transform("EPSG:4326")

# Set the time range
time_range <- "2019-01-01T00:00:00Z,2023-01-01T00:00:00Z"

# RETRIEVE DATA URL TO PERFORM DOWNLOAD ---------------------------------------

url_table <-
  map_df(
    .x = unique(sb$rep),
    function(sample_round) {

      id_list <- sb %>%
        filter(rep == sample_round) %>%
        pull(id)

      h5f_geo_table <-
        map_df(
          .x = id_list,
          function(sample_id) {

            geometry <- sb %>%
              filter(id == sample_id, rep == sample_round)

            bounding_box <- st_bbox(st_geometry(geometry))

            # Set the search filters
            response <-
              GET(
                url = glue(cmr_url, "granules.json"),
                query = list(
                  collection_concept_id = concept_id,
                  page_size = 2000,
                  page_num = 1,
                  temporal = time_range,
                  `bounding_box[]` = str_c(bounding_box, collapse = ",")
                )
              )

            # Get information from the granules
            granules <- parse_json(response)$feed$entry

            # Get the URL from each granule
            granule_url_list <-
              map_chr(granules, ~ { .x["links"][[1]][[1]]$href })

            # Get the name of each granule
            granule_title_list <-
              map_chr(granules, ~ { .x["title"][[1]] })

            # Merge file names and URL in a table
            url_table <-
              tibble(
                granule_url = granule_url_list,
                granule_title = granule_title_list
              ) %>%
              mutate(
                sample_round = sample_round,
                sample_id = sample_id
              )

          }
        )

    }
  )

# Create dir to store data
dir_create(glue("data/gedi_04A/"))

url_table <- url_table %>%
  group_by(granule_url) %>%
  mutate(granule_id = cur_group_id()) %>%
  arrange(granule_id) %>%
  ungroup()

write_parquet(url_table, "data/gedi_04A/gedi_url.parquet")

walk(
  .x = unique(url_table$granule_id),
  function(url_id) {

    id_list <- url_table %>%
      filter(granule_id == url_id)

    url_link <- id_list %>%
      distinct(granule_url) %>%
      pull(granule_url)

    file_name <- id_list %>%
      distinct(granule_title) %>%
      pull(granule_title)

    # Download granule
    GET(
      url = url_link,
      write_disk(
        glue("./data/gedi_04A/{file_name}"),
        overwrite = TRUE
      ),
      config(netrc = TRUE, netrc_file = "auth/.netrc"),
      set_cookies("LC" = "cookies")
    )

    # Get the main groups names of the HDF5 file
    h5f_names <-
      h5ls(
        glue("data/gedi_04A/{file_name}"),
        recursive = FALSE
      ) %>%
      filter(str_detect(name, "BEAM")) %>%
      pull(name)

    h5f_geo_table <-
      map2_df(
        .x = id_list$sample_id,
        .y = id_list$sample_round,
        function(sample_id, sample_round) {

          geometry <- sb %>%
            filter(id == sample_id, rep == sample_round)

          # Iterate trough each GEDI beam to retrieve data
          h5f_geo_table <-
            map_df(
              .x = h5f_names,
              function(beam) {

                cat(
                  "\r",
                  "rep: ", sample_round,
                  "id: ", sample_id,
                  "beam: ", beam,
                  "file: ", url_id,
                  "       "
                )

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
                    beam = beam,
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
                  drop_na(lat, lon)

                # Include geographic information to the table
                h5f_geo_table <- h5f_table %>%
                  st_as_sf(
                    coords = c("lon", "lat"),
                    crs = "EPSG:4326"
                  ) %>%
                  st_join(geometry, left = FALSE) %>%
                  mutate(
                    sample_id = sample_id,
                    sample_rep = sample_round
                  )

                h5closeAll()

                return(h5f_geo_table)

              }
            )

        }
      )

    # Delete HDF5 file (too big to store in the computer)
    file_delete(glue("data/gedi_04A/{file_name}"))

    st_write_parquet(
      h5f_geo_table,
      glue("data/gedi_04A/gedi_04A_{url_id}.parquet")
    )

  }
)
