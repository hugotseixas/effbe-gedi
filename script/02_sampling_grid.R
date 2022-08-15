# HEADER ----------------------------------------------------------------------
#
# Title:
# Description:
#
#
#
# Authors:      Hugo Tameirao Seixas
# Contact:      seixas.hugo@protonmail.com
# Date:         2022-07-29
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
library(geojsonsf)
library(sfarrow)
library(geobr)
library(glue)
library(fs)
library(magrittr)
library(tidyverse)
#
# OPTIONS ---------------------------------------------------------------------
#

#
# SET GEOGRAPHIC EXTENT -------------------------------------------------------

# Load amazon biome limits
amazon <- st_read_parquet("data/aoi/raisg.parquet")

# LOAD PRODUCTS ---------------------------------------------------------------

cc <- rast("data/control_cells.tif")

ec <- rast("data/event_cells.tif")

gc <- c(cc, ec)

names(gc) <-
  c("control_cells", "event_cells")

ma_gc <- aggregate(cc, fact = 70)

sc <-
  setValues(ma_gc, rep(c(1, 0), 2, each = ncol(ma_gc))) *
  setValues(ma_gc, c(1, 0))

sc[sc == 0] <- NA

sb <- as.polygons(sc, dissolve = FALSE)

set.seed(1)

sbc <-
  map_df(
    .x = 1:50,
    function(sample_round) {

      set.seed(sample_round)

      ssb <- shift(sb, runif(1, -1, 1) * 30000, runif(1, -1, 1) * 30000)

      ssb <- mask(ssb, project(vect(amazon), crs(ssb)))

      table <- as_tibble(terra::extract(gc, ssb, xy = TRUE))

      ssb_sub <- table %>%
        group_by(ID) %>%
        summarise(
          across(.cols = c(control_cells, event_cells), ~ sum(.))
        ) %>%
        filter(control_cells >= 20, event_cells >= 20)

      ssb <- ssb %>%
        st_as_sf() %>%
        mutate(id = row_number(), rep = sample_round) %>%
        select(id, rep) %>%
        filter(id %in% ssb_sub$ID)

      return(ssb)

    }
  )

st_write_parquet(sbc, "data/samples.parquet")
