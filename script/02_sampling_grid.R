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
amazon <- geobr::read_biomes(year = 2019) %>%
  filter(code_biome == 1)

# LOAD PRODUCTS ---------------------------------------------------------------

cc <- rast("data/control_cells.tif")

ec <- rast("data/event_cells.tif")

ff <- rast("data/fire_freq.tif")

fa <- rast("data/forest_area.tif")

pa <- cellSize(cc)

gc <- c(cc, ec, ff, fa, pa)

names(gc) <-
  c("control_cells", "event_cells", "fire_freq", "forest_area", "pixel_area")

ma_gc <- aggregate(cc, fact = 50)

sc <-
  setValues(ma_gc, rep(c(1, 0, 0), 3, each = ncol(ma_gc))) *
  setValues(ma_gc, c(1, 0, 0))

sc[sc == 0] <- NA

writeRaster(sc, "data/sampling_cells.tif", overwrite = TRUE)

sb <- as.polygons(sc, dissolve = FALSE)

table <- as_tibble(terra::extract(gc, sb, xy = TRUE))

sb_sub <- table %>%
  group_by(ID) %>%
  summarise(
    across(.cols = c(control_cells, event_cells), ~ sum(.)/n()),
    across(.cols = c(forest_area, pixel_area), ~ sum(.))
  ) %>%
  mutate(forest_fraction = forest_area / pixel_area) %>%
  filter(control_cells > 0.05, event_cells > 0.05, forest_fraction > 0.05)

sb %>%
  st_as_sf() %>%
  mutate(id = row_number()) %>%
  select(id) %>%
  filter(id %in% sb_sub$ID) %>%
  write_sf("data/samples.geojson", delete_dsn = TRUE)
