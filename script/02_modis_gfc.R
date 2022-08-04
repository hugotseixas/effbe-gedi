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
library(rgee)
library(terra)
library(sf)
library(geojsonsf)
library(geobr)
library(googledrive)
library(glue)
library(fs)
library(magrittr)
library(tidyverse)
#
# OPTIONS ---------------------------------------------------------------------
#
ee_Initialize(email = "h234184@dac.unicamp.br", drive = TRUE)
#
# SET GEOGRAPHIC AND TEMPORAL EXTENT ------------------------------------------

# Load amazon biome limits
amazon <- geobr::read_biomes(year = 2019) %>%
  filter(code_biome == 1) %>%
  st_transform(crs = "EPSG:4326")

# Set bounding box
aoi <- st_as_sfc(st_bbox(amazon))

# Load geometries to earth engine
amazon <- sf_as_ee(amazon) # Can take some minutes to import the polygon
aoi <- sf_as_ee(aoi) # Can take some minutes to import the polygon

# LOAD PRODUCTS ---------------------------------------------------------------

## Burned areas ----

burned_area <-
  ee$ImageCollection("MODIS/061/MCD64A1")$
  filterBounds(aoi)$
  select("BurnDate")

burned_2020 <- burned_area$
  filterDate('2020-01-01', '2020-12-31')$
  reduce(ee$Reducer$anyNonZero())

burned_2021 <- burned_area$
  filterDate('2021-01-01', '2021-12-31')$
  reduce(ee$Reducer$anyNonZero())$
  unmask()

anti_2021 <- burned_2020$gt(burned_2021)$selfMask()

## Global forest cover ----

gfc <- ee$Image("UMD/hansen/global_forest_change_2021_v1_9")

cover_2000 <- gfc$select("treecover2000")$gt(90)

loss <- gfc$select("lossyear")$selfMask()$gt(0)$unmask(0)

cover_2021 <- cover_2000$subtract(loss)$selfMask()

burned_cover <- anti_2021$eq(cover_2021)

patchsize <- burned_cover$connectedPixelCount(1024)

patchsizeLarge = patchsize$gt(10)

Map$addLayer(patchsizeLarge$reproject(gfc$projection())$clip(aoi))
