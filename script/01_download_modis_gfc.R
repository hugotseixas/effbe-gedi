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
ee_Initialize(user = "h234184@dac.unicamp.br", drive = TRUE)
#
# SET GEOGRAPHIC EXTENT -------------------------------------------------------

# Load amazon biome limits
amazon <- geobr::read_biomes(year = 2019) %>%
  filter(code_biome == 1) %>%
  st_transform(crs = "EPSG:4326")

# Set bounding box
aoi <- st_as_sfc(st_bbox(amazon))

# Load geometries to earth engine
ee_amazon <- sf_as_ee(amazon) # Can take some minutes to import the polygon
ee_aoi <- sf_as_ee(aoi) # Can take some minutes to import the polygon

# LOAD PRODUCTS ---------------------------------------------------------------

## Burned areas ----

# Load MODIS burned areas product and select burn date band
fire <-
  ee$ImageCollection("MODIS/061/MCD64A1")$
  filterBounds(ee_aoi)$
  select("BurnDate")

# Set projection and scales for the project
proj <- fire$first()$projection()
scale <- proj$nominalScale()

# Get burn frequency
fire_freq <- fire$
  map(function(image) { return(image$gt(0)) })$
  reduce(ee$Reducer$sum())

# Filter area that burned in 2020
fire_2020 <- fire$
  filterDate('2020-01-01', '2020-12-31')$
  reduce(ee$Reducer$anyNonZero())$
  unmask()

# Filter areas that burned in 2021
fire_2021 <- fire$
  filterDate('2021-01-01', '2021-12-31')$
  reduce(ee$Reducer$anyNonZero())$
  unmask()$
  # Create buffer to avoid neighborhood with areas that burned in 2021
  focalMax(radius = 5)

# Filter areas that burned in 2020 but not in 2021
anti_2021 <- fire_2020$gt(fire_2021)$selfMask()

## Global forest cover ----

# Load global forest change data
gfc <- ee$Image("UMD/hansen/global_forest_change_2021_v1_9")

# Select tree cover in 2020 and filter pixels with more than 90% tree cover
forest_2000 <- gfc$select("treecover2000")$gt(90)

# Get forest cover losses for all time series
forest_loss <- gfc$select("lossyear")$gt(0)$unmask(0)

# Get remaining cover at 2021
forest_2021 <- forest_2000$gt(forest_loss)$
  reduceResolution(ee$Reducer$mode(), maxPixels = 5000)$
  reproject(crs = proj, scale = scale)

# Get forest area inside MODIS pixel
forest_area <-
  forest_2000$gt(loss)$
  reduceResolution(ee$Reducer$mean(), maxPixels = 5000)$
  multiply(ee$Image$pixelArea())

#Filter burned pixels
# Get all forests that burned in 2020 but not in 2021
fire_event <- forest_2021$eq(anti_2021)

# Filter pixels that did not burn in 2020 and 2021
# Avoid neighbors with pixels that burned in 2020/2021
fire_control <- forest_2021$
  gt(fire_2020$unmask()$focalMax(radius = 5))$
  gt(fire_2021$unmask())

# DOWNLOAD DATA ---------------------------------------------------------------

ee_as_raster(
  image = fire_event,
  dsn = "data/event_cells.tif",
  region = ee_aoi,
  scale = scale,
  crs = proj
)

ee_as_raster(
  image = fire_control,
  dsn = "data/control_cells.tif",
  region = ee_aoi,
  scale = scale,
  crs = proj
)

ee_as_raster(
  image = forest_area,
  dsn = "data/forest_area.tif",
  region = ee_aoi,
  scale = scale,
  crs = proj
)

ee_as_raster(
  image = fire_freq$toInt(),
  dsn = "data/fire_freq.tif",
  region = ee_aoi,
  scale = scale,
  crs = proj
)

ee_imagecollection_to_local(
  ic = fire,
  dsn = "data/fire.tif",
  region = ee_aoi,
  scale = scale,
  crs = proj
)

ragg <- terra::aggregate(cellSize(r), fact = 20, fun = "sum", na.rm = TRUE)

fragg <- ragg/cellSize(ragg)

fragg[fragg < 0.1] <- 0
fragg[fragg >= 0.1] <- 1

writeRaster(cellSize(r), "r.tif")

c <- ee_as_raster(
  cover_area,
  region = ee_aoi,
  scale = 500
)

c <- rast(c)

cagg <- terra::aggregate(c, fact = 20, fun = "sum", na.rm = TRUE)

fcagg <- cagg/cellSize(cagg)

fcagg[fcagg < 0.1] <- 0
fcagg[fcagg >= 0.1] <- 1

plot(fcagg*fragg)

h_grid <- setValues(aggregate(fcagg, 4, sum), rep(0:1, 2, each = ncol(grid)))
v_grid <- setValues(aggregate(fcagg, 4, sum), 0:1)
grid <- v_grid * h_grid
grid <- project(grid, fcagg)
