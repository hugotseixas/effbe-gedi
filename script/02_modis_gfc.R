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
burned_area <-
  ee$ImageCollection("MODIS/061/MCD64A1")$
  filterBounds(ee_aoi)$
  select("BurnDate")

# Get burn frequency
burn_freq <- burned_area$
  map(function(image) { return(image$gt(0)) })$
  reduce(ee$Reducer$sum())

# Filter area that burned in 2020
burned_2020 <- burned_area$
  filterDate('2020-01-01', '2020-12-31')$
  reduce(ee$Reducer$anyNonZero())

# Filter areas that burned in 2021
burned_2021 <- burned_area$
  filterDate('2021-01-01', '2021-12-31')$
  reduce(ee$Reducer$anyNonZero())$
  unmask()$
  # Create buffer to avoid neighborhood with areas that burned in 2021
  focalMax(radius = 5)

# Filter areas that burned in 2020 but not in 2021
anti_2021 <- burned_2020$gt(burned_2021)$selfMask()

## Global forest cover ----

# Load global forest change data
gfc <- ee$Image("UMD/hansen/global_forest_change_2021_v1_9")

# Select tree cover in 2020 and filter pixels with more than 90% tree cover
cover_2000 <- gfc$select("treecover2000")$gt(90)

# Get forest cover losses for all time serie
loss <- gfc$select("lossyear")$selfMask()$gt(0)$unmask(0)

# Get remaining cover at 2021
cover_2021 <- cover_2000$subtract(loss)$selfMask()

### Filter burned pixels ----

# Get all forests that burned in 2020 but not in 2021
burned_cover <- anti_2021$eq(cover_2021)

# Avoid small patches of burned areas
patchsize <- burned_cover$connectedPixelCount(1024)
exp_patch <- patchsize$gt(10)

### Filter pixels that did not burn in 2020 and 2021

# Avoid neighbors with pixels that burned in 2020/2021
unburned_cover <- cover_2021$
  gt(burned_2020$unmask(0)$focalMax(radius = 5))$
  gt(burned_2021$unmask(0))

# CREATE SAMPLING GRID --------------------------------------------------------

create_sample <-
  function(proj, region, seed, strict, group) {

    values <- ee$FeatureCollection(ee$Feature(NULL, NULL))$
      randomColumn('x', seed)$
      randomColumn('y', seed)$
      first()

    proj <- proj$translate(values$get("x"), values$get("y"))

    initial_grid <- ee$Image$
      random(seed)$
      multiply(1000000)$
      int()

    mask <- ee$Image$pixelCoordinates(proj)$
      expression("!((b('x') + 0.5) % 2 != 0 || (b('y') + 0.5) % 2 != 0)")

    final_grid <- initial_grid$updateMask(mask)$reproject(proj)

    cells <- ee$Image(ee$Algorithms$If(strict, final_grid, initial_grid))$
      clip(region)$
      reproject(proj)

    random <- ee$Image$
      random(seed)$
      multiply(1000000)$
      int()$
      reproject(proj$atScale(10000))

    maximum <- cells$
      addBands(random)$
      reduceConnectedComponents(ee$Reducer$max())

    points <- random$eq(maximum)$selfMask()

  }

ctr_sample <-
  create_sample(
    proj = ee$Projection(burned_area$first()$projection())$atScale(100000),
    region = ee_amazon,
    seed = 2,
    strict = TRUE,
    group = unburned_cover
  )

samples <- ctr_sample$
  reduceToVectors(
    reducer = ee$Reducer$countEvery(),
    geometry = ee_amazon,
    crs = ee$Projection(burned_area$first()$projection())$atScale(10000),
    geometryType = 'bb',
    maxPixels = 1e9
  )

sample_sf <- rgee::ee_as_sf(samples)

write_sf(sample_sf, "data/sample.geojson")

mapsf::mf_theme(bg = "transparent")
mapsf::mf_export(x = amazon, filename = "manuscript/exp_dsn.svg", width = 11)
mapsf::mf_map(amazon)
mapsf::mf_map(sample_sf, add = TRUE, col = "black")
mapsf::mf_inset_on(x = sample_sf[c(1),], pos = "bottomright", cex = .2)
mapsf::mf_init(sample_sf[c(50),], expandBB = rep(10, 10))
mapsf::mf_map(amazon, add = TRUE)
mapsf::mf_map(sample_sf, add = TRUE, col = "black")
box()
mapsf::mf_inset_off()
dev.off()
