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
library(sfarrow)
library(ebal)
library(fs)
library(glue)
library(lubridate)
library(tidyverse)
#
# OPTIONS ---------------------------------------------------------------------
#
#
#
# LOAD FILES ------------------------------------------------------------------

## Load raster files ----

cc <- rast("data/control_cells.tif")

ec <- rast("data/event_cells.tif")

ff <- rast("data/fire_freq.tif")

fa <- rast("data/forest_area.tif")

gc <- c(cc, ec, ff, fa)

names(gc) <-
  c("control_cells", "event_cells", "fire_freq", "forest_area")

## Load GEDI sample data ----

gedi_sample <- st_read_parquet("data/gedi_04A/gedi_04A_1.parquet")

# ORGANIZE DATA ---------------------------------------------------------------

gedi_sample <- gedi_sample %>%
  mutate(
    time = as_date(floor_date(ymd_hms("2018-01-01 00:00:00") + time, "month"))
  ) %>%
  st_transform(crs(gc))

gc_sf <-
  terra::extract(gc, gedi_sample, cells = TRUE) %>%
  select(control_cells:cell)

gedi_sample <- gedi_sample %>%
  bind_cols(gc_sf) %>%
  as_tibble()

gedi_sample <- gedi_sample %>%
  filter(
    (control_cells == 1 & fire_freq == 0) |
      (event_cells == 1 & fire_freq == 1),
    beam %in% c("BEAM0101", "BEAM0110", "BEAM1000", "BEAM1011"),
    agbd >= 50, agbd <= 400
  )

gedi_sample <- gedi_sample %>%
  mutate(experiment_group = if_else(control_cells == 1, 0, 1)) %>%
  group_by(id, experiment_group, time, cell) %>%
  summarise(
    across(.cols = c(agbd, forest_area), .fns = ~ median(.)),
    .groups = "drop"
  )

# AGREGGATE GROUP VALUES ------------------------------------------------------

experiment_cells <- gedi_sample %>%
  filter(year(time) == "2019") %>%
  select(cell, id, experiment_group, agbd, forest_area) %>%
  group_by(id, experiment_group) %>%
  filter(n() > 5) %>%
  group_by(id) %>%
  filter(all(c(0, 1) %in% experiment_group))

cells_weights <-
  map_df(
    .x = unique(experiment_cells$id),
    function(sample_id) {

      experiment_sample <- experiment_cells %>%
        filter(id == sample_id)

      eb <-
        ebalance(
          experiment_sample$experiment_group,
          as.matrix(
            experiment_sample$agbd,
            experiment_sample$forest_area
          )
        )

      treat_sample <- experiment_sample %>%
        filter(experiment_group == 1) %>%
        mutate(weights = 1)

      control_sample <- experiment_sample %>%
        filter(experiment_group == 0) %>%
        mutate(weights = eb$w)

      experiment_sample <-
        bind_rows(treat_sample, control_sample) %>%
        select(cell, id, weights)

    }
  )

gedi_sample %>%
  inner_join(cells_weights, by = c("id", "cell")) %>%
  group_by(id, experiment_group, time) %>%
  summarise(agbd = weighted.mean(agbd, weights), .groups = "drop") %>%
  ggplot() +
  facet_wrap( ~ id) +
  geom_point(
    aes(
      x = time, y = agbd,
      color = factor(experiment_group)
    )
  )

