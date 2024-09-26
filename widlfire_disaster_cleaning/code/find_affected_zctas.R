# This script takes a ZCTA shapefile with 2010 ZCTAs, and the wildfire disaster
# dataset as of sept 10th, 2024, and uses that to produce a file containing
# a list of affected zctas, the disaster ID of the disaster which they were
# affected, the incident date, the year they were affected, and a column
# indicating if this was the first, second, third, etc. fire since 2000
# affecting that ZCTA

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(here)


# Read data ---------------------------------------------------------------

# wf disaster perimeters
load(here("raw_data", 'all_disasters_select_vars.rdata'))

# using milo's file
ca_fires <- all_disaster_perimeters_ics_and_news_buffers_conus_select_variables

# zctas load
zctas <- st_read(here("raw_data", "zcta_shapefile/tl_2018_us_zcta510.shp"))


# Do ----------------------------------------------------------------------

# filter to CA fires
ca_fires <-
  ca_fires |>
  filter(grepl("CA", disaster_list_states))

# rename disaster id for linkage
ca_fires <- ca_fires |> rename(disaster_id = disaster_id_clean)

# create unique id for each fire
ca_fires <- ca_fires |>
  mutate(u_num_id = seq(1:length(ca_fires$disaster_nested_id)))

# intersection to get which zctas were affected

# transform the CRS of zctas to match ca_fires
ca_fires_crs <- st_crs(ca_fires)
zctas_transformed <- st_transform(zctas, crs = ca_fires_crs)

af_zcta <- st_intersection(ca_fires, zctas_transformed)

# select relevant vars
af_zcta <- af_zcta |>
  select(disaster_id, u_num_id, zcta = ZCTA5CE10, aggregate_year)

# get incident date to order exposures to fire properly
af_zcta <- af_zcta |>
  mutate(incident_date = str_sub(disaster_id, start = 6, end = 15))

af_zcta <- af_zcta |> mutate(year = as.Date(incident_date, format = "%Y-%m-%d"))

# arrange by incident date
af_zcta <- af_zcta |> arrange(zcta, incident_date)

# order fire exposures
af_zcta <- af_zcta |>
  group_by(zcta) |>
  mutate(num_times_affected = row_number())

af_zcta <- af_zcta |> st_drop_geometry()
af_zcta <- af_zcta |> mutate(year = year(year))

# write 

write_rds(af_zcta, here("zctas_affected_by_disasters.rds"))




