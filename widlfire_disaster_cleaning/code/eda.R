# EDA

# libraries
library(tidyverse)
library(sf)
library(here)
library(arrow)


# read 
pop_dens <- read_csv(here('fire_pop_density_criteria.csv'))

ca_fires_lauren <- read_parquet(here("all_disaster_perimeters_buffers_conus_dist_select_vars.parquet"))

ca_fires_lauren <- 
  ca_fires_lauren |>
  filter(grepl("CA", aggregate_states))

ca_fires_lauren <- ca_fires_lauren %>% rename(disaster_id = disaster_id_clean) 

ca_fires_lauren <- ca_fires_lauren |> left_join(pop_dens)


# load 
load(here('all_disasters_select_vars.rdata'))

# using milo's file 
ca_fires_milo <- all_disaster_perimeters_ics_and_news_buffers_conus_select_variables

# filter to CA fires 
ca_fires_milo <- 
  ca_fires_milo |>
  filter(grepl("CA", disaster_list_states))

# name disaster ID vars consistently 
ca_fires_milo <- ca_fires_milo %>% rename(disaster_id = disaster_id_clean) 

ca_fires_milo <- ca_fires_milo |> left_join(pop_dens)

sum(is.na(ca_fires_milo$density_criteria_met))


sum(is.na(ca_fires_lauren$density_criteria_met))


k <- st_geometry(ca_fires)
plot(k)




ca_fires |> group_by(aggregate_year) |> summarize(n = n()) |> knitr::kable()

