# EDA

library(tidyverse)
library(sf)
library(here)

load("all_disasters_select_vars.rdata")

View(all_disaster_perimeters_ics_and_news_buffers_conus_select_variables[1:1000,])

pop_dens <- read_csv(here('fire_pop_density_criteria.csv'))


ca_fires <- read_parquet(here("all_disaster_perimeters_buffers_conus_dist_select_vars.parquet"))

ca_fires <- 
  all_disaster_perimeters_ics_and_news_buffers_conus_select_variables |>
  filter(grepl("CA", disaster_list_states))

ca_fires <- ca_fires %>% rename(disaster_id = disaster_id_clean)

k <- st_geometry(ca_fires)
plot(k)

ca_fires |> group_by(aggregate_year) |> summarize(n = n()) |> knitr::kable()

ca_fires <- ca_fires |> left_join(pop_dens)
