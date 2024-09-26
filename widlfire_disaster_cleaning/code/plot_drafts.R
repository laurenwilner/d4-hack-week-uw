# This script generates plots for the hack week presentation 

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(here)
library(MetBrewer)

# Read data ---------------------------------------------------------------

# wf disaster perimeters
load(here("raw_data", 'all_disasters_select_vars.rdata'))

# using milo's file
ca_fires <- all_disaster_perimeters_ics_and_news_buffers_conus_select_variables

# load zcta boundaries 
zctas <- st_read(here("raw_data", "zcta_shapefile/tl_2018_us_zcta510.shp"))
zctas <- zctas %>%
  filter(as.numeric(ZCTA5CE10) >= 90001 &
           as.numeric(ZCTA5CE10) <= 96162)

# load county boundaries
counties <- 
  st_read(here("raw_data", "tl_2019_06_cousub", "tl_2019_06_cousub.shp"))
c_bds <- st_geometry(counties)

# Do ----------------------------------------------------------------------

# filter to CA fires, create different datasets
ca_fires <-
  ca_fires |>
  filter(grepl("CA", disaster_list_states))

fema_fires <- ca_fires %>%
  filter(!is.na(fema_declaration_string)) 

fema_fires_post_2010 <- ca_fires %>% filter(aggregate_year >= 2010)

ca_fires_post_2010 <- ca_fires %>% filter(aggregate_year >= 2010)

# Do plots ----------------------------------------------------------------

# plot all disaster fires by year
ca_fires_post_2010_pt <- ca_fires_post_2010 %>%
  mutate(year = as.factor(aggregate_year))

palette <- met.brewer("Tam", n=10, type="continuous")

# plot all fires by year 
l <-
  ggplot() +
  geom_sf(data = c_bds, color = "black", size = 0.0001, fill = NA) + 
  geom_sf(data = ca_fires_post_2010_pt, aes(fill = year), size = 0.1) + 
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.text = element_blank()    # Remove axis text
  ) +
  scale_fill_manual(values = palette) +  
  ggtitle("Wildfire disasters affecting California by year, 2010-2019") +
  labs(fill = "Year of disaster") +
  theme_map()

ggsave(l, filename = here('figs', 'ca_fires_post_2010_by_year.pdf'))

# plot the number of civilian fatalities by fire 
ca_fires_fatal <- ca_fires_post_2010 %>%
  filter(disaster_civil_fatalities_max > 0)

palette <- rev(met.brewer("Tam", n=30, type="continuous"))

l <-
  ggplot() +
  geom_sf(data = c_bds, color = "black", size = 0.0001, fill = NA) +
  geom_sf(data = ca_fires_fatal,
          aes(fill = disaster_civil_fatalities_max),
          size = 0) +
  theme_minimal() +
  scale_fill_gradientn(colors = palette) +  
  theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  ggtitle("California wildfire dsasters causing civilan fatalities, 2010-2019") +
  labs(fill = "Number of fatalities")        
          
ggsave(l, filename = here('figs', 'civ_fatal_post_2010.pdf'))

# plot the number of structures burned by fire 
structures_burned <- as.numeric(ca_fires_post_2010$disaster_struct_dest_max)
ca_fires_structures <- ca_fires_post_2010 %>%
  mutate(structures_burned = as.numeric(disaster_struct_dest_max))

palette <- rev(met.brewer("Tam", n=30, type="continuous"))

l <-
  ggplot() +
  geom_sf(data = c_bds, color = "black", size = 0.0001, fill = NA) +
  geom_sf(data = ca_fires_structures,
          aes(fill = disaster_struct_dest_max),
          size = 0) +
  theme_minimal() +
  scale_fill_manual(values = palette) +  
  theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  ggtitle("California wildfire dsasters causing civilan fatalities, 2010-2019") +
  labs(fill = "Number of fatalities")        

ggsave(l, filename = here('figs', 'structures_post_2010.pdf'))

# plot fires by year that had fema declarations 
l <- 
  ggplot() +
  geom_sf(data = c_bds, color = "black", size = 0.001) + 
  geom_sf(data = fema_fires_post_2010, fill = 'red', color = "red") +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.text = element_blank()    # Remove axis text
  ) +
  facet_wrap(~aggregate_year)

ggsave(l, filename = here('fema_fires_post_2010_by_year.pdf'))






# plot all wildfire disasters in california 2010-2019


l <-
  ggplot() +
  geom_sf(
    data = c_bds,
    color = "black",
    size = 0.0001,
    fill = NA
  ) +
  geom_sf(
    data = ca_fires_post_2010,
    size = 0,
    fill = 'red',
    color = 'red'
  ) + # Use aes(geometry = geometry) to specify the geometry column
  theme_minimal() +
  scale_fill_viridis_c() +  # Set specific breaks for the legend +
  theme(axis.ticks = element_blank(), # Remove axis ticks
        axis.text = element_blank()    # Remove axis text)
        
ggsave(l, filename = here('all_disaster_fires_post_2010.pdf'))


# sort fires by color for year 
# structures destroyed 

l <- fema_fires %>% st_geometry() %>% ggplot() + geom_sf() + theme_minimal() +
  facet_wrap(~aggregate_year)

# rename disaster id for linkage
ca_fires <- ca_fires |> rename(disaster_id = disaster_id_clean)

k <- ca_fires %>% st_geometry() %>% ggplot() + geom_sf() + theme_minimal()


k1 <- ca_fires %>% st_geometry() %>% ggplot(aes(fill)) + geom_sf() + theme_minimal()

