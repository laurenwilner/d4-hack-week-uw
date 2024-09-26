# Descriptives of the zctas affected file 

library(tidyverse)
library(here)
library(sf)
library(ggthemes)

zctas <- st_read(here("raw_data", "zcta_shapefile/tl_2018_us_zcta510.shp"))
zctas <- zctas %>% filter(as.numeric(ZCTA5CE10) >= 90001 & as.numeric(ZCTA5CE10) <= 96162)

wf_data <- readRDS(here("zctas_affected_by_disasters.rds"))
wf_data <- wf_data %>% select(num_times_affected)
wf_data <- wf_data %>% group_by(zcta) %>% summarize(times_affected = max(num_times_affected))

zctas <- zctas %>% select(zcta = ZCTA5CE10)

zctas <- zctas %>% left_join(wf_data)
zctas <- zctas %>% mutate_all(~replace_na(., 0))


variable_range <- range(zctas$times_affected, na.rm = TRUE)

l <- zctas %>% ggplot() + geom_sf(aes(fill = times_affected), color = NA) + theme_minimal() +
  scale_fill_viridis_c(
    limits = c(0, 14),  # Set the limits to include the range of interest
    breaks = c(0, 7, 14)  # Set specific breaks for the legend
  ) +# Apply viridis color scale
  theme_minimal() +
  ggtitle("Number of wildfire disasters affecting each 
ZCTA in California, 2000-2019") +
  theme(
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.text = element_blank()    # Remove axis text
  ) +
  labs(fill = "Number of wildfire disasters 
affecting each ZCTA") +
  theme_map() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Make the title bigger and bold
    legend.text = element_text(size = 15),  # Make the legend text bigger
    legend.title = element_text(size = 15)  # Make the legend title bigger
  )

  

ggsave(l, filename = "/Users/heathermcbrien/Documents/Documents_1/wildfire_disaster_data_cleaning.nosync/figs/times_affected.pdf")
