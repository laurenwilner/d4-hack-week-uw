
####
####
####
####


library(tidyverse)
library(ggplot2)
library(sf)
library(here)
library(janitor)
library(tidycensus)
library(tigris)
library(sp)
library(ggthemes)
library(ggpubr)

options(tigris_use_cache = TRUE)

# Load data
load(here("data", "all_disasters_select_vars.rdata"))

ca_wf <- all_disaster_perimeters_ics_and_news_buffers_conus_select_variables %>%
  filter(disaster_list_states == "CA") %>%
  mutate(acres = area_sq_m * 0.000247105,
         structures_burned = as.numeric(disaster_struct_dest_max))

out <- over(ca_wf, zcta_shape)


# Loading zcta flat file
ca_zcta <- read_csv(here("data", "zctas_affected_by_disasters.csv")) %>%
  group_by(zcta) %>%
  summarize(num_times_affected = max(num_times_affected, na.rm=TRUE))

zcta_shape <- zctas(year = 2010, state = "CA") %>%
  mutate(zcta = as.numeric(ZCTA5CE10))

ca_zcta <- zcta_shape %>%
  full_join(ca_zcta, by = "zcta")

ca_zcta <- ca_zcta %>%
  mutate(num_times_affected = ifelse(is.na(num_times_affected), 0, num_times_affected))

zcta_shape %>%
  ggplot() +
  geom_sf()

#bringing in claims data
claims_df <- read_csv(here("data", "ca_shape_w_suc_claims.csv")) %>%
  group_by(GEOID10) %>%
  summarize(num_successful_claims = n(),
            pct_successful_claims = mean(i_claim_suc, na.rm=TRUE))

ca_zcta_claims <- zcta_shape %>%
  full_join(claims_df, by = c("zcta" = "GEOID10"))




# getting some descriptive statistics
num_civ_fatalities <- sum(ca_wf$disaster_civil_fatalities_max, na.rm=TRUE)
num_structures_lost <- sum(as.numeric(ca_wf$disaster_struct_dest_max), na.rm=TRUE)      # this isn't accurate
num_acres_burned <- sum(ca_wf$acres, na.rm=TRUE)

# getting the number of acres burned per year
acres_per_year <- ca_wf %>%
  group_by(aggregate_year) %>%
  summarize(acres_burned = sum(acres, na.rm=TRUE),
            num_deaths = sum(disaster_civil_fatalities_max, na.rm = TRUE)) %>% 
  st_drop_geometry()

# outputting bar plot
acres_per_year %>%
  ggplot() +
  geom_bar(mapping = aes(x=aggregate_year, y=acres_burned), stat="identity") +
  geom_point(aes(x=aggregate_year, y = num_deaths*10000), color="red") +
  scale_y_continuous("Total acres burned", labels = label_comma(),
                     sec.axis = sec_axis(trans = ~./10000, name="Number of civilian deaths")) +
  theme_classic() +
  labs(x = "Year", y = "Total acres burned")

# getting map of number of wildfires per zcta
num_wildfires <- ca_zcta %>%
  ggplot() +
  geom_sf(aes(fill = num_times_affected), lwd = 0.01) + 
  theme_minimal() +
  scale_fill_viridis_c(
    limits = c(0, 14),  # Set the limits to include the range of interest
    breaks = c(0, 7, 14)  # Set specific breaks for the legend
  ) +# Apply viridis color scale
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.text = element_blank()    # Remove axis text
  ) +
  labs(fill = "Number of wildfire \ndisasters affecting \neach ZCTA") +
  theme_map() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Make the title bigger and bold
    legend.text = element_text(size = 8),  # Make the legend text bigger
    legend.title = element_text(size = 8),  # Make the legend title bigger
    legend.position.inside = c(0, 0.1))
  
prop_claims <- ca_zcta_claims %>%
  ggplot() +
  geom_sf(aes(fill = pct_successful_claims), lwd = 0.01) + 
  theme_minimal() +
  scale_fill_viridis_c(
    # limits = c(0, 14),  # Set the limits to include the range of interest
    # breaks = c(0, 7, 14)  # Set specific breaks for the legend
  ) +# Apply viridis color scale
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.text = element_blank()    # Remove axis text
  ) +
  labs(fill = "Proportion of \nsuccessful \nwildfire claims") +
  theme_map() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Make the title bigger and bold
    legend.text = element_text(size = 8),  # Make the legend text bigger
    legend.title = element_text(size = 8),  # Make the legend title bigger
    legend.position.inside = c(0, 0.1))


# ca_zcta_claims %>%
#   ggplot() +
#   geom_sf(aes(fill = num_successful_claims), lwd = 0.01) + 
#   theme_minimal() +
#   scale_fill_viridis_c(
#     # limits = c(0, 14),  # Set the limits to include the range of interest
#     # breaks = c(0, 7, 14)  # Set specific breaks for the legend
#   ) +# Apply viridis color scale
#   theme_minimal() +
#   theme(
#     axis.ticks = element_blank(),  # Remove axis ticks
#     axis.text = element_blank()    # Remove axis text
#   ) +
#   labs(fill = "Number of successful \nwildfire claims") +
#   theme_map() +
#   theme(
#     plot.title = element_text(size = 20, face = "bold"),  # Make the title bigger and bold
#     legend.text = element_text(size = 12),  # Make the legend text bigger
#     legend.title = element_text(size = 12),  # Make the legend title bigger
#     legend.position.inside = c(-0.1, 0.1))


fig_2 <- ggarrange(num_wildfires, prop_claims, labels = c("A", "B"))





