
#### Title: ZCTA analysis
#### Purpose: Compare demographic and FEMA claims across ZCTAs
#### Author: David Coomes
#### Date: 9/11/2024

library(tidyverse)
library(ggplot2)
library(sf)
library(here)
library(viridis) 

# Plan:
  # Choose a specific fire (or several)
  # Compare proportion

# Variables of interest
  # i_claim_suc: claim was successful
  # repairAmount: Amount of money for repair
  # replacementAmount: Amount of money for replacement
  # rpfvl: real property damage assessed by FEMA
  # ppfvl: personal property loss assessed by FEMA


# Download data
# FEMA claims data
fema_df <- read_csv(here("data", "02_processed", "ca_shape_w_suc_claims.csv") %>% str_remove("/code/census"))

fema_dis_df <- fema_df %>%
  group_by(disasterNumber) %>%
  add_tally(name = "num_claims_disaster") %>%
  summarize(pct_claim_success = mean(i_claim_suc, na.rm=TRUE),
            num_claims_disaster = mean(num_claims_disaster, na.rm=TRUE))


  
fema_zip_df <- fema_df %>%
  #mutate(disaster_year = format(as.Date(declarationDate, format="%d/%m/%Y"), "%Y")) %>%
  group_by(disasterNumber) %>%
  add_tally(name = "num_claims_disaster") %>%
  group_by(disasterNumber, ZCTA_USE) %>%
  add_tally(name = "num_claims_disaster_zip") %>%
  summarize(pct_claim_success = mean(i_claim_suc, na.rm=TRUE),
            num_claims_disaster = mean(num_claims_disaster, na.rm=TRUE),
            num_claims_disaster_zip = mean(num_claims_disaster_zip, na.rm=TRUE)) %>%
  ungroup() %>%
  arrange(num_claims_disaster, desc=TRUE)

# Adding in geometry data
map_zip_df <- geo_df %>%
  left_join(fema_zip_df %>%
              filter(!is.na(ZCTA_USE)),
            by = c("zcta" = "ZCTA_USE"))

map_zip_4353 <- map_zip_df %>%
  filter(disasterNumber == 4353)

map_zip_


# plot incident 4353
geo_df %>%
  ggplot() +
  geom_sf(fill="grey90") +
  geom_sf(data = map_zip_4353, aes(fill = pct_claim_success), linewidth= 0) +
  scale_fill_viridis(option="D") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12))





