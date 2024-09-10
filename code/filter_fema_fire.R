# Code description -------------------------
# Author: Vivian
# Date: 09/10/2024
# Goal: Filter data from entire FEMA claims to wildfire specific

# Load libraries
library(dplyr)
library(tidyverse)
library(here)
library(vroom)

# Load FEMA data
fema_dta <- vroom(here("data", "01_raw", "IndividualsAndHouseholdsProgramValidRegistrations.csv"))

# Filter data to fires and save -------------------------
names(fema_dta)
dim(fema_dta) # 22 547 943

# focus on wildfires
wf_fema_dta <- fema_dta %>% 
  filter(incidentType == "Fire")

# save
write_csv(wf_fema_dta, here("data", "01_raw", "wf_fema_dta.csv"))










