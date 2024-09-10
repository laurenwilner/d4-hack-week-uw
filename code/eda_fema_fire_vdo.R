# Code description -------------------------
# Author: Vivian
# Date: 09/10/2024
# Goal: Filter data from entire FEMA claims to wildfire specific

# Load libraries
library(dplyr)
library(tidyverse)
library(here)
library(vroom)
library(ggplot2)
library(readr)
library(lubridate)

# Load d# Load d# Load data -----------------------
wf_fema_dta <- read_csv(here("project_code", "data", "interim", "wf_fema_dta.csv"))

# Prep data -----------------------
wf_fema_dta <- wf_fema_dta %>% 
  mutate(year = year(declarationDate))

# EDA -----------------------
names(wf_fema_dta)

###### number of disasters -----------------------
length(unique(wf_fema_dta$disasterNumber))

###### barplot for number of disasters per year
wf_fema_dta %>% 
  group_by(year) %>% 
  summarise(n_disasters = n_distinct(disasterNumber)) %>% 
  ggplot(aes(x = year, y = n_disasters)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Disasters per Year",
       x = "Year",
       y = "Number of Disasters") +
  scale_x_continuous(breaks = seq(min(wf_fema_dta$year), max(wf_fema_dta$year), by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

###### barplot for number of affected/damanged zips (damagedZipCode) per year
wf_fema_dta %>% 
  group_by(year) %>% 
  summarise(n_zips = n_distinct(damagedZipCode)) %>% 
  ggplot(aes(x = year, y = n_zips)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Affected/Damaged Zip Codes per Year",
       x = "Year",
       y = "Number of Zip Codes") + 
  scale_x_continuous(breaks = seq(min(wf_fema_dta$year), max(wf_fema_dta$year), by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

###### owner vs renter -----------------------
# check for distribution (including missing variables) for ownRent variable
table(wf_fema_dta$ownRent, useNA = "always")

# present the above data in a barplot by year, displaying all years on x axis
wf_fema_dta %>% 
  group_by(year, ownRent) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = year, y = n, fill = ownRent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Owner vs Renter per Year",
       x = "Year",
       y = "Number of Claims",
       fill = "Owner vs Renter") +
  scale_x_continuous(breaks = seq(min(wf_fema_dta$year), max(wf_fema_dta$year), by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

###### householdComposition -----------------------
table(wf_fema_dta$householdComposition, useNA = "always")

desired_order <- c("1", "2", "3", "4", "5", ">5")

wf_fema_dta %>% 
  mutate(householdComposition = factor(householdComposition, levels = desired_order)) %>%
  group_by(year, householdComposition) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  complete(year, householdComposition, fill = list(n = 0)) %>% 
  ggplot(aes(x = year, y = n, fill = householdComposition)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Owner vs Renter per Year",
       x = "Year",
       y = "Number of Claims",
       fill = "Owner vs Renter") +
  scale_x_continuous(breaks = seq(min(wf_fema_dta$year), max(wf_fema_dta$year), by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

###### grossIncome -----------------------
table(wf_fema_dta$grossIncome, useNA = "always")







