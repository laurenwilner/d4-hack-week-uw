# Code description -------------------------
# Author: Vivian
# Date: 09/11/2024
# Goal: Create summary maps of demographics/characteristics data from California FEMA wildfire claims

# Load libraries
# setup
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}
pacman::p_load(dplyr, tidyverse, arrow, vroom, here, readr, lubridate, ggplot2, sf, readxl, MetBrewer, ggthemes)

# Set notation to be long
options(scipen = 999)

# Load data -------------------------
my_root <-"~/Users/vivian/Desktop/0_PhD/0_Research Projects/0_repos/d4-hack-week-uw/data/"
path_in <- "01_raw/"
path_plot <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/uw-d4hackweek_fema-eda/project_code/output/figures/"

# read xwalk zip-zcta
ca_xwalk_zip_zcta <- read_excel("/Users/vivian/Desktop/0_PhD/0_Research Projects/uw-d4hackweek_fema-eda/project_code/data/raw/ZIPCodetoZCTACrosswalk2010UDS.xls") %>% 
  filter(StateAbbr == "CA") %>% 
  mutate(ZIP = as.numeric(ZIP))

# fema wildfire data
# df <- read_parquet(paste0(path, "wf_fema_dta_keep_vars.parquet"))
df <-
  read_csv(here("project_code", "data", "interim", "wf_fema_dta.csv")) %>%
  mutate(year = year(declarationDate)) %>%
  filter(damagedStateAbbreviation == "CA") %>%
  filter(year(declarationDate) %in% c(2010:2021)) %>% 
  select(
    "disasterNumber",
    "declarationDate",
    "damagedStateAbbreviation",
    "damagedZipCode",
    "applicantAge",
    "householdComposition",
    "occupantsUnderTwo",
    "occupants2to5",
    "occupants6to18",
    "occupants19to64",
    "occupants65andOver",
    "grossIncome",
    "ownRent",
    "primaryResidence",
    "residenceType",
    "homeOwnersInsurance",
    "registrationMethod",
    "ihpEligible",
    "ihpAmount",
    "haAmount",
    "haEligible",
    "onaEligible",
    "onaAmount",
    "utilitiesOut",
    "habitabilityRepairsRequired",
    "rpfvl",
    "ppfvl",
    "renterDamageLevel",
    "destroyed",
    "rentalAssistanceEligible",
    "rentalAssistanceAmount",
    "repairAssistanceEligible",
    "repairAmount",
    "replacementAssistanceEligible",
    "replacementAmount",
    "personalPropertyEligible",
    "personalPropertyAmount",
    "ihpMax",
    "haMax",
    "onaMax",
    "id",
    "inspnIssued", # added on later by vivian to eval whether this is related to NA rpfvl and ppfvl
    "inspnReturned"
  ) %>% 
  mutate(damagedZipCode = as.numeric(damagedZipCode)) %>% 
  left_join(., ca_xwalk_zip_zcta, by = c("damagedZipCode" = "ZIP"))

# california shapefile
us_shape <- st_read("/Users/vivian/Desktop/0_PhD/0_Research Projects/uw-d4hackweek_fema-eda/project_code/data/raw/US_zcta_2010.shp") %>% 
  mutate(ZCTA5CE10 = as.numeric(ZCTA5CE10))

# Wrangle California shapefile ------------------------------------------------
# filter to california
ca_shape <- us_shape %>% 
  filter(ZCTA5CE10 %in% ca_xwalk_zip_zcta$ZCTA_USE)

# link ca_shape and df
ca_shape <- ca_shape %>% 
  left_join(df, by = c("ZCTA5CE10" = "damagedZipCode"))

# re-project to albers equal
ca_shape <- st_transform(ca_shape, crs = 5070)

# output data
ca_shape_w_suc_claims <- ca_shape %>% 
  mutate(i_claim_suc = case_when(rpfvl + ppfvl > 0 ~ 1,
                                 TRUE ~ 0)) 

# write_csv(ca_shape_w_suc_claims, "ca_shape_w_suc_claims.csv")

table(ca_shape_w_suc_claims$declarationDate)


# Summary vars -------------------------
# number of claims filed, successful claims per zcta, % success
ca_shape_w_suc_claims_zcta <- ca_shape_w_suc_claims %>% 
  group_by(ZCTA5CE10) %>% 
  mutate(n_claims = n(),
         n_claims_suc = sum(i_claim_suc == 1),
         p_claims_suc = n_claims_suc/n_claims) %>% 
  filter(row_number() == 1)

summary(ca_shape_claims$p_claims_suc)

# # is rpvl and ppvl related to inspection (issues and returned)??? - yes
# test <- ca_shape %>% 
#   group_by(ZCTA5CE10) %>% 
#   mutate(n_claims = n(),
#          n_claims_suc = sum(rpfvl + ppfvl > 0),
#          p_claims_suc = n_claims_suc/n_claims) %>% 
#   # filter(is.na(p_claims_suc)) %>% 
#   select(ZCTA5CE10, rpfvl, ppfvl, inspnIssued, inspnReturned, n_claims, n_claims_suc, p_claims_suc) %>% 
#   filter(!is.na(rpfvl), is.na(inspnIssued))

# plot number of claims filed per zcta
p <- ggplot(ca_shape_w_suc_claims_zcta) +
  geom_sf(aes(fill = n_claims), color = NA, size = 0.2) + 
  scale_fill_met_c("Derain", na.value = "grey", direction = -1) + 
  theme_minimal() +
  labs(title = "Number of filed wildfire-related claims",
       fill = "") +
  theme_map()

ggsave(filename = paste0(path_plot, "n_claims_by_zcta.png"), plot = p, width = 10, height = 8)

# plot number of successful claims per zcta
p <- ggplot(ca_shape_w_suc_claims_zcta) +
  geom_sf(aes(fill = n_claims_suc), color = NA, size = 0.2) + 
  scale_fill_met_c("Derain", na.value = "grey", direction = -1) + 
  theme_minimal() +
  labs(title = "Number of successful wildfire-related claims",
       fill = "") +
  theme_map()

ggsave(filename = paste0(path_plot, "n_claims_suc_by_zcta.png"), plot = p, width = 10, height = 8)

# plot % successful claims per zcta
p <- ggplot(ca_shape_w_suc_claims_zcta) +
  geom_sf(aes(fill = p_claims_suc), color = NA, size = 0.2) + 
  scale_fill_met_c("Derain", na.value = "grey", direction = -1) + 
  theme_minimal() +
  labs(title = "Percentage of successful wildfire-related claims",
       fill = "") +
  theme_map()

ggsave(filename = paste0(path_plot, "p_claims_suc_by_zcta.png"), plot = p, width = 10, height = 8)


# Ordinal vars -------------------------
# grossIncome, householdComposition, applicantAge
ca_shape <- ca_shape %>% 
  mutate(grossIncome2 = case_when(grossIncome == "0" ~ 0,
                                 grossIncome == "<$15,000" ~ 1,
                                 grossIncome == "$15,000-$30,000" ~ 2,
                                 grossIncome == "$30,001-$60,000" ~ 3,
                                 grossIncome == "$60,001-$120,000" ~ 4,
                                 grossIncome == "$120,001-$175,000" ~ 5,
                                 grossIncome == ">$175,000" ~ 6,
                                 TRUE ~ NA_real_),
         householdComposition2 = case_when(householdComposition == "1" ~ 1,
                                           householdComposition == "2" ~ 2,
                                           householdComposition == "3" ~ 3,
                                           householdComposition == "4" ~ 4,
                                           householdComposition == "5" ~ 5,
                                           householdComposition == ">5" ~ 6,
                                           TRUE ~ NA_real_),
         applicantAge2 = case_when(applicantAge == "<19" ~ 1,
                                   applicantAge == "19-34" ~ 2,
                                   applicantAge == "35-49" ~ 3,
                                   applicantAge == "50-64" ~ 4,
                                   applicantAge == "65+" ~ 5,
                                   TRUE ~ NA_real_))

ca_shape_inc_comp_age <- ca_shape %>% 
  group_by(ZCTA5CE10) %>% 
  mutate(m_grossIncome = mean(grossIncome2, na.rm = TRUE),
         m_householdComposition = mean(householdComposition2, na.rm = TRUE),
         m_applicantAge = mean(applicantAge2, na.rm = TRUE)) %>% 
  filter(row_number() == 1)

##### Plot ordinal vars (relative scale) -------------------------
# create map of grossIncome
p <- ggplot(ca_shape_inc_comp_age) +
  geom_sf(aes(fill = m_grossIncome), color = NA, size = 0.2) + 
  scale_fill_met_c("Derain", na.value = "grey", direction = -1) + 
  theme_minimal() +
  labs(title = "Gross Income by ZCTA",
       fill = "Gross Income") +
  theme_map()

ggsave(filename = paste0(path_plot, "grossIncome_by_zcta.png"), plot = p, width = 10, height = 8)

# create map of household composition
p <- ggplot(ca_shape_inc_comp_age) +
  geom_sf(aes(fill = m_householdComposition), color = NA, size = 0.2) + 
  scale_fill_met_c("Derain", na.value = "grey", direction = -1) + 
  theme_minimal() +
  labs(title = "Household Composition by ZCTA",
       fill = "Household Composition") +
  theme_map()

ggsave(filename = paste0(path_plot, "householdComposition_by_zcta.png"), plot = p, width = 10, height = 8)

# create map of applicant age
p <- ggplot(ca_shape_inc_comp_age) +
  geom_sf(aes(fill = m_applicantAge), color = NA, size = 0.2) + 
  scale_fill_met_c("Derain", na.value = "grey", direction = -1) + 
  theme_minimal() +
  labs(title = "Applicant Age by ZCTA",
       fill = "Applicant Age") +
  theme_map()

ggsave(filename = paste0(path_plot, "applicantAge_by_zcta.png"), plot = p, width = 10, height = 8)

# Boolean vars --------------------------------------------------------

# eval the responses of boolean variables
var_boolean <-
  c(
    "ownRent",
    "primaryResidence",
    "homeOwnersInsurance",
    "ihpEligible",
    "haEligible",
    "onaEligible",
    "utilitiesOut",
    "habitabilityRepairsRequired",
    "destroyed",
    "repairAssistanceEligible",
    "replacementAssistanceEligible",
    "personalPropertyEligible",
    "ihpMax",
    "haMax",
    "onaMax"
  )

# explore the unique values of each boolean variable
unique_values_list <- lapply(var_boolean, function(var) unique(ca_shape[[var]]))

names(unique_values_list) <- var_boolean
print(unique_values_list)

# count number of "yes" to each response
# calc the proportion of "yes" given total non-NA responses
ca_shape_binary <- ca_shape %>%
  mutate(ownRent = case_when(ownRent == "Owner" ~ 1, ownRent == "Renter" ~ 0)) %>% 
  group_by(ZCTA5CE10) %>% 
  mutate(
    n_ownRent = sum(ownRent == 1),
    n_primaryResidence = sum(primaryResidence == 1),
    n_homeOwnersInsurance = sum(homeOwnersInsurance == 1),
    n_ihpEligible = sum(ihpEligible == 1),
    n_haEligible = sum(haEligible == 1),
    n_onaEligible = sum(onaEligible == 1),
    n_utilitiesOut = sum(utilitiesOut == 1),
    n_habitabilityRepairsRequired = sum(habitabilityRepairsRequired == 1),
    n_destroyed = sum(destroyed == 1),
    n_rentalAssistanceEligible = sum(rentalAssistanceEligible == 1),
    n_repairAssistanceEligible = sum(repairAssistanceEligible == 1),
    n_replacementAssistanceEligible = sum(replacementAssistanceEligible == 1),
    n_personalPropertyEligible = sum(personalPropertyEligible == 1),
    n_ihpMax = sum(ihpMax == 1),
    n_haMax = sum(haMax == 1),
    n_onaMax = sum(onaMax == 1)
  ) %>% 
  mutate(
    p_ownRent = sum(ownRent == 1) / sum(ownRent == 1 | ownRent == 0),
    p_primaryResidence = sum(primaryResidence == 1) / sum(primaryResidence == 1 | primaryResidence == 0),
    p_homeOwnersInsurance = sum(homeOwnersInsurance == 1) / sum(homeOwnersInsurance == 1 | homeOwnersInsurance == 0),
    p_ihpEligible = sum(ihpEligible == 1) / sum(ihpEligible == 1 | ihpEligible == 0),
    p_haEligible = sum(haEligible == 1) / sum(haEligible == 1 | haEligible == 0),
    p_onaEligible = sum(onaEligible == 1) / sum(onaEligible == 1 | onaEligible == 0),
    p_utilitiesOut = sum(utilitiesOut == 1) / sum(utilitiesOut == 1 | utilitiesOut == 0),
    p_habitabilityRepairsRequired = sum(habitabilityRepairsRequired == 1) / sum(habitabilityRepairsRequired == 1 | habitabilityRepairsRequired == 0),
    p_destroyed = sum(destroyed == 1) / sum(destroyed == 1 | destroyed == 0),
    p_rentalAssistanceEligible = sum(rentalAssistanceEligible == 1) / sum(rentalAssistanceEligible == 1 | rentalAssistanceEligible == 0),
    p_repairAssistanceEligible = sum(repairAssistanceEligible == 1) / sum(repairAssistanceEligible == 1 | repairAssistanceEligible == 0),
    p_replacementAssistanceEligible = sum(replacementAssistanceEligible == 1) / sum(replacementAssistanceEligible == 1 | replacementAssistanceEligible == 0),
    p_personalPropertyEligible = sum(personalPropertyEligible == 1) / sum(personalPropertyEligible == 1 | personalPropertyEligible == 0),
    p_ihpMax = sum(ihpMax == 1) / sum(ihpMax == 1 | ihpMax == 0),
    p_haMax = sum(haMax == 1) / sum(haMax == 1 | haMax == 0),
    p_onaMax = sum(onaMax == 1) / sum(onaMax == 1 | onaMax == 0)
  ) %>% 
  filter(row_number() == 1)

# glimpse(ca_shape_binary)
# summary(ca_shape_binary$n_ownRent)
# summary(ca_shape_binary$p_ownRent)

###### Plot boolean vars (abs number and prop)  -------------------------

variables_to_plot <-
  c(
    "n_ownRent",
    "n_primaryResidence",
    "n_homeOwnersInsurance",
    "n_ihpEligible",
    "n_haEligible",
    "n_onaEligible",
    "n_utilitiesOut",
    "n_habitabilityRepairsRequired",
    "n_destroyed",
    "n_rentalAssistanceEligible",
    "n_repairAssistanceEligible",
    "n_replacementAssistanceEligible",
    "n_personalPropertyEligible",
    "n_ihpMax",
    "n_haMax",
    "n_onaMax",
    "p_ownRent",
    "p_primaryResidence",
    "p_homeOwnersInsurance",
    "p_ihpEligible",
    "p_haEligible",
    "p_onaEligible",
    "p_utilitiesOut",
    "p_habitabilityRepairsRequired",
    "p_destroyed",
    "p_rentalAssistanceEligible",
    "p_repairAssistanceEligible",
    "p_replacementAssistanceEligible",
    "p_personalPropertyEligible",
    "p_ihpMax",
    "p_haMax",
    "p_onaMax"
  )

map_zcta_variable_c <- function(data, variable, palette = "Derain") {
  # Create the plot
  p <- ggplot(data) +
    geom_sf(aes(fill = .data[[variable]]), color = NA, size = 0.2) + 
    scale_fill_met_c(palette, na.value = "grey", direction = -1) +
    theme_minimal() +
    labs(title = paste(variable, "by ZCTA"),
         fill = variable) +
    theme_map()
  
  # Save the plot
  ggsave(filename = paste0(path_plot, variable, "_by_zcta.png"), plot = p, width = 10, height = 8)
}

# Loop through each variable and generate the plot
for (variable in variables_to_plot) {
  print(variable)
  map_zcta_variable_c(ca_shape_binary, variable)
}


# Numeric vars -------------------------------------------------------------------
# "ihpAmount", "haAmount", "onaAmount", "rentalAssistanceAmount", "repairAmount", "replacementAmount", "personalPropertyAmount"

# select numeric vars
var_numeric <-
  c(
    "ihpAmount",
    "haAmount",
    "onaAmount",
    "rentalAssistanceAmount",
    "repairAmount",
    "replacementAmount",
    "personalPropertyAmount",
    "rpfvl", # real property loss
    "ppfvl" # personal property loss
  )

# calculate the medians
ca_shape_numeric <- ca_shape %>% 
  select(ZCTA5CE10, all_of(var_numeric)) %>%
  group_by(ZCTA5CE10) %>% 
  mutate(
    med_ihpAmount = median(ihpAmount, na.rm = TRUE),
    med_haAmount = median(haAmount, na.rm = TRUE),
    med_onaAmount = median(onaAmount, na.rm = TRUE),
    med_rentalAssistanceAmount = median(rentalAssistanceAmount, na.rm = TRUE),
    med_repairAmount = median(repairAmount, na.rm = TRUE),
    med_replacementAmount = median(replacementAmount, na.rm = TRUE),
    med_personalPropertyAmount = median(personalPropertyAmount, na.rm = TRUE),
    med_rpfvl = median(rpfvl, na.rm = TRUE),
    med_ppfvl = median(ppfvl, na.rm = TRUE)
  ) %>% 
  filter(row_number() == 1)

##### Plot numeric vars (median) -------------------------
variables_to_plot <-
  c(
    "med_ihpAmount",
    "med_haAmount",
    "med_onaAmount",
    "med_rentalAssistanceAmount",
    "med_repairAmount",
    "med_replacementAmount",
    "med_personalPropertyAmount",
    "med_rpfvl",
    "med_ppfvl"
  )

# Loop through each variable and generate the plot
for (variable in variables_to_plot) {
  print(variable)
  map_zcta_variable_c(ca_shape_numeric, variable)
}


hist(ca_shape$rpfvl)
summary(ca_shape$ppfvl)


