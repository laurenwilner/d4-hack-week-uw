
#### Title: Claim to Flame census data download 
#### Purpose: Download census data at ZCTA level for use in FEMA claims analysis 
#### Author: David Coomes
#### Date: 9/10/2024

rm(list = ls())

# load libraries
library(tidyverse)
library(tidycensus)
library(here)
library(sf)
library(MetBrewer)

# census_api_key("YOUR KEY HERE", install = TRUE)





# loading 2000 census variables
census_2000_vars_sf1 <- load_variables(year = 2000, dataset = "sf1")
census_2000_vars_sf2 <- load_variables(year = 2000, dataset = "sf2") 
census_2000_vars_sf3 <- load_variables(year = 2000, dataset = "sf3") 
census_2000_vars_sf4 <- load_variables(year = 2000, dataset = "sf4") 

census_vars_2000 <- census_2000_vars_sf1 %>%
  bind_rows(census_2000_vars_sf2) %>%
  bind_rows(census_2000_vars_sf3) %>%
  bind_rows(census_2000_vars_sf4)

# loading 2010 census variables
census_vars_2010_sf1 <- load_variables(year = 2010, dataset = "sf1")
census_vars_2010_sf2 <- load_variables(year = 2010, dataset = "sf2")

census_vars_2010 <- census_vars_2010_sf1 %>%
  bind_rows(census_vars_2010_sf2)

# loading 2020 census variables
census_vars_2020_dhc <- load_variables(year=2020, dataset = "dhc")
census_vars_2020_dp <- load_variables(year=2020, dataset = "dp")

census_vars_2020 <- census_vars_2020_dhc %>%
  bind_rows(census_vars_2020_dp)

# looking for specific variables
census_vars_2010 %>%
  ## Filter down to concepts containing the string
  ## "GEOGRAPHICAL MOBILITY"
  filter(grepl("EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER", concept)) %>% 
  select(concept) %>% 
  ## Remeber concepts cover many variables, 
  ## so we just need to see the unique values to find 
  ## the one we want
  unique() %>%
  print(n = 60)


## Needed variables
  # 2000: 
    # Median income: P053001, 
    # % in poverty: 	
      # P092001: total households
      # P092002: total households below poverty line
    # employment status:
      # P043005: total male civilians in labor force
      # P043007: total male civilians unemployed in labor force
      # P043012: total female civilians in labor force
      # P043014: total female civilians unemployed in labor force
    # education status
      # P037002: total male
      # P037003, 
  
  # 2010: 
    # Median income: Not included
    # % in poverty: Not included
    # employment status: Not included
    # education status: Not included
    # Tenure: 
      # H004001: total # of households
      # H00400: total # of households owned with mortgage
      # H004001: total # of households owned free and clear
      # H004001: total # of households renting







##################################
## Download and clean variables ##
##################################



###########################
## 2010 Census Variables ##
###########################


 
## Tenure status
# downloading data
tenure_df <- get_decennial(
  geography = "zcta",
  table = "H004", 
  state = "CA",
  year = 2010,
  geometry = TRUE
)

# cleaning data
# getting zcta geography to use with all variables
geo_df <- tenure_df %>%
  mutate(zcta = as.numeric(GEOID) - 600000) %>%
  select(zcta, geometry) %>%
  group_by(zcta) %>%
  slice(1)

# getting % renter occupied housing
tenure_df <- tenure_df %>%
  mutate(zcta = as.numeric(GEOID) - 600000) %>%
  select(zcta, variable, value) %>%
  left_join(census_vars_2010 %>%
              select(name, label), 
            by = c("variable" = "name")) %>%
  st_drop_geometry() %>%
  pivot_wider(id_cols = c(zcta), names_from = label, values_from = value) %>%
  mutate(pct_homes_renter_occupied = `Total!!Renter occupied` / Total) %>%
  rename(num_houses = Total) %>%
  select(zcta, num_houses, pct_homes_renter_occupied) 


## Race/ethnicity
# downloading data
race_eth_df <- get_decennial(
  geography = "zcta",
  #variables = "P131002",
  table = "P005", 
  state = "CA",
  year = 2010,
  geometry = FALSE
)

# cleaning data
race_eth_df <- race_eth_df %>%
  mutate(zcta = as.numeric(GEOID) - 600000) %>%
  select(zcta, variable, value) %>%
  left_join(census_vars_2010 %>%
              select(name, label), 
            by = c("variable" = "name")) %>%
  pivot_wider(id_cols = zcta, names_from = label, values_from = value) %>%
  mutate(pct_race_eth_white = `Total!!Not Hispanic or Latino!!White alone` / Total,
         pct_race_eth_black = `Total!!Not Hispanic or Latino!!Black or African American alone` / Total,
         pct_race_eth_aian = `Total!!Not Hispanic or Latino!!American Indian and Alaska Native alone` / Total,
         pct_race_eth_asian = `Total!!Not Hispanic or Latino!!Asian alone` / Total,
         pct_race_eth_nhpi = `Total!!Not Hispanic or Latino!!Native Hawaiian and Other Pacific Islander alone` / Total,
         pct_race_eth_other = `Total!!Not Hispanic or Latino!!Some Other Race alone` / Total,
         pct_race_eth_multiple = `Total!!Not Hispanic or Latino!!Two or More Races` / Total,
         pct_race_eth_hispanic = `Total!!Hispanic or Latino` / Total) %>%
  rename(total_persons = Total) %>%
  select(zcta, total_persons, pct_race_eth_white:pct_race_eth_hispanic)


## Rurality
# downloading data
rural_df <- get_decennial(
  geography = "zcta",
  table = "H002", 
  state = "CA",
  year = 2010,
  geometry = FALSE
)

rural_df <- rural_df %>%
  mutate(zcta = as.numeric(GEOID) - 600000) %>%
  select(zcta, variable, value) %>%
  left_join(census_vars_2010 %>%
              select(name, label), 
            by = c("variable" = "name")) %>%
  pivot_wider(id_cols = zcta, names_from = label, values_from = value) %>%
  mutate(pct_living_rural = `Total!!Rural` / Total) %>%
  select(zcta, pct_living_rural)
  
  
## Combining all 2010 census variables
census_2010 <- tenure_df %>%
  full_join(race_eth_df, by="zcta") %>%
  full_join(rural_df, by="zcta") %>%
  mutate(year = 2010,
         source = "Census")


save(census_2010, file = here("data", "02_processed", "census_2010.RData") %>% str_remove("/code/census"))




###########################
## 2020 Census Variables ##
###########################


## Race/ethnicity
# downloading data
race_eth_df <- get_decennial(
  geography = "zcta",
  #variables = "P131002",
  table = "P005", 
  state = "CA",
  year = 2020,
  geometry = FALSE
)






map_check <- geo_df %>%
  left_join(tenure_df, by = "zcta")












# plotting renter occupied status to check
ggplot() + geom_sf(data = map_check, aes(fill = pct_homes_renter_occupied), linewidth= 0) +
  labs(title = "Average Percent Migration Among Mainland US Counties from 2011-2021") + 
  #scale_fill_viridis_b() + 
  scale_fill_viridis(option="D") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12))

























    

# median income
df <- get_decennial(
  geography = "zcta",
  #variables = "P131002",
  table = "P092", 
  state = "CA",
  year = 2000
)


df <- get_decennial(
  geography = "zcta",
  variables = "HCT022001",
  state = "CA",
  year = 2000
)


## Function for downloading zip code data into a table
zip_table_func <- function(census_table, census_year, output_year, geometry=FALSE, category=NULL) {
  # Function that downloads data from acs5 year estimates for specific zipcodes
  # acs_table is the data table that you are requesting
  # acs_year is the last year of the 5 year estimates
  # county_list is a list of counties that you want to get information for
  # output_year is a string variable that is the list of years of data that you want (ex: "2011-2015")
  # survey_type is for either 1 year or 5 year estimates (5 year is the default)
  # geometry allows you to get location data for maps - set to FALSE by default
  # var_name is the general name that you want to use for the output variable (ex: disability)
  
  # first, download variables from Tidycensus
  df <- get_decennial(
    geography = "zcta",
    table = acs_table,
    year = acs_year, 
    #survey= survey_type,
    geometry = geometry, 
    state = "CA"
  ) %>%
    # next, filter to the selected zip codes and reshape data
    # filter(GEOID %in% zip_list) %>%
    mutate(year = output_year) %>%
    rename(geography = NAME)
  
  # finally, add variable names
  # load variable names from Tidycensus
  #acs_vars <- load_variables(acs_year, cache=TRUE)     # list of variables for this year
  local_vars <- census_vars_2000 %>%
    filter(str_detect(name, paste0(acs_table, "_", sep="")))
  
  # add variable names to the data frame and separate
  df <- df %>%
    left_join(local_vars[ , c("name", "concept", "label")], by = c("variable"="name")) %>%
    mutate(label = gsub(":", "", label),
           label = str_replace(label, "Estimate!!Total!!", "")) %>%
    separate(label, into=c("sex", "age", "indicator"), sep="!!") %>%
    mutate(category = category)
  
  return(df)
}


df <- zip_table_func(acs_table = "P037", acs_year = 2000, output_year = "2000", geometry=FALSE)

local_vars <- census_2000_vars_sf3 %>%
  filter(str_detect(name, "P037"))

df <- get_decennial(
  geography = "zcta",
  table = "P037",
  year = 2000, 
  #survey= survey_type,
  geometry = FALSE, 
  state = "CA"
) %>%
  # next, filter to the selected zip codes and reshape data
  # filter(GEOID %in% zip_list) %>%
  mutate(year = 2000) %>%
  rename(geography = NAME)

# finally, add variable names
# load variable names from Tidycensus
#acs_vars <- load_variables(acs_year, cache=TRUE)     # list of variables for this year
local_vars <- census_2000_vars_sf3 %>%
  filter(str_detect(name, "P037"))

df1 <- df %>%
  left_join(local_vars[ , c("name", "concept", "label")], by = c("variable"="name")) %>%
  mutate(label = gsub(":", "", label),
         label = str_replace(label, "Total!!", "")) %>%
  separate(label, into=c("sex", "age", "indicator"), sep="!!") %>%
  mutate(category = NULL)




zip_df_function <- function(acs_table, acs_year, zip_list, output_year, survey_type = "acs5", geometry=FALSE, category=NULL) {
  # Function that downloads data from multiple acs5 year estimates for specific counties and combines them into a single data frame
  # acs_table is the data table that you are requesting
  # acs_year is a list of the last year of the 5 year estimates (ex: list(2015, 2020))
  # output_year is a list of string variables that is the list of years of data that you want (ex: list("2011-2015", "2016-2020")
  # survey_type is for either 1 year or 5 year estimates (5 year is the default)
  
  df <- NULL
  for (i in 1:length(acs_year)) {
    output <- zip_table_func(acs_table = acs_table, acs_year = acs_year[[i]], zip_list = zip_list, output_year = output_year[[i]], 
                             geometry=geometry, category=category)
    df <- bind_rows(df, output)
  }
  
  return(df)
  
}




zip_est_func <- function(acs_table, acs_year, zip_list, output_year, survey_type = "acs5", geometry=FALSE,
                         group_var=NULL, category=NULL) {
  # This function downloads multiple years of data from ACS and combines age and sex groups to calculate overall numbers and percentages
  # It also has an option to include sub-groups via the "group_var" choice
  
  # acs_table is the data table that you are requesting - need to look up
  # acs_year is a list of the last year of the 5 year estimates (ex: list(2015, 2020))
  # county_list is a list of counties that you want to get information for
  # output_year is a list of string variables that is the list of years of data that you want (ex: list("2011-2015", "2016-2020")
  # survey_type is for either 1 year or 5 year estimates (5 year is the default)
  # geometry is TRUE if you want to include GIS information for mapping
  # State is for the state that you would like estimates for (Washington is default)
  # group_var defines if you want to break out estimates by sub-population (i.e. sex, age). Default is NULL
  
  
  # Downloading and cleaning all of the data sets that are requested
  data_set <- NULL
  for (i in 1:length(acs_year)) {
    output <- zip_table_func(acs_table = acs_table, acs_year = acs_year[[i]], zip_list = zip_list, output_year = output_year[[i]], 
                             geometry=FALSE)
    data_set <- bind_rows(data_set, output)
  }
  
  # Extracting geometry to merge back in later
  if (geometry==TRUE){
    geom_acs_year <- unlist(tail(acs_year, n=1))
    geom_output_year <- unlist(tail(output_year, n=1))
    geometry_df <- zip_table_func(acs_table = acs_table, acs_year = geom_acs_year, zip_list = zip_list, output_year = geom_output_year, 
                                  geometry=TRUE) %>%
      dplyr::select(geography, geometry) %>%
      group_by(geography) %>%
      slice(1)
  }
  
  # setting up grouping variables
  group_by_vars <- c("geography", "indicator", "year")
  group_by_vars2 <- c("geography", "indicator", "year", group_var)
  group_by_vars3 <- c("geography", "year", group_var)
  
  # Calculating overall numbers and percentages
  df <- data_set %>%
    filter(!is.na(indicator)) %>%             # removing all rows that are aggregates of other rows
    group_by_at(group_by_vars) %>%
    summarize(est_count = sum(estimate),
              moe_count = round(sqrt(sum(moe^2)), 0)) %>%
    group_by(geography, year) %>%
    mutate(total_pop = sum(est_count),
           total_pop_moe = moe_sum(moe = moe_count, estimate = est_count),
           est_proportion = round(est_count/sum(est_count)*100, 1),
           moe_proportion = round(moe_prop(num = est_count, denom = total_pop, moe_num = moe_count, moe_denom = total_pop_moe)*100, 1)) %>%
    #filter(UQ(sym(var_name)) == var_label) %>%
    dplyr::select(-c(total_pop, total_pop_moe))
  
  # Pivoting estimates to longer format
  est_df <- df %>%
    dplyr::select(geography, year, est_count, est_proportion, indicator) %>%
    rename(number = est_count, percent = est_proportion) %>%
    pivot_longer(cols = c(number, percent), names_to = "measurement_type", values_to = "estimate")
  
  # Pivoting moe to longer format
  moe_df <- df %>%
    dplyr::select(geography, year, moe_count, moe_proportion, indicator) %>%
    rename(number = moe_count, percent = moe_proportion) %>%
    pivot_longer(cols = c(number, percent), names_to = "measurement_type", values_to = "moe")
  
  # Combining estimates and moe
  df_full <- est_df %>%
    full_join(moe_df) %>%
    #rename(geography = county) %>%
    mutate(population = "Total population",
           moe_low = ifelse(estimate - moe < 0, 0, estimate - moe),
           moe_high = ifelse(estimate + moe > 100 & measurement_type=="percent", 100, estimate + moe)) %>%
    relocate(population)
  
  # Calculating group level numbers and percentages
  if (!is.null(group_var)){
    # Calculating group numbers and percentages
    df_group <- data_set %>%
      filter(!is.na(indicator)) %>%             # removing all rows that are aggregates of other rows
      group_by_at(group_by_vars2) %>%
      summarize(est_count = sum(estimate),
                moe_count = round(sqrt(sum(moe^2)), 0)) %>%
      group_by_at(group_by_vars3) %>%
      mutate(total_pop = sum(est_count),
             total_pop_moe = moe_sum(moe = moe_count, estimate = est_count),
             est_proportion = round(est_count/sum(est_count)*100, 1),
             moe_proportion = round(moe_prop(num = est_count, denom = total_pop, moe_num = moe_count, moe_denom = total_pop_moe)*100, 1)) %>%
      #filter(UQ(sym(var_name)) == var_label) %>%
      dplyr::select(-c(total_pop, total_pop_moe))
    
    # Pivoting estimates to longer format
    est_df_group <- df_group %>%
      dplyr::select(geography, year, est_count, est_proportion, indicator) %>%
      rename(number = est_count, percent = est_proportion) %>%
      pivot_longer(cols = c(number, percent), names_to = "measurement_type", values_to = "estimate")
    
    # Pivoting moe to longer format
    moe_df_group <- df_group %>%
      dplyr::select(geography, year, moe_count, moe_proportion, indicator) %>%
      rename(number = moe_count, percent = moe_proportion) %>%
      pivot_longer(cols = c(number, percent), names_to = "measurement_type", values_to = "moe")
    
    # Combining estimates and moe
    df_group_full <- est_df_group %>%
      full_join(moe_df_group) %>%
      mutate(moe_low = ifelse(estimate - moe < 0, 0, estimate - moe),
             moe_high = ifelse(estimate + moe > 100 & measurement_type=="percent", 100, estimate + moe)) %>%
      rename(any_of(c(population = group_var)))
    
  }
  
  if (!is.null(group_var)){
    df_full <- df_full %>%
      bind_rows(df_group_full) %>%
      mutate(category=category) %>%
      relocate(category, .after=year)
  }
  
  if (geometry==TRUE){
    df_full <- df_full %>%
      left_join(geometry_df, by="geography")
  }
  
  
  return(df_full)       # returning full data set
  
}




