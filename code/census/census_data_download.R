
#### Title: Claim to Flame census data download 
#### Purpose: Download census data at ZCTA level for use in FEMA claims analysis 
#### Author: David Coomes
#### Date: 9/10/2024

rm(list = ls())

# load libraries
library(tidyverse)
library(tidycensus)
library(here)


## Function for downloading zip code data into a table
zip_table_func <- function(acs_table, acs_year, zip_list, output_year, survey_type = "acs5", geometry=FALSE, category=NULL) {
  # Function that downloads data from acs5 year estimates for specific zipcodes
  # acs_table is the data table that you are requesting
  # acs_year is the last year of the 5 year estimates
  # county_list is a list of counties that you want to get information for
  # output_year is a string variable that is the list of years of data that you want (ex: "2011-2015")
  # survey_type is for either 1 year or 5 year estimates (5 year is the default)
  # geometry allows you to get location data for maps - set to FALSE by default
  # var_name is the general name that you want to use for the output variable (ex: disability)
  
  # first, download variables from Tidycensus
  df <- get_acs(
    geography = "zcta",
    table = acs_table,
    year = acs_year, 
    survey= survey_type,
    geometry = geometry
  ) %>%
    # next, filter to the selected zip codes and reshape data
    filter(GEOID %in% zip_list) %>%
    mutate(year = output_year) %>%
    rename(geography = NAME)
  
  # finally, add variable names
  # load variable names from Tidycensus
  acs_vars <- load_variables(acs_year, survey_type, cache=TRUE)     # list of variables for this year
  local_vars <- acs_vars %>%
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



zip_df_function <- function(acs_table, acs_year, zip_list, output_year, survey_type = "acs5", geometry=FALSE, category=NULL) {
  # Function that downloads data from multiple acs5 year estimates for specific counties and combines them into a single data frame
  # acs_table is the data table that you are requesting
  # acs_year is a list of the last year of the 5 year estimates (ex: list(2015, 2020))
  # county_list is a list of counties that you want to get information for
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




