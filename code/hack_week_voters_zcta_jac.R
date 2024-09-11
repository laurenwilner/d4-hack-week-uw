##Hack Week##
#Voter turnout from https://statewidedatabase.org/d00/g10_geo_conv.html

# Load packages, installing if needed
if(!requireNamespace("pacman", quietly = TRUE))
  install.packages("pacman")

pacman::p_load(
  here,
  ggplot2,
  dplyr,
  sf,
  readr,
  tidyr,
  MetBrewer,
  patchwork,
  arrow)

voter_path <- here("..", "state_g10_2011blk_by_g10_sr.csv")
voter <- read_csv(voter_path)

#Create tractID in the voter data
voter <- voter %>% mutate(tract_id = substr(BLOCK_KEY, start = 1, stop = 11))

#Keep key voter variables: BLCKREG (registered voters in block)
voter <- voter %>% select(BLOCK_KEY,tract_id, BLKREG)

#Create tract level voter reg counts
voter_ct <- voter %>% group_by(tract_id) %>%
  summarize(ct_actual_reg = sum(BLKREG))

#Check counts at ct-level
summary(voter_ct$ct_actual_reg)

#read in the zcta-ct crosswalk from:https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.2010.html#par_textimage_674173622
zcta_ct_xwalk <- read_delim(here("..", "zcta_tract_rel_10.txt"))

#filter to just CA
zcta_ct_xwalk <- zcta_ct_xwalk %>% filter(STATE=="06")

#keep just necessary variables: zcta id, ct id
zcta_ct_xwalk <- zcta_ct_xwalk %>% select(ZCTA5, GEOID)

#add voter data to ZCTA dataframe
zcta_ct_voter <- left_join(zcta_ct_xwalk, voter_ct, by=c("GEOID"= "tract_id"))

#summarize to ZCTA
zcta_voter <- zcta_ct_voter %>% group_by(ZCTA5) %>%
  summarize(zcta_actual_reg = sum(ct_actual_reg))

#Add number of adults in each ZCTA to generate denominator


#Create percent registered
zcta_voter <-
  zcta_voter %>% mutate(voter_reg_percent = zcta_actual_reg / zcta_adult_pop *
                          100)

summary(zcta_voter$voter_reg_percent) #22 NA

zip_path <- here("..", "ca_zip_2010", "data_EPSG_4326", "ZCTA2010.shp")
zip <- st_read(zip_path)
zip_nad83 <- st_transform(zip, crs=3310)

#join voter data to sf object
zip_nad83 <- left_join(zip_nad83, zcta_voter, by=c("ZCTA"="ZCTA5"))

zip_nad83 %>%
  ggplot(aes(fill = voter_reg_percent)) + 
  geom_sf(color = "white", lwd = 0)+ 
  scale_fill_gradientn("% registered voters", colors = rev(met.brewer("Derain", type = "continuous")))+
  theme_void(base_size = 14) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) 
