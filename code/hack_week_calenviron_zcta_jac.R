##Hack Week##
#CalEnviroScreen 2.0 and 4.0

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

#Bring in CES 2.0 first, it was only available as a shapefile for download
ces_2_path <- here("..", "ces2_0shp", "CES2_0Results_SHP.shp")
ces_2 <- st_read(ces_2_path)
glimpse(ces_2)

#snag only variables of interest for this analysis
ces_2 <- ces_2 %>% select(Tractdbl,TractTXT, Population, County, Approx__ZI, CESScore, PopCharPct, PopCharSco, PollutionS, PollutionP)
glimpse(ces_2)

#Rename variables so they are consistent with CES 4.0 dataset
ces_2 <- ces_2 %>% 
  rename(
    ZIP_2 = Approx__ZI,
    CES_score_2 = CESScore,
    poll_burd_percent_2 = PollutionP,
    poll_burd_score_2 = PollutionS,
    pop_char_score_2 = PopCharSco,
    pop_char_percent_2 = PopCharPct,
    County_2 = County
  )
glimpse(ces_2)

#Drop tracts with -1 score (no score), removing 62 census tracts
ces_2 <- ces_2 %>% filter(CES_score_2>0)

#Removing zip codes that aren't regulation, dropping 6 rows
ces_2 <- ces_2 %>% filter(ZIP_2>89999)

#Remove spatial aspect
ces_2 <-st_drop_geometry(ces_2) 

#Calculate population for each ZCTA
ces_2_zcta_pop <- ces_2 %>% group_by(ZIP_2) %>% 
  summarize(tot_zcta_pop_2 = sum(Population)) 

#Calculate population scores
ces_2 <- ces_2 %>%  
  mutate(popwt_ct_poll_burd_score_2 = (poll_burd_score_2 * Population),
         popwt_ct_CES_score_2 = (CES_score_2 * Population))

#Calculate population weighted ZCTA
ces_2_zcta <- ces_2 %>% group_by(ZIP_2) %>%
  summarize(zcta_poll_burd_score_2 = mean(popwt_ct_poll_burd_score_2),
            zcta_CES_score_2 = mean(popwt_ct_CES_score_2))

#Bring total pop at zcta back to main data
ces_2_zcta <- left_join(ces_2_zcta, ces_2_zcta_pop)

#Dividing by total population to finalize pop-weighted scores
ces_2_zcta <- ces_2_zcta %>% mutate(zcta_poll_burd_score_2 = zcta_poll_burd_score_2/tot_zcta_pop_2,
                                    zcta_CES_score_2 = zcta_CES_score_2/tot_zcta_pop_2)

#Calculate percentile of zcta pollution burden score for CES2
ecdf_function <- ecdf(ces_2_zcta$zcta_poll_burd_score_2)
ecdf_function2 <- ecdf(ces_2_zcta$zcta_CES_score_2)
ces_2_zcta <- ces_2_zcta %>% mutate(zcta_poll_burd_percent_2 = ecdf_function(zcta_poll_burd_score_2)*100,
                                    zcta_ces_percent_2 = ecdf_function2(zcta_CES_score_2)*100)
glimpse(ces_2_zcta)

#Select final data for CES 2 ZCTA
ces_2_zcta <- ces_2_zcta %>% select(ZIP_2, zcta_ces_percent_2, zcta_poll_burd_percent_2, tot_zcta_pop_2)

#Bring in CES 4.0
ces_4 <- read_csv("data/01_raw/calenviroscreen-40.csv")
glimpse(ces_4)
summary(ces_4$CES_score_4)

#Drop NAs, n=102 census tracts missing data
ces_4 <- ces_4 %>% drop_na(CES_score_4)

#Removing zip codes that aren't regulation, dropping 6 rows
ces_4 <- ces_4 %>% filter(ZIP>89999)

#Calculate population for each ZCTA
ces_4_zcta_pop <- ces_4 %>% group_by(ZIP) %>% 
  summarize(tot_zcta_pop_4 = sum(tot_pop)) 

#Calculate population scores
ces_4 <- ces_4 %>%  
  mutate(popwt_ct_poll_burd_score_4 = (poll_burd_score_4 * tot_pop),
         popwt_ct_CES_score_4 = (CES_score_4 * tot_pop))

#Calculate population weighted ZCTA
ces_4_zcta <- ces_4 %>% group_by(ZIP) %>%
  summarize(zcta_poll_burd_score_4 = mean(popwt_ct_poll_burd_score_4),
            zcta_CES_score_4 = mean(popwt_ct_CES_score_4))

#Bring total pop at zcta back to main data
ces_4_zcta <- left_join(ces_4_zcta, ces_4_zcta_pop)

#Dividing by total population to finalize pop-weighted scores
ces_4_zcta <- ces_4_zcta %>% mutate(zcta_poll_burd_score_4 = zcta_poll_burd_score_4/tot_zcta_pop_4,
                                    zcta_CES_score_4 = zcta_CES_score_4/tot_zcta_pop_4)

#Calculate percentile of zcta pollution burden score for CES4
ecdf_function <- ecdf(ces_4_zcta$zcta_poll_burd_score_4)
ecdf_function2 <- ecdf(ces_4_zcta$zcta_CES_score_4)
ces_4_zcta <- ces_4_zcta %>% mutate(zcta_poll_burd_percent_4 = ecdf_function(zcta_poll_burd_score_4)*100,
                                    zcta_ces_percent_4 = ecdf_function2(zcta_CES_score_4)*100)
glimpse(ces_4_zcta)

#Select final ZCTA data
ces_4_zcta <- ces_4_zcta %>% select(ZIP, zcta_ces_percent_4, zcta_poll_burd_percent_4, tot_zcta_pop_4)

#make CES2.0 ZIP
ces_2_zcta <- ces_2_zcta %>% mutate(ZIP_2 = as.numeric(ZIP_2))

#Bring together ces_4 and ces_2
ces_vars <- left_join(ces_4_zcta, ces_2_zcta, by=c("ZIP"="ZIP_2"))
glimpse(ces_vars)

#Create disadvantaged community variables (top quartile of each)
ces_vars <- ces_vars %>% mutate(ces_4_q4 = ifelse(zcta_ces_percent_4>=75,1,0),
                                ces_4_envburd_q4 = ifelse(zcta_poll_burd_percent_4>=75,1,0),
                                ces_2_q4 = ifelse(zcta_ces_percent_2>=75,1,0),
                                ces_2_envburd_q2 = ifelse(zcta_poll_burd_percent_2>=75,1,0))

#Make a map of CES and pollution burden at ZIP level
zip_path <- here("..", "ca_zip_2010", "data_EPSG_4326", "ZCTA2010.shp")
zip <- st_read(zip_path)
zip_nad83 <- st_transform(zip, crs=3310)
zip_nad83 <- zip_nad83 %>% mutate(ZCTA=as.numeric(ZCTA))
zip_nad83 <- left_join(zip_nad83, ces_vars, by = c("ZCTA"="ZIP"))

main_ces <- zip_nad83 %>%
  ggplot(aes(fill = factor(ces_4_q4))) + 
  geom_sf(color = "white", lwd = 0)+ 
  scale_fill_manual("CES Disadvantaged", labels = c("No","Yes"), values = c("#97C784", "#EECA6D"))+
  theme_void(base_size = 14) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) 

pol <- zip_nad83 %>%
  ggplot(aes(fill = factor(ces_4_envburd_q4))) + 
  geom_sf(color = "white", lwd = 0)+ 
  scale_fill_manual("Pollution Disadvantaged", labels = c("No","Yes"), values = c("#6F9B69", "#818BE1"))+
  theme_void(base_size = 14) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) 
main_ces + pol

#Save data file for future analysis
write_csv(ces_vars, "data/02_processed/ces_vars.csv") 
