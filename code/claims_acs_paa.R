
#### Title: PAA FEMA claims analysis
#### Purpose: Preliminary analysis of FEMA claims data for PAA abstract
#### Author: David Coomes
#### Date: September 23, 2024


# load libraries
library(tidyverse)
library(here)
library(ggplot2)
library(jtools)
library(Hmisc)
library(urbnmapr)
library(tidycensus)
library(janitor)


# Bring in data
# FEMA claims data - individual level
fema_claims <- read_csv(here("data", "02_processed", "ca_shape_w_suc_claims.csv"))

# ACS data - ZCTA level
acs <- read_csv(here("data", "02_processed", "SES variables ACS.csv")) %>%
  mutate(median_income = as.numeric(MedIncome))

# California map
county_umap <- get_urbn_map("zctas", sf = TRUE)


## Data cleaning
fema_claims_df <- fema_claims %>%
  filter(!is.na(disasterNumber), !ownRent == "Unknown") %>%
  mutate(age_group = factor(applicantAge,
                            levels = c("50-64", "<19", "19-34", "35-49", "65+")),
         income_group = factor(grossIncome,
                               levels = c("$30,001-$60,000", 
                                          "0",
                                          "<$15,000",
                                          "$15,000-$30,000",
                                          "$60,001-$120,000",
                                          "$120,001-$175,000",
                                          ">$175,000")),
         income_group_plot = factor(grossIncome,
                               levels = c("0",
                                          "<$15,000",
                                          "$15,000-$30,000",
                                          "$30,001-$60,000", 
                                          "$60,001-$120,000",
                                          "$120,001-$175,000",
                                          ">$175,000")))


# fema claims at area level
# we want % successful claims, total # of claims, total amount of claims, 
  # % renters for claims
fema_claims_zip_df <- fema_claims_df %>%
  group_by(GEOID10, disasterNumber) %>%
  dplyr::summarize(pct_successful_claims = mean(i_claim_suc, na.rm=TRUE)*100,
            num_successful_claims = sum(i_claim_suc, na.rm=TRUE),
            total_num_claims = n(),
            total_amount_claims = sum(ihpAmount),
            pct_renters_claims = (sum(ownRent=="Renter") / (sum(ownRent=="Owner") + sum(ownRent=="Renter"))*100))


# Descriptive
fema_claims_df %>%
  ggplot(aes(x=income_group_plot)) + 
  geom_bar() +
  facet_wrap(~i_claim_suc, nrow=2) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

# amount awarded
fema_claims_df %>%
  ggplot(aes(x=log(ihpAmount))) +
  geom_histogram()

# Number and percent of successful claims by zipcode
table(fema_claims_zip_df$total_num_claims)
summary(cut2(fema_claims_zip_df$total_num_claims, m=100))
summary(fema_claims_zip_df$total_num_claims)
summary(fema_claims_zip_df$pct_successful_claims)
summary(fema_claims_zip_df$pct_successful_claims[fema_claims_zip_df$total_num_claims > 100])

fema_claims_zip_df 


fema_claims_zip_df %>%
  filter(pct_successful_claims > 0) %>%
  ggplot(aes(pct_successful_claims)) +
  geom_histogram() + 
  theme_classic()



### Analysis - individual level

# Logistic regressions
model_1 <- glm(i_claim_suc ~ age_group + disasterNumber + ownRent, data = fema_claims_df, 
               family = binomial(link = "logit"))
summary(model_1)
exp(3.315e-2)
summ(model_1, exp=TRUE)

model_2 <- glm(i_claim_suc ~ ownRent + disasterNumber, data = fema_claims_df,
               family = binomial(link = "logit"))

summ(model_2, exp=TRUE)

model_3 <- glm(i_claim_suc ~ income_group + ownRent + disasterNumber, data = fema_claims_df, 
               family = binomial(link = "logit"))
summ(model_3, exp=TRUE)

model_4 <- glm(i_claim_suc ~ age_group + income_group + ownRent + disasterNumber, data = fema_claims_df, 
               family = binomial(link = "logit"))
summ(model_4, exp=TRUE)

model_5 <- lm(log(ihpAmount) ~ ownRent + income_group + disasterNumber, 
               data = fema_claims_df %>%
                filter(ihpAmount > 0))
summ(model_5, exp=TRUE, confint=TRUE)


### Analysis - area level
fema_claims_zip_df <- fema_claims_zip_df %>%
  left_join(acs, by = c("GEOID10" = "ZCTA")) %>%
  mutate(median_income = median_income/1000) 

model_6 <- lm(num_successful_claims ~ median_income + disasterNumber, data = fema_claims_zip_df %>%
                filter(num_successful_claims > 0))
summ(model_6, confint=TRUE)

model_7 <- lm(pct_successful_claims ~ median_income + disasterNumber, data = fema_claims_zip_df %>%
                filter(num_successful_claims > 0))
summ(model_7, confint=TRUE)

model_8 <- lm(pct_successful_claims ~ EduLessHigh + disasterNumber, data = fema_claims_zip_df %>%
                filter(num_successful_claims > 0))
summ(model_8, confint=TRUE)

model_9 <- lm(num_successful_claims ~ median_income + EduLessHigh + LinIsolation + ElderNoInsMedi +
                disasterNumber, data = fema_claims_zip_df %>%
                filter(num_successful_claims > 0))
summ(model_9, confint=TRUE)

model_10 <- lm(pct_successful_claims ~ median_income + EduLessHigh + LinIsolation + ElderNoInsMedi +
                disasterNumber, data = fema_claims_zip_df %>%
                filter(num_successful_claims > 0),
                family = binomial())
summ(model_10, confint=TRUE)



