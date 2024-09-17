# Code description -------------------------
# Author: Vivian
# Date: 09/11/2024
# Goal: Select relevant variables for wildfire fema data

# Load libraries
# setup
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}
pacman::p_load(dplyr, tidyverse, arrow, vroom, here, readr, lubridate)

# Set notation to be long
options(scipen = 999)

# Load data -------------------------
my_root <-"~/Users/vivian/Desktop/0_PhD/0_Research Projects/0_repos/d4-hack-week-uw/data/"
path <- "01_raw/"
df <- read_parquet(paste0(path, "wf_fema_dta.parquet"))

# Select relevant variables -------------------------
# identify vars
keep_vars <-
  c(
    "disasterNumber",
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
    "id"
  )


# select above vars
df <- df %>%
  select(all_of(keep_vars))

# Save data -------------------------
write_parquet(df, paste0(path, "wf_fema_dta_keep_vars.parquet"))




