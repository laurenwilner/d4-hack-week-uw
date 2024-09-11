#------------------------------
# CSDE D4 Hack Week 
# September 9-13
# FEMA EDA 

#------------------------------
# setup
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(tidyverse, arrow)

#------------------------------
# load data
my_root <- "~/Desktop/Desktop/epidemiology_PhD/00_repos/d4-hack-week-uw/"
path <- "01_raw/"
df <- read_parquet(paste0(path, "wf_fema_dta.parquet"))

#------------------------------
# EDA

