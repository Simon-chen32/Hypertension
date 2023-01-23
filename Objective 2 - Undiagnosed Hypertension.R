# set working directory 
setwd("C:/Users/SIMC/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - 03 Secondary prevention/Analysis")

#### Installing Packages ####
library(tidyverse)
library(tmap)
library(sf)
library(janitor)
library(stringr)
library(ggplot2)

### Loading in Data ####
# GP-LSOA Data
GP_lsoa_data <- read_csv("GP LSOA Distribution Data.csv")

# Age-Distribution Data
GP_age_dist_cl <- read_csv("Cleaned GP Age Distribution.csv")

# QOF Data 
Hyper21_22cl <- read_csv("QOF Data 2021_22.csv")

# ICS Shapefile
Eng_ICS <- st_read("~/Shapefiles/Sub-ICB Shapefiles/SICBL_JUL_2022_EN_BUC.shp")
# Rename the three ICS with "Isle" so there are no difficulties with merging
Eng_ICS$SICBL22NM <- gsub("NHS Cornwall and The Isles Of Scilly ICB - 11N", "NHS Cornwall and the Isles of Scilly ICB - 11N", Eng_ICS$SICBL22NM)
Eng_ICS$SICBL22NM <- gsub("NHS Hampshire and Isle Of Wight ICB - 10R", "NHS Hampshire and Isle of Wight ICB - 10R", Eng_ICS$SICBL22NM)
Eng_ICS$SICBL22NM <- gsub("NHS Hampshire and Isle Of Wight ICB - D9Y0V", "NHS Hampshire and Isle of Wight ICB - D9Y0V", Eng_ICS$SICBL22NM)

# LSOA to Region Lookup File
lsoa_region <- read_csv("LSOA to Region Lookup File.csv")

#### Objective 2 ####
# Comparing QOF Prevalence to HSE Prevalence by GP 
# Loading in HSE GP Prevalence data 
hse_hyp_prev <- read_csv("hypertension_prevalence_estimate_HSE.csv") %>%
  clean_names()

# Age Standardising HSE data 
hse_hypertension_prev <- merge(GP_age_dist_cl, hse_hyp_prev, by.x = 'org_code', by.y = 'code') %>%
  # Create new column for the percentage of patients a GP serves in each LSOA 
  mutate(hse_exp_hyp = (exp_hyp_male*perc_male + exp_hyp_female*perc_female)*100)

# merging to QOF data from 21/22
hse_qof_comparison <- merge(hse_hyp_prev, Hyper21_22_cl, by.x = 'code', by.y = 'practice_code')

# finding difference in crude prevalence 
undiagnosed_hypertension_prev <- hse_qof_comp %>%
  mutate(undiagnosed_hyp = percent - prevalence_percent_21_22) %>%
  rename(hse_prevalence = percent, 
         practice_code = code)

# finding absolute prevalence at GP level 
gp_undiagnosed_hypertension <- merge(undiagnosed_hypertension_prev, GP_lsoa_data, by = 'practice_code') %>%
  # selecting variables of interest 
  select(practice_code, practice_name, number_of_patients, hse_prevalence, prevalence_percent_21_22, undiagnosed_hyp,
         lsoa_code, lsoa_pop, sub_icb_loc_ods_code, sub_icb_loc_ons_code, sub_icb_loc_name) %>%
  # Calculating total undiagnosed by GP 
  mutate(undiagnosed_totals = (undiagnosed_hyp/100)*number_of_patients, 
         undiagnosed_v2 = number_of_patients*0.1125633,
         gp_coverage = number_of_patients/lsoa_pop)

# Aggregating at LSOA level 
lsoa_undiagnosed_hypertension <- gp_undiagnosed_hypertension %>%
  group_by(lsoa_code) %>%
  summarise(undiagnosed_prev = sum(gp_coverage*undiagnosed_hyp, na.rm = TRUE), 
            hypertension_prev = sum(gp_coverage*prevalence_percent_21_22, na.rm = T),
            lsoa_pop = mean(lsoa_pop)) %>%
  mutate(undiagnosed = round(undiagnosed_prev*lsoa_pop/100), 
         undiagnosed_v2 = round(0.1125633*lsoa_pop), 
         hypertension_cases = round(hypertension_prev*lsoa_pop/100))

abs_undiagnosed_lsoa_ccg <- merge(abs_undiagnosed_lsoa, lsoa_ccg_pop_merge, by = c('lsoa_code', 'lsoa_pop')) %>%
  select(-c(FID, LAD21CD, LAD21NM))

abs_undiagnosed_lsoa_region <- merge(abs_undiagnosed_lsoa_ccg, ccg_region, by = c('CCG21NM', 'CCG21CD', 'CCG21CDH'))

# Aggregating at ICS level
sub_icb_undiagnosed_hypertension <- gp_undiagnosed_hypertension %>%
  group_by(sub_icb_loc_ods_code, sub_icb_loc_ons_code, sub_icb_loc_name) %>%
  summarise(undiagnosed_hypertension = round(sum(undiagnosed_totals, na.rm = TRUE),digits = 0),
            sub_icb_pop = sum(number_of_patients)) %>%
  ungroup()

top_decile_undiagnosed_ics <-  sub_icb_undiagnosed_hypertension %>%
  mutate(decile = ntile(undiagnosed_hypertension, 10)) %>%
  filter(decile == 10)

# plotting on a map 
top_decile_undiagnosed_ics_shp <- merge(Eng_ICS, top_decile_undiagnosed_ics, by.x = 'SICBL22CD', 
                                        by.y = 'sub_icb_loc_ons_code')

tm_shape(Eng_ICS) + 
  tm_borders(col = "black") + 
  tm_shape(top_decile_undiagnosed_ics_shp) + 
  tm_fill(col = "#f7a600", alpha = 0.6) + 
  tm_layout(main.title = "Top Decile of Undiagnosed Hypertension") + 
  tm_compass(position = c("left", "top")) + 
  tm_scale_bar(position = c("right", "bottom"))

# Aggregating at Regional Level
regional_abs_undiagnosed <- merge(sub_icb_abs, ccg_region, by.x = 'sub_icb_loc_ods_code', by.y = 'SICBL22CDH') %>%
  group_by(NHSER21NM, NHSER21CD) %>%
  summarise(tot_undiagnosed = sum(undiagnosed_hypertension), 
            undiagnosed_v2 = sum(undiagnosed_v2),
            region_population = sum(sub_icb_pop)) %>%
  mutate(rate_per_100000 = round(tot_undiagnosed/(region_population/100000), digits = 2), 
         undiagnosed_prev = round(tot_undiagnosed/region_population*100, digits = 2))

# Mean HSE England percentage is 25.58151
# Subtract from QOF average of 14.32518 to get 11.25633 Undiagnosed 
