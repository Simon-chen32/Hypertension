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
Hyper21_22_cl <- read_csv("QOF Data 2021_22.csv")

# Lookup File
ics_region <- read_csv("~/Lookup Files/Sub_ICB_Locations_to_ICB_to_NHS_England_(Region)_(July_2022)_Lookup.csv") %>%
  select(-ObjectId)

ics_population <- read_csv("ICS Population.csv")

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
hse_hyp_prev <- read_csv("~/Hypertension/hypertension_prevalence_estimate_HSE.csv") %>%
  clean_names() %>%
  rename(hse_prevalence = percent, 
         practice_code = code)

# Age Standardising HSE data 
hse_hypertension_prev <- merge(GP_age_dist_cl, hse_hyp_prev, by.x = 'org_code', by.y = 'practice_code') %>%
  # Create new column for the percentage of patients a GP serves in each LSOA 
  mutate(hse_exp_hyp = (exp_hyp_male*perc_male + exp_hyp_female*perc_female)*100) 

# merging to QOF data from 21/22
hse_qof_comparison <- merge(hse_hyp_prev, Hyper21_22_cl, by = 'practice_code') 

# finding difference in crude prevalence 
gp_undiagnosed_hypertension_prev <- merge(hse_qof_comparison, GP_lsoa_data, by = 'practice_code') %>%
  mutate(undiagnosed_hyp = hse_prevalence - prevalence_percent_21_22, 
         undiagnosed_patients = (undiagnosed_hyp/100)*number_of_patients) 
# Rename the three ICS with "Isle" so there are no difficulties with merging
gp_undiagnosed_hypertension_prev$sub_icb_loc_name <- gsub("NHS Cornwall and The Isles Of Scilly ICB - 11N", 
                                                          "NHS Cornwall and the Isles of Scilly ICB - 11N", gp_undiagnosed_hypertension_prev$sub_icb_loc_name)
gp_undiagnosed_hypertension_prev$sub_icb_loc_name <- gsub("NHS Hampshire and Isle Of Wight ICB - 10R",
                                                          "NHS Hampshire and Isle of Wight ICB - 10R", gp_undiagnosed_hypertension_prev$sub_icb_loc_name)
gp_undiagnosed_hypertension_prev$sub_icb_loc_name <- gsub("NHS Hampshire and Isle Of Wight ICB - D9Y0V", 
                                                          "NHS Hampshire and Isle of Wight ICB - D9Y0V", gp_undiagnosed_hypertension_prev$sub_icb_loc_name)

# finding absolute prevalence at GP level 
lsoa_undiagnosed_hypertension <- gp_undiagnosed_hypertension_prev %>%
 # Calculating total undiagnosed by GP 
  mutate(gp_coverage = number_of_patients/lsoa_pop) %>%
  # Aggregating at LSOA level
  group_by(lsoa_code) %>%
  summarise(undiagnosed_prev = sum(gp_coverage*undiagnosed_hyp, na.rm = TRUE), 
            hypertension_prev = sum(gp_coverage*prevalence_percent_21_22, na.rm = T),
            lsoa_pop = mean(lsoa_pop)) %>%
  mutate(undiagnosed = round(undiagnosed_prev*lsoa_pop/100), 
         hypertension_cases = round(hypertension_prev*lsoa_pop/100))

# Aggregating at ICS level
sub_icb_undiagnosed_hypertension <- gp_undiagnosed_hypertension_prev %>%
  group_by(sub_icb_loc_ods_code, sub_icb_loc_ons_code, sub_icb_loc_name) %>%
  summarise(undiagnosed_hypertension = round(sum(undiagnosed_patients, na.rm = T),digits = 0), 
            population = sum(number_of_patients), 
            undiagnosed_rate = round(undiagnosed_hypertension/population*100, digits = 2))

top_decile_undiagnosed_ics <-  sub_icb_undiagnosed_hypertension %>%
  mutate(decile = ntile(undiagnosed_hypertension, 10)) %>%
  filter(decile == 10)

# plotting on a map 
top_decile_undiagnosed_ics_shp <- merge(Eng_ICS, top_decile_undiagnosed_ics, 
                                        by.x = c('SICBL22CD','SICBL22NM'), 
                                        by.y = c('sub_icb_loc_ons_code', 'sub_icb_loc_name'))

tm_shape(Eng_ICS) + 
  tm_borders(col = "black") + 
  tm_shape(top_decile_undiagnosed_ics_shp) + 
  tm_fill(col = "#f7a600", alpha = 0.6) + 
  tm_layout(main.title = "Top Decile of Undiagnosed Hypertension") + 
  tm_compass(position = c("left", "top")) + 
  tm_scale_bar(position = c("right", "bottom"))

# Aggregating at Regional Level
regional_undiagnosed <- merge(sub_icb_undiagnosed_hypertension, ics_region, 
                              by.x = c('sub_icb_loc_ons_code', 'sub_icb_loc_name', 'sub_icb_loc_ods_code'),
                              by.y = c('SICBL22CD', 'SICBL22NM', 'SICBL22CDH')) %>%
  group_by(NHSER22NM, NHSER22CD) %>%
  summarise(tot_undiagnosed = sum(undiagnosed_hypertension), 
            region_population = sum(population)) %>%
  mutate(rate_per_100000 = round(tot_undiagnosed/(region_population/100000), digits = 2), 
         undiagnosed_prev = round(tot_undiagnosed/region_population*100, digits = 2))

# Mean HSE England percentage is 25.58151
# Subtract from QOF average of 14.32518 to get 11.25633 Undiagnosed 

# Exporting Data
write_csv(lsoa_undiagnosed_hypertension, "LSOA Undiagnosed Hypertension.csv")
write_csv(sub_icb_undiagnosed_hypertension, "ICS Undiagnosed Hypertension.csv")
write_csv(regional_undiagnosed, "Undiagnosed Hypertension by Region.csv")
