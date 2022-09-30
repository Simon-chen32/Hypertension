# set working directory 
setwd("~/Hypertension")

#### Installing Packages ####
library(tidyverse)
library(tmap)
library(janitor)
library(sf)
library(stringr)
library(ggplot2)

#### Loading Data #####
# Loading in Shapefile
Eng_CCG <- st_read("~/Hypertension/CCG Geography/CCG_APR_2021_EN_BFC.shp")
ENG_LSOA21 <- st_read("~/Hypertension/LSOA Shapefiles/LSOA_2021_EW_BFE_V5.shp")
ENG_LSOA11 <- st_read("~/Hypertension/LSOA Shapefiles/infuse_lsoa_lyr_2011.shp") %>% 
  filter(str_detect(geo_code, '^E'))

ENG_LAD17 <- st_read("~/Hypertension/2011 LAD Census Geography/Local_Authority_Districts_(December_2017)_Boundaries_in_the_UK_(WGS84).shp") %>%
  filter(str_detect(lad17cd, '^E'))

London_2011 <- st_read("~/Hypertension/LSOA Shapefiles/statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp") %>%
  subset(., select = -c(RGN11CD:AVHHOLDSZ))
London_LAD <- st_read("~/Hypertension/LSOA Shapefiles/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp") %>%
  subset(., select = -c(ONS_INNER, SUB_2006, SUB_2009))

# Loading in GP Data
GP_age_dist <- read_csv("~/Hypertension/GP Data/PopulationAgeDistribution.csv") %>%
  clean_names()

GP_age_dist_wide <- GP_age_dist %>% 
  pivot_wider(names_from = c(age, sex), 
              values_from = value)

# Loading in IMD data 
GP_imd <- read_csv("~/Hypertension/GP Data/indicators_GP_data.csv") %>%
  clean_names() %>%
  filter(time_period == 2019) %>%
  filter(area_type == 'GP')

GP_imd_cl <- GP_imd %>%
  subset(., select = c(parent_code, parent_name, area_code, area_name, value))

LSOA_imd <- read_csv("LSOA_IMD_Scores_Deciles.csv") %>%
  clean_names() %>%
  rename(imd_decile = index_of_multiple_deprivation_imd_decile_where_1_is_most_deprived_10_percent_of_lso_as, 
         imd_score = index_of_multiple_deprivation_imd_score) 

LSOA_imd_cl <- LSOA_imd %>%
  subset(., select =c(lsoa_code_2011, lsoa_name_2011, local_authority_district_code_2019, local_authority_district_name_2019, 
                      imd_score, imd_decile, income_score_rate, employment_score_rate, education_skills_and_training_score, 
                      health_deprivation_and_disability_score, crime_score, living_environment_score, 
                      barriers_to_housing_and_services_score))

# Loading in LSOA data 
GP_lsoa_dist <- read_csv("~/Hypertension/GP Data/gp-reg-pat-prac-lsoa-all.csv") %>%
  clean_names() %>%
  subset(., select = c(practice_code, practice_name, lsoa_code, number_of_patients))

GP_lsoa_data <- GP_lsoa_dist %>% 
  group_by(lsoa_code) %>%
  summarise(lsoa_pop = sum(number_of_patients), 
            practice_code = practice_code, 
            number_of_patients = number_of_patients)

lsoa_ccg_la <- read_csv("LSOA_to_CCG_to_LA_ApriL_2017.csv")

lad_region <- read_csv("Local_Authority_District_to_Region_(December_2018)_Lookup_in_England.csv")

# Checking what LSOA's don't merge, as there is a difference between the two lists

#### Objective One #####
# Loading in Hypertension Prevalence Data 
Hyper21_22 <- read_csv("~/Hypertension/QOF Data/QOF_Hypertension_21-22.csv", skip = 10) %>%
  clean_names() %>%
  rename(list_size_21_22 = list_size_11, 
         under79_21_22 = list_size_79,
         over80_21_22 = list_size_ages_80,
         register_21_22 = register_14,
         prevalence_percent_21_22 = prevalence_percent_15,
         under79_numerator_21_22 = numerator_32, 
         under79_denominator_21_22 = denominator_33, 
         under79_achievement_net_exceptions_21_22 = underlying_achievement_net_of_pc_as_percent_34,
         under79_percent_receiving_intervention_21_22 = patients_receiving_intervention_percent_38,
         over80_numerator_21_22 = numerator_40,
         over80_denominator_21_22 = denominator_41, 
         over80_achievement_net_exceptions_21_22 = underlying_achievement_net_of_pc_as_percent_42,
         over80_percent_receiving_intervention_21_22 = patients_receiving_intervention_percent_46) %>%
  subset(., under79_denominator_21_22 != 0) %>%
  subset(., over80_denominator_21_22 != 0)

Hyper21_22_cl <- Hyper21_22 %>%
  subset(., select = c(sub_icb_loc_ods_code, sub_icb_loc_ons_code, sub_icb_loc_name, 
                       practice_code, practice_name, list_size_21_22, under79_21_22, over80_21_22, register_21_22, 
                       prevalence_percent_21_22, under79_numerator_21_22, under79_denominator_21_22, 
                       under79_achievement_net_exceptions_21_22, under79_percent_receiving_intervention_21_22,
                       over80_numerator_21_22, over80_denominator_21_22, over80_achievement_net_exceptions_21_22,
                       over80_percent_receiving_intervention_21_22))

lsoa_hyper_prev <- merge(GP_lsoa_data, Hyper21_22_cl, by = 'practice_code') %>%
  # Create new column for the percentage of patients a GP serves in each LSOA 
  mutate(gp_coverage = (number_of_patients/lsoa_pop))

# Check if any Practices/LSOAs did not merge over 
checkGP = setdiff( GP_lsoa_data$practice_code, Hyper21_22_cl$practice_code)
checkGP2 = setdiff(Hyper21_22_cl$practice_code, GP_lsoa_data$practice_code)

alldiff_GP <- Hyper21_22_cl[1:dim(Hyper21_22_cl)[1] %in% checkGP,]

checkLSOA = setdiff(GP_lsoa_data$lsoa_code, lsoa_hyper_prev$lsoa_code)

#### Comparing 2019-20 Data to 2021-22 Data #####
## 19-20 ##
Hyper19_20 <- read_csv("~/Hypertension/QOF Data/QOF_Hypertension_19-20.csv", skip = 12) %>%
  clean_names() %>%
  rename(ccg_code = ccg_ods_code_practice_parent,
         ccg_geography_code = ccg_ons_code_practice_parent,
         ccg_name = ccg_name_practice_parent,
         list_size_19_20 = list_size_11, 
         under79_19_20 = list_size_79,
         over80_19_20 = list_size_ages_80,
         register_19_20 = register_14, 
         prevalence_percent_19_20 = prevalence_percent_15,
         under79_numerator_19_20 = numerator_32, 
         under79_denominator_19_20 = denominator_33, 
         under79_achievement_net_exceptions_19_20 = net_of_pc_as_percent_34,
         under79_percent_receiving_intervention_19_20 = intervention_percent_38,
         over80_numerator_19_20 = numerator_40,
         over80_denominator_19_20 = denominator_41, 
         over80_achievement_net_exceptions_19_20 = net_of_pc_as_percent_42,
         over80_percent_receiving_intervention_19_20 = intervention_percent_46) %>%
  subset(., under79_denominator_19_20 != 0) %>%
  subset(., over80_denominator_19_20 != 0)

Hyper19_20$under79_achievement_net_exceptions_19_20 <- as.numeric(Hyper19_20$under79_achievement_net_exceptions_19_20)
Hyper19_20$under79_percent_receiving_intervention_19_20 <- as.numeric(Hyper19_20$under79_percent_receiving_intervention_19_20)
Hyper19_20$over80_achievement_net_exceptions_19_20 <- as.numeric(Hyper19_20$over80_achievement_net_exceptions_19_20)
Hyper19_20$over80_percent_receiving_intervention_19_20 <- as.numeric(Hyper19_20$over80_percent_receiving_intervention_19_20)

Hyper19_20_cl <- Hyper19_20 %>%
  subset(., select = c(ccg_code, practice_code, list_size_19_20, register_19_20, prevalence_percent_19_20, under79_numerator_19_20, 
                       under79_denominator_19_20, under79_achievement_net_exceptions_19_20, 
                       under79_percent_receiving_intervention_19_20, over80_numerator_19_20, over80_denominator_19_20, 
                       over80_achievement_net_exceptions_19_20, over80_percent_receiving_intervention_19_20))

Hyper19_20_grouped <- Hyper19_20 %>%
  group_by(ccg_code) %>%
  summarise(tot_list_size_19_20 = sum(list_size_19_20), 
            tot_register_19_20 = sum(register_19_20), 
            avg_prevalence_19_20 = mean(prevalence_percent_19_20), 
            tot_u79_numerator_19_20 = sum(under79_numerator_19_20), 
            tot_u79_denominator_19_20 = sum(under79_denominator_19_20),
            avg_u79_achievement_19_20 = mean(under79_achievement_net_exceptions_19_20), 
            avg_u79_intervention_19_20 = mean(under79_percent_receiving_intervention_19_20), 
            tot_o80_numerator_19_20 = sum(over80_numerator_19_20), 
            tot_o80_denominator_19_20 = sum(over80_denominator_19_20), 
            avg_o80_achievement_19_20 = mean(over80_achievement_net_exceptions_19_20), 
            avg_o80_intervention_19_20 = mean(over80_percent_receiving_intervention_19_20))

Hyper_compare <- full_join(Hyper19_20_cl, Hyper21_22_cl)
Hyper_compare$prev_diff <- Hyper_compare$prevalence_percent_19_20 - Hyper_compare$prevalence_percent_21_22

GP_unchanged <- Hyper_compare %>%
  filter(., prev_diff <= 1) %>%
  filter(., prev_diff >= -1)

### Hypertension Analysis ####
# checking the mean and median of hypertension prevalence by GP 
mean(lsoa_hyper_prev$prevalence_percent_21_22) # 12.61%
median(lsoa_hyper_prev$prevalence_percent_21_22) # 13.03%

lsoa_grouped <- lsoa_hyper_prev %>%
  group_by(lsoa_code) %>%
  summarise(hypertension_prevalence = sum(gp_coverage*prevalence_percent_21_22))
 
ccg_grouped <- merge(lsoa_grouped, lsoa_hyper_prev, by = 'lsoa_code') %>%
  subset(., select = c(lsoa_code, hypertension_prevalence, 
                       sub_icb_loc_ods_code, sub_icb_loc_ons_code, sub_icb_loc_name))

# Changing some CCG Codes to Match 2021 Codes 
ccg_grouped$sub_icb_loc_ons_code[ccg_grouped$sub_icb_loc_ons_code == "E38000258"] <- "E38000220"
ccg_grouped$sub_icb_loc_ons_code[ccg_grouped$sub_icb_loc_ons_code == "E38000259"] <- "E38000250"
ccg_grouped$sub_icb_loc_ons_code[ccg_grouped$sub_icb_loc_ons_code == "E38000260"] <- "E38000026"
ccg_grouped$sub_icb_loc_ons_code[ccg_grouped$sub_icb_loc_ons_code == "E38000261"] <- "E38000229"
ccg_grouped$sub_icb_loc_ons_code[ccg_grouped$sub_icb_loc_ons_code == "E38000262"] <- "E38000242"
ccg_grouped$sub_icb_loc_ons_code[ccg_grouped$sub_icb_loc_ons_code == "E38000263"] <- "E38000182" # NHS Tameside and Glossop to NHS Greater Manchester

# comparing mean and median in new df to data reported by GPs 
mean(lsoa_grouped$hypertension_prevalence) # 14.33%
median(lsoa_grouped$hypertension_prevalence) #14.57%

# Creating a Shapefile to plot prevalence by LSOA
hyper_prev_shp <- merge(ENG_LSOA11, lsoa_grouped, by.x = 'geo_code', by.y = 'lsoa_code')

tm_shape(hyper_prev_shp) + 
  tm_polygons(col = 'hypertension_prevalence', border.alpha = 0.5, title = "Hypertension Prevalence %", 
              legend.hist = TRUE) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence by LSOA', legend.outside = TRUE) 

lad_prevalence <- merge(lsoa_ccg_la, lsoa_grouped, by.x = 'LSOA11CD', by.y = 'lsoa_code') %>%
  group_by(LAD17CD) %>%
  summarise(hypertension_prevalence = mean(hypertension_prevalence))

lad_hypertension <- merge(ENG_LAD17, lad_prevalence, by.x = 'lad17cd', by.y = 'LAD17CD')

ldn_boro_hyper <- merge(London_LAD, lad_prevalence, by.x = 'GSS_CODE', by.y = 'LAD17CD')

# create a plot for London
london_hyper <- merge(London_2011, lsoa_grouped, by.x = 'LSOA11CD', by.y = 'lsoa_code')

tm_shape(london_hyper) + 
  tm_polygons(col = 'hypertension_prevalence', border.alpha = 0.5, title = "Hypertension Prevalence %", 
              legend.hist = TRUE, palette = "Blues") +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence by LSOA', legend.outside = TRUE) 

# Generate a Plot at LAD level 
tm_shape(lad_hypertension) +
  tm_polygons(col = 'hypertension_prevalence', border.alpha = 0.5, title = "Hypertension Prevalence %", 
              legend.hist = TRUE, palette = "Blues") +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence by Local Authority', legend.outside = TRUE) 

tm_shape(ldn_boro_hyper) +
  tm_polygons(col = 'hypertension_prevalence', border.alpha = 0.5, title = "Hypertension Prevalence %", 
              legend.hist = TRUE, palette = "Blues") +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence by Borough', legend.outside = TRUE) 

# Plot at CCG Level 
ccg_hyper <- merge(Eng_CCG, ccg_grouped, by.x = 'CCG21CD', by.y = 'sub_icb_loc_ons_code')

ggplot(ccg_hyper) + 
  aes(x = reorder(CCG21NM, -hypertension_prevalence), y = hypertension_prevalence, color = CCG21NM) + 
  geom_boxplot() + coord_flip() +
  geom_hline(aes(yintercept=mean(hypertension_prevalence)), color = 'black', 
             size = 1) +
  theme(legend.position = "none")

tm_shape(ccg_hyper) +
  tm_polygons(col = 'hypertension_prevalence', border.alpha = 0.5, title = "Hypertension Prevalence %", 
              legend.hist = TRUE, palette = "Blues") +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence by CCG', legend.outside = TRUE) 

#### Deprivation Analysis ####
lsoa_hyper_imd <- merge(lsoa_grouped, LSOA_imd_cl, by.x = 'lsoa_code', by.y = 'lsoa_code_2011')

la_hyper_imd <- merge(lsoa_hyper_imd, lad_region, by.x = 'local_authority_district_code_2019', by.y = 'LAD18CD')

# Investigating the relationship between IMD and Hypertension Prevalence 
ggplot(lsoa_hyper_imd, aes(x = imd_decile, y = hypertension_prevalence)) + 
  geom_point(aes(color = imd_decile))

# Seems there's little relationship

# Investigate by Region 
ggplot(la_hyper_imd, aes(x = income_score_rate, y = hypertension_prevalence)) + 
  geom_point(aes(color = RGN18NM)) +
  stat_smooth(method = 'lm', col = 'red', size = 1)

ggplot(la_hyper_imd) + 
  aes(x = hypertension_prevalence, color = RGN18NM, fill = RGN18NM) + 
  geom_density(alpha = 0.25)

lad_grouped <- la_hyper_imd %>%
  group_by(local_authority_district_code_2019) %>%
  summarise(hypertension_prevalence = sum(hypertension_prevalence),
            mean_imd_decile = mean(imd_decile), 
            region = RGN18NM)

ggplot(la_hyper_imd) + 
  aes(x = RGN18NM, fill = hypertension_prevalence) +
  geom_bar()

ggplot(la_hyper_imd) + 
  aes(x = reorder(RGN18NM, -hypertension_prevalence), y = hypertension_prevalence, color = RGN18NM) + 
  geom_boxplot() +
  coord_flip() +
  geom_hline(aes(yintercept=mean(hypertension_prevalence)), color = 'black', 
             size = 1)

# Investigate by LA 
london_hyper_imd <- merge(london_hyper, LSOA_imd_cl, by.x = 'LSOA11CD', by.y = 'lsoa_code_2011')

# London
ggplot(london_hyper_imd, aes(x = imd_score, y = hypertension_prevalence)) + 
  geom_point(aes(color = imd_decile))

ggplot(london_hyper_imd) + 
  aes(x = hypertension_prevalence, color = LAD11NM, fill = LAD11NM) + 
  geom_density(alpha = 0.25)

ggplot(london_hyper_imd) + 
  aes(x = LAD11NM, fill = hypertension_prevalence) +
  geom_bar()

ggplot(london_hyper_imd) + 
  aes(x = reorder(LAD11NM, -hypertension_prevalence), y = hypertension_prevalence, color = LAD11NM) + 
  geom_boxplot() + coord_flip() +
  geom_hline(aes(yintercept=mean(hypertension_prevalence)), color = 'black', 
             size = 0.6) 


# Investigate relationship between IMD and Hypertension Prevalence
ggplot(lsoa_hyper_imd, aes(x = imd_score, y = hypertension_prevalence)) + 
  geom_point(aes(color = imd_decile)) +
  stat_smooth(method = 'lm', col = 'red', size = 1)

# Create a second set Shapefile for Deprivation Prevalence 
imd_shp <- merge(ENG_LSOA11, LSOA_imd, by.x = 'geo_code', by.y = 'lsoa_code_2011') 

# analysing the relation between hypertension prevalence and IMD 


tm_shape(imd_shp) + 
  tm_polygons(col = 'imd_decile', legend.title = "Index of Multiple Deprivation Decile by LSOA") 


#### Objective 3 ####
#### Loading in QOF data from 2014-15 to 2019-20 ####
## 14-15 ##
Hyper14_15 <- read_csv("~/Hypertension/QOF Data/QOF_Hypertension_14-15.csv", skip = 2) %>%
  clean_names() %>%
  rename(list_size_14_15 = list_size_18,
         register_14_15 = register_19, 
         prevalence_percent_14_15 = prevalence_per_cent_20, 
         hyp006_numerator_14_15 = numerator, 
         hyp006_denominator_14_15 = denominator, 
         achievement_net_exceptions_14_15 = underlying_achievement_net_of_exceptions_per_cent,
         percent_receiving_intervention_14_15 = percentage_of_patients_receiving_intervention) 

Hyper14_15_cl <- Hyper14_15 %>%
  subset(., select = c(ccg_code, list_size_14_15, register_14_15, prevalence_percent_14_15, hyp006_numerator_14_15, 
                       hyp006_denominator_14_15, achievement_net_exceptions_14_15, percent_receiving_intervention_14_15)) %>%
  group_by(ccg_code) %>%
  summarise(tot_list_size_14_15 = sum(list_size_14_15), 
            tot_register_14_15 = sum(register_14_15), 
            avg_prevalence_14_15 = mean(prevalence_percent_14_15), 
            tot_numerator_14_15 = sum(hyp006_numerator_14_15), 
            tot_denominator_14_15 = sum(hyp006_denominator_14_15),
            avg_achievement_14_15 = mean(achievement_net_exceptions_14_15), 
            avg_intervention_14_15 = mean(percent_receiving_intervention_14_15))

## 15-16 ##
Hyper15_16 <- read_csv("~/Hypertension/QOF Data/QOF_Hypertension_15-16.csv", skip = 8) %>%
  clean_names() %>%
  rename(list_size_15_16 = list_size_15, 
         register_15_16 = register_16, 
         prevalence_percent_15_16 = prevalence_per_cent_17,
         hyp006_numerator_15_16 = numerator, 
         hyp006_denominator_15_16 = denominator, 
         achievement_net_exceptions_15_16 = underlying_achievement_net_of_exceptions_per_cent,
         percent_receiving_intervention_15_16 = patients_receiving_intervention_per_cent)

Hyper15_16_cl <- Hyper15_16 %>%
  subset(., select = c(ccg_code, list_size_15_16, register_15_16, prevalence_percent_15_16, hyp006_numerator_15_16, 
                       hyp006_denominator_15_16, achievement_net_exceptions_15_16, percent_receiving_intervention_15_16)) %>%
  group_by(ccg_code) %>%
  summarise(tot_list_size_15_16 = sum(list_size_15_16), 
            tot_register_15_16 = sum(register_15_16), 
            avg_prevalence_15_16 = mean(prevalence_percent_15_16), 
            tot_numerator_15_16 = sum(hyp006_numerator_15_16), 
            tot_denominator_15_16 = sum(hyp006_denominator_15_16),
            avg_achievement_15_16 = mean(achievement_net_exceptions_15_16), 
            avg_intervention_15_16 = mean(percent_receiving_intervention_15_16))

## 16-17 ##
Hyper16_17 <- read_csv("~/Hypertension/QOF Data/QOF_Hypertension_16-17.csv", skip = 8) %>%
  clean_names() %>%
  rename(list_size_16_17 = list_size_17, 
         register_16_17 = register_18, 
         prevalence_percent_16_17 = prevalence_per_cent_19,
         hyp006_numerator_16_17 = numerator, 
         hyp006_denominator_16_17 = denominator, 
         achievement_net_exceptions_16_17 = underlying_achievement_net_of_exceptions_per_cent,
         percent_receiving_intervention_16_17 = patients_receiving_intervention_per_cent) %>%
  subset(., hyp006_denominator_16_17 != 0) # removing rows where there is a zero in the denominator

Hyper16_17$percent_receiving_intervention_16_17 <- as.numeric(Hyper16_17$percent_receiving_intervention_16_17)
Hyper16_17$achievement_net_exceptions_16_17 <- as.numeric(Hyper16_17$achievement_net_exceptions_16_17)

Hyper16_17_cl <- Hyper16_17 %>%
  subset(., select = c(ccg_code, list_size_16_17, register_16_17, prevalence_percent_16_17, hyp006_numerator_16_17, 
                       hyp006_denominator_16_17, achievement_net_exceptions_16_17, percent_receiving_intervention_16_17))%>%
  group_by(ccg_code) %>%
  summarise(tot_list_size_16_17 = sum(list_size_16_17), 
            tot_register_16_17 = sum(register_16_17), 
            avg_prevalence_16_17 = mean(prevalence_percent_16_17), 
            tot_numerator_16_17 = sum(hyp006_numerator_16_17), 
            tot_denominator_16_17 = sum(hyp006_denominator_16_17),
            avg_achievement_16_17 = mean(achievement_net_exceptions_16_17), 
            avg_intervention_16_17 = mean(percent_receiving_intervention_16_17))

## 17-18 ##
Hyper17_18 <- read_csv("~/Hypertension/QOF Data/QOF_Hypertension_17-18.csv", skip = 8) %>%
  clean_names() %>%
  rename(list_size_17_18 = list_size_17, 
         register_17_18 = register_18, 
         prevalence_percent_17_18 = prevalence_per_cent_19,
         hyp006_numerator_17_18 = numerator, 
         hyp006_denominator_17_18 = denominator, 
         achievement_net_exceptions_17_18 = underlying_achievement_net_of_exceptions_per_cent,
         percent_receiving_intervention_17_18 = patients_receiving_intervention_per_cent) %>%
  subset(., hyp006_denominator_17_18 != 0)

Hyper17_18$percent_receiving_intervention_17_18 <- as.numeric(Hyper17_18$percent_receiving_intervention_17_18)
Hyper17_18$achievement_net_exceptions_17_18 <- as.numeric(Hyper17_18$achievement_net_exceptions_17_18)

Hyper17_18_cl <- Hyper17_18 %>%
  subset(., select = c(ccg_code, list_size_17_18, register_17_18, prevalence_percent_17_18, hyp006_numerator_17_18, 
                       hyp006_denominator_17_18, achievement_net_exceptions_17_18, percent_receiving_intervention_17_18))%>%
  group_by(ccg_code) %>%
  summarise(tot_list_size_17_18 = sum(list_size_17_18), 
            tot_register_17_18 = sum(register_17_18), 
            avg_prevalence_17_18 = mean(prevalence_percent_17_18), 
            tot_numerator_17_18 = sum(hyp006_numerator_17_18), 
            tot_denominator_17_18 = sum(hyp006_denominator_17_18),
            avg_achievement_17_18 = mean(achievement_net_exceptions_17_18), 
            avg_intervention_17_18 = mean(percent_receiving_intervention_17_18))


## 18-19 ##
Hyper18_19 <- read_csv("~/Hypertension/QOF Data/QOF_Hypertension_18-19.csv", skip = 11) %>%
  clean_names() %>%
  rename(list_size_18_19 = list_size_17, 
         register_18_19 = register_18, 
         prevalence_percent_18_19 = prevalence_percent_19,
         hyp006_numerator_18_19 = numerator, 
         hyp006_denominator_18_19 = denominator, 
         achievement_net_exceptions_18_19 = net_of_exceptions_percent,
         percent_receiving_intervention_18_19 = intervention_percent,
         ccg_geography_code = ccg_ons_code) %>%
  subset(., hyp006_denominator_18_19 != 0)

Hyper18_19_cl <- Hyper18_19 %>%
  subset(., select = c(ccg_code, list_size_18_19, register_18_19, prevalence_percent_18_19, hyp006_numerator_18_19, 
                       hyp006_denominator_18_19, achievement_net_exceptions_18_19, percent_receiving_intervention_18_19)) %>%
  group_by(ccg_code) %>%
  summarise(tot_list_size_18_19 = sum(list_size_18_19), 
            tot_register_18_19 = sum(register_18_19), 
            avg_prevalence_18_19 = mean(prevalence_percent_18_19), 
            tot_numerator_18_19 = sum(hyp006_numerator_18_19), 
            tot_denominator_18_19 = sum(hyp006_denominator_18_19),
            avg_achievement_18_19 = mean(achievement_net_exceptions_18_19), 
            avg_intervention_18_19 = mean(percent_receiving_intervention_18_19))

Compare_18_19 <- full_join(Hyper18_19, Hyper19_20_cl)
Compare_18_19$prev_diff <- Compare_18_19$prevalence_percent_19_20 - Compare_18_19$prevalence_percent_18_19

GP_unchanged_18_19 <- Compare_18_19 %>%
  filter(., prev_diff <= 0.1) %>%
  filter(., prev_diff >= -0.1)

# Load in Data to Change the CCG Codes
ccg_2019_codes <- read_csv("merges_up_to_2019.csv") %>%
  clean_names()
ccg_2020_codes <- read_csv("merges_2020.csv") %>%
  clean_names()
ccg_2021_codes <- read_csv("merges_2021.csv") %>%
  clean_names()

#### Merging Data ####
# Merging the two GP datasets
GP_data <- merge(GP_age_dist_wide, GP_imd_cl, all = TRUE) %>%
  rename(imd_value = value)


# Mergining GP Data to Shapefile 
GP_shp <- merge(Eng_CCG, GP_data, by.x = c('CCG21CD'), by.y = c('parent_code'))

# Merging by CCG 
QOF_16 <- merge(Hyper14_15_cl, Hyper15_16_cl, by = "ccg_code", all = TRUE)

QOF_17 <- merge(QOF_16, Hyper16_17_cl, by = 'ccg_code', all = TRUE)
# Changing CCG codes as CCGs merge
QOF_17$ccg_code[QOF_17$ccg_code == "00W"] <- "14L"
QOF_17$ccg_code[QOF_17$ccg_code == "01M"] <- "14L"
QOF_17$ccg_code[QOF_17$ccg_code == "01N"] <- "14L"

QOF_18 <- merge(QOF_17, Hyper17_18_cl, by = 'ccg_code', all = TRUE)
# Changing more CCG Codes - 14Y 
# Note: 14Y does not appear until 2018/19, as Buckinghamshire CCG data is excluded for the 2017/18 year due to small sample size 
#       and participation in an alternative payment scheme 
QOF_18$ccg_code[QOF_18$ccg_code == "10H"] <- "14Y"
QOF_18$ccg_code[QOF_18$ccg_code == "10Y"] <- "14Y"

# Changing more CCG Codes - 15A
QOF_18$ccg_code[QOF_18$ccg_code == "10M"] <- "15A"
QOF_18$ccg_code[QOF_18$ccg_code == "10N"] <- "15A"
QOF_18$ccg_code[QOF_18$ccg_code == "10W"] <- "15A"
QOF_18$ccg_code[QOF_18$ccg_code == "11D"] <- "15A"
# 15C
QOF_18$ccg_code[QOF_18$ccg_code == "11H"] <- "15C"
QOF_18$ccg_code[QOF_18$ccg_code == "11T"] <- "15C"
QOF_18$ccg_code[QOF_18$ccg_code == "12A"] <- "15C"
# 15D
QOF_18$ccg_code[QOF_18$ccg_code == "10G"] <- "15D"
QOF_18$ccg_code[QOF_18$ccg_code == "10T"] <- "15D"
QOF_18$ccg_code[QOF_18$ccg_code == "11C"] <- "15D"
# 15E
QOF_18$ccg_code[QOF_18$ccg_code == "04X"] <- "15E"
QOF_18$ccg_code[QOF_18$ccg_code == "05P"] <- "15E"
QOF_18$ccg_code[QOF_18$ccg_code == "13P"] <- "15E"
# 15F
QOF_18$ccg_code[QOF_18$ccg_code == "02V"] <- "15F"
QOF_18$ccg_code[QOF_18$ccg_code == "03C"] <- "15F"
QOF_18$ccg_code[QOF_18$ccg_code == "03G"] <- "15F"

QOF_19 <- merge(QOF_18, Hyper18_19_cl, by = 'ccg_code', all = TRUE)
# 15M
QOF_19$ccg_code[QOF_19$ccg_code == "03X"] <- "15M"
QOF_19$ccg_code[QOF_19$ccg_code == "03Y"] <- "15M"
QOF_19$ccg_code[QOF_19$ccg_code == "04J"] <- "15M"
QOF_19$ccg_code[QOF_19$ccg_code == "04R"] <- "15M"
# 15N
QOF_19$ccg_code[QOF_19$ccg_code == "99P"] <- "15N"
QOF_19$ccg_code[QOF_19$ccg_code == "99Q"] <- "15N"

QOF_19_data <- merge(QOF_19, ccg_2020_codes, by.x = 'ccg_code', by.y = 'old_code', all = TRUE)

QOF_19_to_20 <- QOF_19_data %>%
  mutate(., new_code = case_when(ccg_code != QOF_19_data$new_code ~ QOF_19_data$new_code, 
                                 TRUE ~ QOF_19_data$ccg_code)) %>%
  subset(., select = -c(ccg_code))

QOF_20 <- merge(QOF_19_to_20, Hyper19_20_grouped, by.x = "new_code", by.y = 'ccg_code', all = TRUE) 

QOF_cl <- QOF_20[-219,] # Clean and Finalised Dataset of 5 years of QOF Data

QOF_data <- QOF_cl %>% 
  rename(ccg_code = new_code) %>%
  group_by(ccg_code) %>%
  summarise(tot_list_size_14_15 = sum(tot_list_size_14_15, na.rm = TRUE), 
            tot_register_14_15 = sum(tot_register_14_15, na.rm = TRUE), 
            avg_prevalence_14_15 = mean(avg_prevalence_14_15, na.rm = TRUE), 
            tot_numerator_14_15 = sum(tot_numerator_14_15, na.rm = TRUE), 
            tot_denominator_14_15 = sum(tot_denominator_14_15, na.rm = TRUE),
            avg_achievement_14_15 = mean(avg_achievement_14_15, na.rm = TRUE), 
            avg_intervention_14_15 = mean(avg_intervention_14_15, na.rm = TRUE), 
            tot_list_size_15_16 = sum(tot_list_size_15_16, na.rm = TRUE), 
            tot_register_15_16 = sum(tot_register_15_16, na.rm = TRUE), 
            avg_prevalence_15_16 = mean(avg_prevalence_15_16, na.rm = TRUE), 
            tot_numerator_15_16 = sum(tot_numerator_15_16, na.rm = TRUE), 
            tot_denominator_15_16 = sum(tot_denominator_15_16, na.rm = TRUE),
            avg_achievement_15_16 = mean(avg_achievement_15_16, na.rm = TRUE), 
            avg_intervention_15_16 = mean(avg_intervention_15_16, na.rm = TRUE),
            tot_list_size_16_17 = sum(tot_list_size_16_17, na.rm = TRUE), 
            tot_register_16_17 = sum(tot_register_16_17, na.rm = TRUE), 
            avg_prevalence_16_17 = mean(avg_prevalence_16_17, na.rm = TRUE), 
            tot_numerator_16_17 = sum(tot_numerator_16_17, na.rm = TRUE), 
            tot_denominator_16_17 = sum(tot_denominator_16_17, na.rm = TRUE),
            avg_achievement_16_17 = mean(avg_achievement_16_17, na.rm = TRUE), 
            avg_intervention_16_17 = mean(avg_intervention_16_17, na.rm = TRUE),
            tot_list_size_17_18 = sum(tot_list_size_17_18, na.rm = TRUE), 
            tot_register_17_18 = sum(tot_register_17_18, na.rm = TRUE), 
            avg_prevalence_17_18 = mean(avg_prevalence_17_18, na.rm = TRUE), 
            tot_numerator_17_18 = sum(tot_numerator_17_18, na.rm = TRUE), 
            tot_denominator_17_18 = sum(tot_denominator_17_18, na.rm = TRUE),
            avg_achievement_17_18 = mean(avg_achievement_17_18, na.rm = TRUE), 
            avg_intervention_17_18 = mean(avg_intervention_17_18, na.rm = TRUE),
            tot_list_size_18_19 = sum(tot_list_size_18_19, na.rm = TRUE), 
            tot_register_18_19 = sum(tot_register_18_19, na.rm = TRUE), 
            avg_prevalence_18_19 = mean(avg_prevalence_18_19, na.rm = TRUE), 
            tot_numerator_18_19 = sum(tot_numerator_18_19, na.rm = TRUE), 
            tot_denominator_18_19 = sum(tot_denominator_18_19, na.rm = TRUE),
            avg_achievement_18_19 = mean(avg_achievement_18_19, na.rm = TRUE), 
            avg_intervention_18_19 = mean(avg_intervention_18_19, na.rm = TRUE),
            tot_list_size_19_20 = sum(tot_list_size_19_20, na.rm = TRUE), 
            tot_register_19_20 = sum(tot_register_19_20, na.rm = TRUE), 
            avg_prevalence_19_20 = mean(avg_prevalence_19_20, na.rm = TRUE), 
            tot_u79_numerator_19_20 = sum(tot_u79_numerator_19_20, na.rm = TRUE), 
            tot_u79_denominator_19_20 = sum(tot_u79_denominator_19_20, na.rm = TRUE),
            avg_u79_achievement_19_20 = mean(avg_u79_achievement_19_20, na.rm = TRUE), 
            avg_u79_intervention_19_20 = mean(avg_u79_intervention_19_20, na.rm = TRUE), 
            tot_o80_numerator_19_20 = sum(tot_o80_numerator_19_20, na.rm = TRUE), 
            tot_o80_denominator_19_20 = sum(tot_o80_denominator_19_20, na.rm = TRUE), 
            avg_o80_achievement_19_20 = mean(avg_o80_achievement_19_20, na.rm = TRUE), 
            avg_o80_intervention_19_20 = mean(avg_o80_intervention_19_20, na.rm = TRUE))

