# set working directory 
setwd("~/Hypertension")

#### Installing Packages ####
library(tidyverse)
library(tmap)
library(sf)
library(janitor)
library(stringr)
library(ggplot2)


#### Loading Data #####
# Loading in Shapefile
Eng_CCG <- st_read("~/Hypertension/CCG Shapefiles/CCG_APR_2021_EN_BFC.shp")

# Renaming and Recoding CCG labels following the most recent changes to CCG/ICB names
# Northamptonshire
Eng_CCG$CCG21CD[Eng_CCG$CCG21CD=="E38000242"] <- "E38000262"
Eng_CCG$CCG21NM[Eng_CCG$CCG21NM=="NHS Northamptonshire CCG"] <- "NHS Northamptonshire ICB"

# Greater Manchester 
Eng_CCG$CCG21CD[Eng_CCG$CCG21CD=="E38000182"] <- "E38000263"
Eng_CCG$CCG21NM[Eng_CCG$CCG21NM=="NHS Tameside and Glossop CCG"] <- "NHS Greater Manchester ICB"

# Cambridgeshire and Peterborough 
Eng_CCG$CCG21CD[Eng_CCG$CCG21CD=="E38000026"] <- "E38000260"
Eng_CCG$CCG21NM[Eng_CCG$CCG21NM=="NHS Cambridgeshire and Peterborough CCG"] <- "NHS Cambridgeshire and Peterborough ICB"

# Derby and Derbyshire 
Eng_CCG$CCG21CD[Eng_CCG$CCG21CD=="E38000229"] <- "E38000261"
Eng_CCG$CCG21NM[Eng_CCG$CCG21NM=="NHS Derby and Derbyshire CCG"] <- "NHS Derby and Derbyshire ICB"

# Black Country
Eng_CCG$CCG21CD[Eng_CCG$CCG21CD=="E38000250"] <- "E38000259"
Eng_CCG$CCG21NM[Eng_CCG$CCG21NM=="NHS Black Country and West Birmingham CCG"] <- "NHS Black Country ICB"

# Birmingham and Solihull
Eng_CCG$CCG21CD[Eng_CCG$CCG21CD=="E38000220"] <- "E38000258"
Eng_CCG$CCG21NM[Eng_CCG$CCG21NM=="NHS Birmingham and Solihull CCG"] <- "NHS Birmingham and Solihull ICB"


ENG_LSOA21 <- st_read("~/Hypertension/LSOA Shapefiles/LSOA_2021_EW_BFE_V5.shp")
ENG_LSOA11 <- st_read("~/Hypertension/LSOA Shapefiles/infuse_lsoa_lyr_2011.shp") %>% 
  filter(str_detect(geo_code, '^E'))
London_2011 <- st_read("~/Hypertension/LSOA Shapefiles/statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp")

# Loading in GP Data
GP_age_dist <- read_csv("~/Hypertension/Population Age Distributions/gp-reg-pat-prac-quin-age-sep-22.csv") %>%
  clean_names()

#### Age Standardisation ####
GP_age_dist_wide <- GP_age_dist %>% 
  filter(org_type == 'GP') %>%
  pivot_wider(names_from = c(age_group_5, sex), 
              values_from = number_of_patients) %>%
  clean_names() %>%
  # summing to total number of males and females in each age category 
  mutate(male0_15 = rowSums(select(., x0_4_male:x10_14_male)), 
         male16_24 = rowSums(select(., x15_19_male:x20_24_male)), 
         male25_34 = rowSums(select(., x25_29_male:x30_34_male)), 
         male35_44 = rowSums(select(., x35_39_male:x40_44_male)), 
         male45_54 = rowSums(select(., x45_49_male:x50_54_male)), 
         male55_64 = rowSums(select(., x55_59_male:x60_64_male)), 
         male65_74 = rowSums(select(., x65_69_male:x70_74_male)), 
         male75 = rowSums(select(., x75_79_male)), 
         male80plus =rowSums(select(., x80_84_male:x95_male)), 
         female0_15 = rowSums(select(., x0_4_female:x10_14_female)), 
         female16_24 = rowSums(select(., x15_19_female:x20_24_female)), 
         female25_34 = rowSums(select(., x25_29_female:x30_34_female)), 
         female35_44 = rowSums(select(., x35_39_female:x40_44_female)), 
         female45_54 = rowSums(select(., x45_49_female:x50_54_female)), 
         female55_64 = rowSums(select(., x55_59_female:x60_64_female)), 
         female65_74 = rowSums(select(., x65_69_female:x70_74_female)), 
         female75 = rowSums(select(., x75_79_female)), 
         female80plus = rowSums(select(., x80_84_female:x95_female)))

GP_age_dist_cl <- GP_age_dist_wide %>%
  select(org_code, all_female, all_male, all_all, male0_15:female80plus) %>%
  # change the total number of males and females into percentages
  mutate(male0_15_perc = male0_15/all_male, 
         male16_24_perc = male16_24/all_male, 
         male25_34_perc = male25_34/all_male, 
         male35_44_perc = male35_44/all_male, 
         male45_54_perc = male45_54/all_male, 
         male55_64_perc = male55_64/all_male, 
         male65_74_perc = male65_74/all_male, 
         male75_79_perc = male75/all_male, 
         male80plus_perc = male80plus/all_male,
         female0_15_perc = female0_15/all_female, 
         female16_24_perc = female16_24/all_female, 
         female25_34_perc = female25_34/all_female, 
         female35_44_perc = female35_44/all_female, 
         female45_54_perc = female45_54/all_female, 
         female55_64_perc = female55_64/all_female, 
         female65_74_perc = female65_74/all_female, 
         female75_79_perc = female75/all_female, 
         female80plus_perc = female80plus/all_female)

# calculating the expected hypertension prevalence by age group
GP_age_dist_cl <- GP_age_dist_cl %>%
  mutate(exp_hyp_16_24_male = male16_24_perc*0.01, 
         exp_hyp_25_34_male = male25_34_perc*0.01, 
         exp_hyp_35_44_male = male35_44_perc*0.03, 
         exp_hyp_45_54_male = male45_54_perc*0.10, 
         exp_hyp_55_64_male = male55_64_perc*0.26, 
         exp_hyp_65_74_male = male65_74_perc*0.38,
         exp_hyp_75_79_male = male75_79_perc*0.53,
         exp_hyp_80plus_male = male80plus_perc*0.53,
         exp_hyp_16_24_female = female16_24_perc*0.00, 
         exp_hyp_25_34_female = female25_34_perc*0.01, 
         exp_hyp_35_44_female = female35_44_perc*0.02, 
         exp_hyp_45_54_female = female45_54_perc*0.09, 
         exp_hyp_55_64_female = female55_64_perc*0.19, 
         exp_hyp_65_74_female = female65_74_perc*0.32,
         exp_hyp_75_79_female = female75_79_perc*0.46, 
         exp_hyp_80plus_female = female80plus_perc*0.46)

# calculating the total expected hypertension by GP 
GP_age_dist_cl <- GP_age_dist_cl %>%
  mutate(exp_hyp_male = exp_hyp_16_24_male + exp_hyp_25_34_male + exp_hyp_35_44_male + exp_hyp_45_54_male + 
           exp_hyp_55_64_male + exp_hyp_65_74_male + exp_hyp_75_79_male + exp_hyp_80plus_male, 
         exp_u79_hyp_male = exp_hyp_16_24_male + exp_hyp_25_34_male + exp_hyp_35_44_male + 
           exp_hyp_45_54_male +  exp_hyp_55_64_male + exp_hyp_65_74_male + exp_hyp_75_79_male,
         exp_hyp_female = exp_hyp_16_24_female + exp_hyp_25_34_female + exp_hyp_35_44_female + exp_hyp_45_54_female +
           exp_hyp_55_64_female + exp_hyp_65_74_female + exp_hyp_75_79_female + exp_hyp_80plus_female, 
         exp_u79_hyp_female = exp_hyp_16_24_female + exp_hyp_25_34_female + exp_hyp_35_44_female + 
           exp_hyp_45_54_female + exp_hyp_55_64_female + exp_hyp_65_74_female + exp_hyp_75_79_female,
         perc_male = all_male/all_all, 
         perc_female = all_female/all_all, 
         male_u79_perc = (all_male-male80plus)/all_all, 
         male_o80_perc = male80plus/all_all, 
         female_u79_perc = (all_female-female80plus)/all_all, 
         female_o80_perc = female80plus/all_all)

LSOA_imd <- read_csv("LSOA_IMD_Scores_Deciles.csv") %>%
  clean_names() %>%
  rename(imd_decile = index_of_multiple_deprivation_imd_decile_where_1_is_most_deprived_10_percent_of_lso_as, 
         imd_score = index_of_multiple_deprivation_imd_score) 

LSOA_imd_cl <- LSOA_imd %>%
  subset(., select =c(lsoa_code_2011, lsoa_name_2011, local_authority_district_code_2019, local_authority_district_name_2019, 
                      imd_score, imd_decile, income_score_rate, employment_score_rate, education_skills_and_training_score, 
                      health_deprivation_and_disability_score, crime_score, living_environment_score, 
                      barriers_to_housing_and_services_score))

# Loading in LSOA data from July 2022
GP_lsoa_dist <- read_csv("~/Hypertension/GP Data/gp-reg-pat-prac-lsoa-all.csv") %>%
  clean_names() %>%
  subset(., select = c(practice_code, practice_name, lsoa_code, number_of_patients))

GP_lsoa_data <- GP_lsoa_dist %>% 
  group_by(lsoa_code) %>%
  summarise(lsoa_pop = sum(number_of_patients), 
            practice_code = practice_code, 
            number_of_patients = number_of_patients) %>%
  filter(grepl("E", lsoa_code))

# Loading in some files for aggregating geography upwards
lsoa_ccg_la <- read_csv("LSOA_to_CCG_to_LAD_(April_2021)_Lookup_in_England.csv")

# Renaming and Recoding CCG labels following the most recent changes to CCG/ICB names
# Northamptonshire
lsoa_ccg_la$CCG21CD[lsoa_ccg_la$CCG21CD=="E38000242"] <- "E38000262"
lsoa_ccg_la$CCG21NM[lsoa_ccg_la$CCG21NM=="NHS Northamptonshire CCG"] <- "NHS Northamptonshire ICB"

# Greater Manchester 
lsoa_ccg_la$CCG21CD[lsoa_ccg_la$CCG21CD=="E38000182"] <- "E38000263"
lsoa_ccg_la$CCG21NM[lsoa_ccg_la$CCG21NM=="NHS Tameside and Glossop CCG"] <- "NHS Greater Manchester ICB"

# Cambridgeshire and Peterborough 
lsoa_ccg_la$CCG21CD[lsoa_ccg_la$CCG21CD=="E38000026"] <- "E38000260"
lsoa_ccg_la$CCG21NM[lsoa_ccg_la$CCG21NM=="NHS Cambridgeshire and Peterborough CCG"] <- "NHS Cambridgeshire and Peterborough ICB"

# Derby and Derbyshire 
lsoa_ccg_la$CCG21CD[lsoa_ccg_la$CCG21CD=="E38000229"] <- "E38000261"
lsoa_ccg_la$CCG21NM[lsoa_ccg_la$CCG21NM=="NHS Derby and Derbyshire CCG"] <- "NHS Derby and Derbyshire ICB"

# Black Country
lsoa_ccg_la$CCG21CD[lsoa_ccg_la$CCG21CD=="E38000250"] <- "E38000259"
lsoa_ccg_la$CCG21NM[lsoa_ccg_la$CCG21NM=="NHS Black Country and West Birmingham CCG"] <- "NHS Black Country ICB"

# Birmingham and Solihull
lsoa_ccg_la$CCG21CD[lsoa_ccg_la$CCG21CD=="E38000220"] <- "E38000258"
lsoa_ccg_la$CCG21NM[lsoa_ccg_la$CCG21NM=="NHS Birmingham and Solihull CCG"] <- "NHS Birmingham and Solihull ICB"

ccg_region <- read_csv("CCG_to_STP_and_NHS_England_(Region)_(April_2021)_Lookup_in_England.csv") %>%
  subset(., select = c(CCG21CD, CCG21CDH, CCG21NM, NHSER21CD, NHSER21NM))

# Renaming and Recoding CCG labels following the most recent changes to CCG/ICB names
# Northamptonshire
ccg_region$CCG21CD[ccg_region$CCG21CD=="E38000242"] <- "E38000262"
ccg_region$CCG21NM[ccg_region$CCG21NM=="NHS Northamptonshire CCG"] <- "NHS Northamptonshire ICB"

# Greater Manchester 
ccg_region$CCG21CD[ccg_region$CCG21CD=="E38000182"] <- "E38000263"
ccg_region$CCG21NM[ccg_region$CCG21NM=="NHS Tameside and Glossop CCG"] <- "NHS Greater Manchester ICB"

# Cambridgeshire and Peterborough 
ccg_region$CCG21CD[ccg_region$CCG21CD=="E38000026"] <- "E38000260"
ccg_region$CCG21NM[ccg_region$CCG21NM=="NHS Cambridgeshire and Peterborough CCG"] <- "NHS Cambridgeshire and Peterborough ICB"

# Derby and Derbyshire 
ccg_region$CCG21CD[ccg_region$CCG21CD=="E38000229"] <- "E38000261"
ccg_region$CCG21NM[ccg_region$CCG21NM=="NHS Derby and Derbyshire CCG"] <- "NHS Derby and Derbyshire ICB"

# Black Country
ccg_region$CCG21CD[ccg_region$CCG21CD=="E38000250"] <- "E38000259"
ccg_region$CCG21NM[ccg_region$CCG21NM=="NHS Black Country and West Birmingham CCG"] <- "NHS Black Country ICB"

# Birmingham and Solihull
ccg_region$CCG21CD[ccg_region$CCG21CD=="E38000220"] <- "E38000258"
ccg_region$CCG21NM[ccg_region$CCG21NM=="NHS Birmingham and Solihull CCG"] <- "NHS Birmingham and Solihull ICB"

#### Analysis #####
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
                       over80_percent_receiving_intervention_21_22)) %>%
  mutate(over80_prev = over80_denominator_21_22/over80_21_22, 
         under79_prev = under79_denominator_21_22/under79_21_22)

Hyper21_22_cl$over80_achievement_net_exceptions_21_22 <- as.numeric(Hyper21_22_cl$over80_achievement_net_exceptions_21_22)
Hyper21_22_cl$over80_percent_receiving_intervention_21_22 <- as.numeric(Hyper21_22_cl$over80_percent_receiving_intervention_21_22)

GP_lsoa_age_data <- merge(GP_age_dist_cl, GP_lsoa_data, by.x = 'org_code', by.y = 'practice_code')

lsoa_hyper_prev <- merge(GP_lsoa_age_data, Hyper21_22_cl, by.x = 'org_code', by.y = 'practice_code') %>%
  # Create new column for the percentage of patients a GP serves in each LSOA 
  mutate(gp_coverage = (number_of_patients/lsoa_pop), 
         total80plus = male80plus + female80plus,
         tot_o80 = (number_of_patients/all_all)*total80plus,
         exp_hyp = (exp_hyp_male*perc_male + exp_hyp_female*perc_female)*100, 
         exp_u79_hyp = (exp_u79_hyp_male*male_u79_perc + exp_u79_hyp_female*female_u79_perc)*100)

gp_coverage <- lsoa_hyper_prev %>%
  select(org_code, lsoa_code, lsoa_pop, number_of_patients, practice_name, 
         gp_coverage, prevalence_percent_21_22, exp_hyp)
gp_cov90 <- gp_coverage %>%
  filter(gp_coverage >= 0.9)

lsoa_med_age <- read_csv("~/Hypertension/Population Age Distributions/lsoa_median_age.csv", skip = 4) %>%
  clean_names()

gp_cov90_age_dist <- merge(gp_cov90, lsoa_med_age, by = 'lsoa_code') %>%
  select_if(~ !any(is.na(.))) %>%
  select(-c(la_code_2018_boundaries:la_name_2021_boundaries)) %>%
  get_dupes(practice_name)

diff_5plus <- gp_cov90_age_dist %>%
  mutate(age_diff = median_age - lead(median_age)) 

diff_filtered <- diff_5plus %>%
  filter(age_diff <= -5 & practice_name == lead(practice_name) | 
           age_diff >= 5 & practice_name == lead(practice_name)) 

diff_10plus <- diff_5plus %>%
  filter(age_diff <= -10 & practice_name == lead(practice_name) | 
           age_diff >= 10 & practice_name == lead(practice_name)) 

exp_diff5plus <- merge(diff_10plus, lsoa_age_dist_5yr, by = c('lsoa_code', 'lsoa_name')) %>%
  select(-c(age0_15:age75plus_perc, starts_with('la'))) %>%
  mutate(exp_diff = exp_hyp - exp_hyp_no_sex*100) %>%
  filter(exp_diff <= -5 | exp_diff >= 5)

obs_diff5plus <-  merge(diff_10plus, lsoa_age_dist_5yr, by = c('lsoa_code', 'lsoa_name')) %>%
  select(-c(age0_15:age75plus_perc, starts_with('la'))) %>%
  mutate(obs_diff = prevalence_percent_21_22 - exp_hyp_no_sex*100) %>%
  filter(obs_diff <= -5 | obs_diff >= 5)

### Hypertension Analysis ####
# checking the mean and median of hypertension prevalence by GP 
mean(lsoa_hyper_prev$prevalence_percent_21_22) # 12.61%
median(lsoa_hyper_prev$prevalence_percent_21_22) # 13.03%

# aggregating prevalence from practice to lsoa level
lsoa_grouped <- lsoa_hyper_prev %>%
  group_by(lsoa_code) %>%
  summarise(obs_hyper_prev = sum(gp_coverage*prevalence_percent_21_22),
            obs_u79_prev = sum(gp_coverage*under79_prev)*100,
            obs_o80_prev = sum(gp_coverage*over80_prev)*100,
            exp_hyper_prev = sum(exp_hyp*gp_coverage),
            exp_u79_prev = sum(gp_coverage*exp_u79_hyp), 
            lsoa_pop = mean(lsoa_pop), 
            over80_pop = sum(tot_o80),
            u79_achievement = sum(gp_coverage*under79_achievement_net_exceptions_21_22), 
            u79_intervention = sum(gp_coverage*under79_percent_receiving_intervention_21_22), 
            o80_achievement = sum(gp_coverage*over80_achievement_net_exceptions_21_22), 
            o80_intervention = sum(gp_coverage*over80_percent_receiving_intervention_21_22))

# merging with expected hypertension rates for males and females 
lsoa_age_adj <- lsoa_grouped %>%
  mutate(obs_over_exp = obs_hyper_prev/exp_hyper_prev,
         u79_obs_over_exp = obs_u79_prev/exp_u79_prev,
         age_std_prev = mean(obs_hyper_prev)*obs_over_exp, 
         age_std_u79_prev = mean(obs_u79_prev)*u79_obs_over_exp)

# Merging at CCG 
ccg_grouped <- merge(lsoa_age_adj, lsoa_ccg_la, by.x = 'lsoa_code', by.y = 'LSOA11CD') 

# comparing mean and median in new df to data reported by GPs 
mean(lsoa_grouped$obs_hyper_prev) # 14.30%
median(lsoa_grouped$obs_hyper_prev) #14.55%

# Age Standardised Dataframe 
mean(lsoa_age_adj$age_std_prev) # 15.11

#### Plotting ####
# Creating a Shapefile to plot prevalence by LSOA
hyper_prev_shp <- merge(ENG_LSOA11, lsoa_age_adj, by.x = 'geo_code', by.y = 'lsoa_code')

tm_shape(hyper_prev_shp) + 
  tm_polygons(col = 'age_std_prev', border.alpha = 0.5, title = "Age Std. Prev.", 
              legend.hist = TRUE, palette = "-RdBu", breaks = c(0, 5, 10, 15, 20, 25, Inf), 
              lables = c("0-5", "5-10", "10-15", "15-20", "20-25", "25+")) + 
  tm_compass(position = c("right", "top")) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(main.title = "Age Standardised Hypertension Prevalence Rate", legend.outside = TRUE)

tm_shape(hyper_prev_shp) + 
  tm_polygons(col = 'obs_over_exp', border.alpha = 0.5, title = "Observed/Expected Ratio", 
              legend.hist = TRUE, palette = "-RdBu", breaks = c(0, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, Inf), 
              labels = c("<0.5", "0.5-0.7", "0.7-0.9", "0.9-1.1", "1.1-1.3", "1.3-1.5", ">1.5")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Hypertension Rates vs Expected', legend.outside = TRUE, legend.text.size = 2) 

# removing Ealing
hyper_prev_no_ealing <- hyper_prev_shp %>%
  filter(!grepl('Ealing', name))

# Plot at CCG Level 
ccg_shp <- merge(Eng_CCG, ccg_region, by = c('CCG21CD', 'CCG21NM'))
lsoa_ccg_shp <- merge(ENG_LSOA11, ccg_grouped, by.x = 'geo_code', by.y = 'lsoa_code')
shp_df <- merge(lsoa_ccg_shp, ccg_region, by = c('CCG21CD', 'CCG21NM', 'CCG21CDH'))

# separate file that aggregates the data
ccg_agg <- ccg_grouped %>%
  group_by(CCG21CD, CCG21NM, CCG21CDH) %>%
  summarise(avg_prevalence_21_22 = mean(obs_hyper_prev), 
            exp_hyp_21_22 = mean(exp_hyper_prev), 
            obs_over_exp_22 = mean(obs_over_exp), 
            age_std_prev_21_22 = mean(age_std_prev),
            lsoa_pop_21_22 = sum(lsoa_pop),
            avg_u79_achievement_21_22 = mean(u79_achievement), 
            avg_u79_intervention_21_22 = mean(u79_intervention), 
            avg_o80_achievement_21_22 = mean(o80_achievement), 
            avg_o80_intervention_21_22 = mean(o80_intervention)) %>%
  # including variable to indicate if above or below national average
  mutate(above_average = case_when(age_std_prev_21_22 > 15.11 ~ 1, 
                                   T ~ 0))

ccg_agg_shp <- merge(Eng_CCG, ccg_agg, by = c('CCG21CD', 'CCG21NM'))
ccg_agg_region <- merge(ccg_agg_shp, ccg_region) 

# Creating Deciles for CCGs 
ccg_agg_region$percentile <- ntile(ccg_agg_region$age_std_prev_21_22, 100)

tm_shape(ccg_agg_shp) + 
  tm_polygons(col = "age_std_prev_21_22", palette ="-RdBu", title = "Age Std. Prevalence %", 
              legend.hist = TRUE) +
  tm_compass(position = c("left", "top")) + 
  tm_scale_bar() +
  tm_layout("Age Std. Hypertension Prevalence by CCG", legend.outside = TRUE)

top10_ccg <- ccg_agg_region %>%
  filter(percentile >= 91) %>%
  select(-c(OBJECTID:SHAPE_Area))

bottom5_ccg <- ccg_agg_region %>%
  arrange(age_std_prev_21_22) %>%
  slice(1:5)

# Plotting the Top Decile of CCGs by Prevalence
ggplot(top10_ccg, aes(x = reorder(CCG21NM, -age_std_prev_21_22), y = age_std_prev_21_22)) + 
  geom_point(size = 3) + 
  coord_flip() +
  theme(legend.position = "none") + 
  labs(x = "CCG", y = "Age Standardised Prevalence Rate (%)", 
       title = "Hypertension Prevalence Distribution Amongst Top 10% of CCGs")

tm_shape(Eng_CCG) + 
  tm_borders(col = "black") +
  tm_shape(top10_ccg) + 
  tm_fill(col = "#B2182B") +
  tm_compass(position = c("left", "top")) + 
  tm_scale_bar() + 
  tm_layout(main.title = "Top 10th Percentile of Prevalence CCGs")


# Plotting how many CCGs in each Region are Above National Average
ggplot(ccg_agg_region, aes(x = fct_infreq(NHSER21NM), y = above_average, fill = NHSER21NM)) + 
  geom_col() +
  labs(x = "Region", y = "Number of CCGs", 
       title = "Number of CCGs Above the National Average by Region") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") 


# Subsetting CCGs by Region
# Obtaining the Region Boundaries
midlands_ccg <- subset(ccg_shp, NHSER21NM == 'Midlands')
north_east_ccg <- subset(ccg_shp, NHSER21NM == 'North East and Yorkshire')
north_west_ccg <- subset(ccg_shp, NHSER21NM == 'North West')
london_ccg <- subset(ccg_shp, NHSER21NM == 'London')
south_east_ccg <- subset(ccg_shp, NHSER21NM == 'South East')
south_west_ccg <- subset(ccg_shp, NHSER21NM == 'South West')
east_eng_ccg <- subset(ccg_shp, NHSER21NM == 'East of England')

ccg_hyper <- merge(ccg_shp, ccg_grouped, by = c('CCG21CD', 'CCG21NM', 'CCG21CDH'))

# Obtaining LSOA boundaries within Regions
midlands_ccg_hyper <- subset(shp_df, NHSER21NM == 'Midlands') %>%
  subset(., select = -c(geo_labelw, label, name, LAD21CD, LAD21NM)) %>%
  rename(lsoa_code = geo_code)
north_east_ccg_hyper <- subset(shp_df, NHSER21NM == 'North East and Yorkshire')%>%
  subset(., select = -c(geo_labelw, label, name, LAD21CD, LAD21NM)) %>%
  rename(lsoa_code = geo_code)
north_west_ccg_hyper <- subset(shp_df, NHSER21NM == 'North West')%>%
  subset(., select = -c(geo_labelw, label, name, LAD21CD, LAD21NM)) %>%
  rename(lsoa_code = geo_code)

london_subset <- subset(shp_df, NHSER21NM == 'London')%>%
  subset(., select = -c(geo_labelw, label, name, LAD21CD, LAD21NM)) %>%
  rename(lsoa_code = geo_code)

london_ccg_hyper <- merge(shp, ccg_grouped, by.x = 'LSOA11CD', by.y = 'lsoa_code') %>%
  subset(., select = -c(LAD11CD, LAD11NM, FID, LSOA11NM.y, LAD21CD, LAD21NM)) 
south_east_ccg_hyper <- subset(shp_df, NHSER21NM == 'South East') %>%
  subset(., select = -c(geo_labelw, label, name, LAD21CD, LAD21NM)) %>%
  rename(lsoa_code = geo_code)
south_west_ccg_hyper <- subset(shp_df, NHSER21NM == 'South West') %>%
  subset(., select = -c(geo_labelw, label, name, LAD21CD, LAD21NM)) %>%
  rename(lsoa_code = geo_code)
east_eng_ccg_hyper <- subset(shp_df, NHSER21NM == 'East of England') %>%
  subset(., select = -c(geo_labelw, label, name, LAD21CD, LAD21NM)) %>%
  rename(lsoa_code = geo_code)

# Looking at Age standardised Prevalence
ggplot(ccg_hyper) + 
  aes(x = reorder(CCG21NM, -age_std_prev), y = age_std_prev, color = CCG21NM) + 
  geom_boxplot() + coord_flip() +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', 
             size = 1) +
  theme(legend.position = "none")

ggplot(ccg_hyper) + 
  aes(x = reorder(CCG21NM, -u79_achievement), y = u79_achievement, color = CCG21NM) + 
  geom_boxplot() + coord_flip() +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', 
             size = 1) +
  theme(legend.position = "none")

tm_shape(ccg_agg_shp) + 
  tm_polygons(col = 'age_std_prev_21_22', border.alpha = 0.8, title = "Age Std. Prevalence %", legend.hist = TRUE, 
              palette = "-RdBu") + 
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(main.title = "Hypertension Prevalence by CCG", legend.outside = TRUE)


#### Breaking it Down by Region ####
# Midlands - Prevalence 
ggplot(midlands_ccg_hyper) + 
  aes(x = reorder(CCG21NM, -age_std_prev), y = age_std_prev, fill = CCG21NM) +
  #  scale_fill_brewer(palette = "RdBu") + 
  geom_boxplot(fatten = NULL) + 
  coord_flip() +
  stat_summary(fun = "mean",geom = "point", size = 2, color = "white") +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', size = 1) +
  geom_text(aes(0, mean(age_std_prev), label = 'Midlands Mean', vjust = -0.5, hjust = -0.1)) +
  geom_hline(yintercept = 17.09, color = 'red', size = 1, linetype = 'dashed') +
  geom_text(aes(0, 17.09, label = 'National Average', color = 'red', vjust = -0.5, hjust = 1.1)) +
  theme(legend.position = "none") +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "CCG", 
       title = "Standardised Hypertension Prevalence in the Midlands")

tm_shape(midlands_ccg_hyper) +
  tm_fill(col = 'age_std_prev', border.alpha = 0.5, title = "Hypertension Prevalence %", 
          legend.hist = TRUE, 
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0,10, 12.5, 15, 17.5, 20, Inf), 
          lables = c("<10", "10-12.5", "12.5-15", "15-17.5", "17.5-20", "20+")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in the Midlands', legend.outside = TRUE) +
  tm_shape(midlands_ccg) +
  tm_borders()

tm_shape(midlands_ccg_hyper) +
  tm_fill(col = 'obs_over_exp', title = "Obs:Exp Ratio", 
          legend.hist = TRUE, palette = "-RdBu",
          breaks = c(0, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, Inf), 
          labels = c("<0.5", "0.5-0.7", "0.7-0.9", "0.9-1.1", "1.1-1.3", "1.3-1.5", ">1.5")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Expected vs Observed Hypertension Rates in the Midlands', legend.outside = TRUE) +
  tm_shape(midlands_ccg) +
  tm_borders('black', lwd = 1)


# North East and Yorkshire
ggplot(north_east_ccg_hyper) + 
  aes(x = reorder(CCG21NM, -age_std_prev), y = age_std_prev, fill = CCG21NM) + 
  geom_boxplot(fatten = NULL) + 
  coord_flip() +
  stat_summary(fun = "mean",geom = "point", size = 2, color = "white") +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', size = 1) +
  geom_text(aes(0, mean(age_std_prev), label = 'NE England Mean', vjust = -0, hjust = -0.1)) +
  geom_hline(yintercept = 17.09, color = 'red', size = 1, linetype = 'dashed') +
  geom_text(aes(0, 17.09, label = 'National Average', color = 'red', vjust = -0.5, hjust = 1.1)) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "CCG", 
       title = "Standardised Hypertension Prevalence in NE England")

tm_shape(north_east_ccg_hyper) +
  tm_fill(col = 'age_std_prev', title = "Hypertension Prevalence %", 
          legend.hist = TRUE,  
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0,10, 12.5, 15, 17.5, 20, Inf), 
          lables = c("<10", "10-12.5", "12.5-15", "15-17.5", "17.5-20", "20+")) + 
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in NE England', legend.outside = TRUE) +
  tm_shape(north_east_ccg) +
  tm_borders('black', lwd = 1)

tm_shape(north_east_ccg_hyper) +
  tm_fill(col = 'obs_over_exp', title = "Obs:Exp Ratio", 
          legend.hist = TRUE, palette = "-RdBu",
          breaks = c(0, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, Inf), 
          labels = c("<0.5", "0.5-0.7", "0.7-0.9", "0.9-1.1", "1.1-1.3", "1.3-1.5", ">1.5")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Expected vs Observed Hypertension Rates in NE England', legend.outside = TRUE) +
  tm_shape(north_east_ccg) +
  tm_borders('black', lwd = 1)

# NW England
ggplot(north_west_ccg_hyper) + 
  aes(x = reorder(CCG21NM, -age_std_prev), y = age_std_prev, fill = CCG21NM) + 
  geom_boxplot(fatten = NULL) + 
  coord_flip() +
  stat_summary(fun = "mean",geom = "point", size = 2, color = "white") +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', size = 1) +
  geom_text(aes(0, mean(age_std_prev), label = 'NW England Mean', vjust = -0.1, hjust = -0.1)) +
  geom_hline(yintercept = 17.09, color = 'red', size = 1, linetype = 'dashed') +
  geom_text(aes(0, 17.09, label = 'National Average', color = 'red', vjust = -0.5, hjust = 1.1)) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "CCG", 
       title = "Standardised Hypertension Prevalence in NW England")

tm_shape(north_west_ccg_hyper) +
  tm_fill(col = 'age_std_prev', title = "Hypertension Prevalence %", 
          legend.hist = TRUE,  
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0,10, 12.5, 15, 17.5, 20, Inf), 
          lables = c("<10", "10-12.5", "12.5-15", "15-17.5", "17.5-20", "20+")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in NW England', legend.outside = TRUE) +
  tm_shape(north_west_ccg) +
  tm_borders('black', lwd = 1)

nw_ratio <- tm_shape(north_west_ccg_hyper) +
  tm_fill(col = 'obs_over_exp', title = "Obs:Exp Ratio", 
          legend.hist = TRUE, palette = "-RdBu",
          breaks = c(0, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, Inf), 
          labels = c("<0.5", "0.5-0.7", "0.7-0.9", "0.9-1.1", "1.1-1.3", "1.3-1.5", ">1.5")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Expected vs Observed Hypertension Rates in NW England', legend.outside = TRUE) +
  tm_shape(north_west_ccg) +
  tm_borders('black', lwd = 1)

# London 
ggplot(london_ccg_hyper) + 
  aes(x = reorder(CCG21NM, -age_std_prev), y = age_std_prev, fill = CCG21NM) + 
  geom_boxplot(fatten = NULL) + 
  coord_flip() +
  stat_summary(fun = "mean",geom = "point", size = 2, color = "white") +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', size = 1) +
  geom_text(aes(0, mean(age_std_prev), label = 'London Mean', vjust = -0.5, hjust = 1.1)) +
  geom_hline(yintercept = 17.09, color = 'red', size = 1, linetype = 'dashed') +
  geom_text(aes(0, 17.09, label = 'National Average', color = 'red', vjust = -0.5, hjust = -0.1)) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "CCG", 
       title = "Standardised Hypertension Prevalence in Lonon")

tm_shape(london_ccg_hyper) +
  tm_fill(col = 'age_std_prev', title = "Hypertension Prevalence %", 
          legend.hist = TRUE, 
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0,10, 12.5, 15, 17.5, 20, Inf), 
          lables = c("<10", "10-12.5", "12.5-15", "15-17.5", "17.5-20", "20+")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in London', legend.outside = TRUE) +
  tm_shape(london_ccg) +
  tm_borders('black', lwd = 1)

tm_shape(london_ccg_hyper) +
  tm_fill(col = 'obs_over_exp', title = "Obs:Exp Ratio", 
          legend.hist = TRUE, palette = "-RdBu",
          breaks = c(0, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, Inf), 
          labels = c("<0.5", "0.5-0.7", "0.7-0.9", "0.9-1.1", "1.1-1.3", "1.3-1.5", ">1.5")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Expected vs Observed Hypertension Rates in London', legend.outside = TRUE) +
  tm_shape(london_ccg) +
  tm_borders('black', lwd = 1)

# SE England
ggplot(south_east_ccg_hyper) + 
  aes(x = reorder(CCG21NM, -age_std_prev), y = age_std_prev, fill = CCG21NM) + 
  geom_boxplot(fatten = NULL) + 
  coord_flip() +
  stat_summary(fun = "mean",geom = "point", size = 2, color = "white") +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', size = 1) +
  geom_text(aes(0, mean(age_std_prev), label = 'SE England Mean', vjust = -0.5, hjust = 1.1)) +
  geom_hline(yintercept = 17.09, color = 'red', size = 1, linetype = 'dashed') +
  geom_text(aes(0, 17.09, label = 'National Average', color = 'red', vjust = -0.5, hjust = -0.1)) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "CCG", 
       title = "Standardised Hypertension Prevalence in SE England")

tm_shape(south_east_ccg_hyper) +
  tm_fill(col = 'age_std_prev', title = "Hypertension Prevalence %", 
          legend.hist = TRUE, 
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0,10, 12.5, 15, 17.5, 20, Inf), 
          lables = c("<10", "10-12.5", "12.5-15", "15-17.5", "17.5-20", "20+")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in SE England', legend.outside = TRUE) +
  tm_shape(south_east_ccg) +
  tm_borders('black', lwd = 1)

tm_shape(south_east_ccg_hyper) +
  tm_fill(col = 'obs_over_exp', title = "Obs:Exp Ratio", 
          legend.hist = TRUE, palette = "-RdBu",
          breaks = c(0, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, Inf), 
          labels = c("<0.5", "0.5-0.7", "0.7-0.9", "0.9-1.1", "1.1-1.3", "1.3-1.5", ">1.5")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Expected vs Observed Hypertension Rates in SE England', legend.outside = TRUE) +
  tm_shape(south_east_ccg) +
  tm_borders('black', lwd = 1)

# SW England
ggplot(south_west_ccg_hyper) + 
  aes(x = reorder(CCG21NM, -age_std_prev), y = age_std_prev, fill = CCG21NM) + 
  geom_boxplot(fatten = NULL) + 
  coord_flip() +
  stat_summary(fun = "mean",geom = "point", size = 2, color = "white") +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', size = 1) +
  geom_text(aes(0, mean(age_std_prev), label = 'SW England Mean', vjust = -0.5, hjust = 1.1)) +
  geom_hline(yintercept = 17.09, color = 'red', size = 1, linetype = 'dashed') +
  geom_text(aes(0, 17.09, label = 'National Average', color = 'red', vjust = -0.5, hjust = -0.1)) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "CCG", 
       title = "Standardised Hypertension Prevalence in SW England")

tm_shape(south_west_ccg_hyper) +
  tm_fill(col = 'age_std_prev', title = "Hypertension Prevalence %", 
          legend.hist = TRUE, 
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0,10, 12.5, 15, 17.5, 20, Inf), 
          lables = c("<10", "10-12.5", "12.5-15", "15-17.5", "17.5-20", "20+")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in SW England', legend.outside = TRUE) +
  tm_shape(south_west_ccg) +
  tm_borders('black', lwd = 1)

tm_shape(south_west_ccg_hyper) +
  tm_fill(col = 'obs_over_exp', title = "Obs:Exp Ratio", 
          legend.hist = TRUE, palette = "-RdBu",
          breaks = c(0, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, Inf), 
          labels = c("<0.5", "0.5-0.7", "0.7-0.9", "0.9-1.1", "1.1-1.3", "1.3-1.5", ">1.5")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Expected vs Observed Hypertension Rates in SW England', legend.outside = TRUE) +
  tm_shape(south_west_ccg) +
  tm_borders('black', lwd = 1)

# East England  
ggplot(east_eng_ccg_hyper) + 
  aes(x = reorder(CCG21NM, -age_std_prev), y = age_std_prev, fill = CCG21NM) + 
  geom_boxplot(fatten = NULL) + 
  coord_flip() +
  stat_summary(fun = "mean",geom = "point", size = 2, color = "white") +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', size = 1) +
  geom_text(aes(0, mean(age_std_prev), label = 'East England Mean', vjust = -0.5, hjust = 1.1)) +
  geom_hline(yintercept = 17.09, color = 'red', size = 1, linetype = 'dashed') +
  geom_text(aes(0, 17.09, label = 'National Average', color = 'red', vjust = -0.5, hjust = -0.1)) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "CCG", 
       title = "Standardised Hypertension Prevalence in East England")

tm_shape(east_eng_ccg_hyper) +
  tm_fill(col = 'age_std_prev', border.alpha = 0.5, title = "Hypertension Prevalence %", 
          legend.hist = TRUE,
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0,10, 12.5, 15, 17.5, 20, Inf), 
          lables = c("<10", "10-12.5", "12.5-15", "15-17.5", "17.5-20", "20+")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in East England', legend.outside = TRUE) +
  tm_shape(east_eng_ccg) +
  tm_borders('black', lwd = 1)

tm_shape(east_eng_ccg_hyper) +
  tm_fill(col = 'obs_over_exp', title = "Obs:Exp Ratio", 
          legend.hist = TRUE, palette = "-RdBu",
          breaks = c(0, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, Inf), 
          labels = c("<0.5", "0.5-0.7", "0.7-0.9", "0.9-1.1", "1.1-1.3", "1.3-1.5", ">1.5")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Expected vs Observed Hypertension Rates in East England', legend.outside = TRUE) +
  tm_shape(east_eng_ccg) +
  tm_borders('black', lwd = 1) 

# Investigating Regionally
ggplot(ccg_hyper) + 
  aes(x = reorder(NHSER21NM, -age_std_prev), y = age_std_prev, fill = NHSER21NM) + 
  geom_boxplot(fatten = NULL) +
  stat_summary(fun = "mean", geom = "point", size = 2, color = "white") + 
  coord_flip() +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', size = 1) + 
  geom_text(aes(0, mean(age_std_prev), label = "National Average", vjust = -0.5, hjust = 1.05)) +
  theme(legend.position = "none") +
  labs(x = "Region", y = "Age Standardised Prevalence Rate (%)", 
       title = "Standardised Hypertension Prevalence by Region")


#### Deprivation Analysis ####
lsoa_hyper_imd <- merge(lsoa_age_adj, LSOA_imd_cl, by.x = 'lsoa_code', by.y = 'lsoa_code_2011')

# Finding National Average IMD Score
mean(lsoa_hyper_imd$imd_score) # 21.67

# Aggregating IMD at CCG 
ccg_imd <- merge(ccg_hyper, LSOA_imd_cl, by.x = 'lsoa_code', by.y = 'lsoa_code_2011') %>%
  group_by(CCG21CD, CCG21NM) %>%
  summarise(imd_score = mean(imd_score), 
            income_score = mean(income_score_rate), 
            employment_score = mean(employment_score_rate), 
            education_training_score = mean(education_skills_and_training_score), 
            health_disability_score = mean(health_deprivation_and_disability_score),
            crime_score = mean(crime_score), 
            living_env_score = mean(living_environment_score), 
            housing_score = mean(barriers_to_housing_and_services_score)) 

# Getting IMD Decile by CCG
ccg_imd$imd_decile <- ntile(ccg_imd$imd_score, 10)
# Investigate relationship between IMD and Hypertension Prevalence
ggplot(lsoa_hyper_imd, aes(x = imd_score, y = obs_hyper_prev)) + 
  geom_point(aes(color = imd_decile)) +
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Age Standardised Prevalence Rate")

# Comparing to Expected Hypertension 
ggplot(lsoa_hyper_imd, aes(x = imd_score, y = exp_hyper_prev)) + 
  geom_point(aes(color = imd_decile)) +
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Age Standardised Prevalence Rate", fill = "IMD Decile", 
       title = "Relationship Between Hypertension and Deprivation")

# Seeing how the relationship changes when age adjusted
ggplot(lsoa_hyper_imd, aes(x = imd_score, y = age_std_prev, color = imd_decile)) + 
  geom_point() +
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Age Standardised Prevalence Rate", color = "IMD Decile",
       title = "Relationship Between Hypertension and Deprivation") 

# Comparing expected to observed ratios to IMD
ggplot(lsoa_hyper_imd, aes(x = imd_score, y = obs_over_exp)) + 
  geom_point(aes(color = imd_decile)) + 
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Reported Prevalence to Expected", color = "IMD Decile", 
       title = "Relationship between Deprivation and Hypertension Performance vs Expected")
# More deprived areas tend to have worse than expected rates of hypertension 

# Breaking down the relationship by Region 
## Midlands ##
midlands_imd <- merge(midlands_ccg_hyper, LSOA_imd_cl, by.x = "lsoa_code", by.y = "lsoa_code_2011")
# Age Adjusted Relationship 
ggplot(midlands_imd, aes(x = imd_score, y = age_std_prev)) + 
  geom_point(aes(color = imd_decile)) +
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Age Standardised Prevalence Rate")
# Ratio
ggplot(midlands_imd, aes(x = imd_score, y = obs_over_exp)) + 
  geom_point(aes(color = imd_decile)) + 
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Reported Prevalence to Expected")

## North-East England ##
north_east_imd <- merge(north_east_ccg_hyper, LSOA_imd_cl, by.x = "lsoa_code", by.y = "lsoa_code_2011")
ggplot(north_east_imd, aes(x = imd_score, y = age_std_prev)) + 
  geom_point(aes(color = imd_decile)) +
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Age Standardised Prevalence Rate")

ggplot(north_east_imd, aes(x = imd_score, y = obs_over_exp)) + 
  geom_point(aes(color = imd_decile)) + 
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Reported Prevalence to Expected")

## South-East England ## 
south_east_imd <-  merge(south_east_ccg_hyper, LSOA_imd_cl, by.x = "lsoa_code", by.y = "lsoa_code_2011")
ggplot(south_east_imd, aes(x = imd_score, y = age_std_prev)) + 
  geom_point(aes(color = imd_decile)) +
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Age Standardised Prevalence Rate")

ggplot(south_east_imd, aes(x = imd_score, y = obs_over_exp)) + 
  geom_point(aes(color = imd_decile)) + 
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Reported Prevalence to Expected")

## South-West England ## 
south_west_imd <- merge(south_west_ccg_hyper, LSOA_imd_cl, by.x = "lsoa_code", by.y = "lsoa_code_2011")
ggplot(south_west_imd, aes(x = imd_score, y = age_std_prev)) + 
  geom_point(aes(color = imd_decile)) +
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Age Standardised Prevalence Rate")

ggplot(south_west_imd, aes(x = imd_score, y = obs_over_exp)) + 
  geom_point(aes(color = imd_decile)) + 
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Reported Prevalence to Expected")

## East England ##
east_imd <- merge(east_eng_ccg_hyper, LSOA_imd_cl, by.x = "lsoa_code", by.y = "lsoa_code_2011")
ggplot(east_imd, aes(x = imd_score, y = age_std_prev)) + 
  geom_point(aes(color = imd_decile)) +
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Age Standardised Prevalence Rate")

ggplot(east_imd, aes(x = imd_score, y = obs_over_exp)) + 
  geom_point(aes(color = imd_decile)) + 
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Reported Prevalence to Expected")

## London ## 
london_imd <- merge(london_subset, LSOA_imd_cl, by.x = "lsoa_code", by.y = "lsoa_code_2011")
ggplot(london_imd, aes(x = imd_score, y = age_std_prev)) + 
  geom_point(aes(color = imd_decile)) +
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Age Standardised Prevalence Rate")

ggplot(london_imd, aes(x = imd_score, y = obs_over_exp)) + 
  geom_point(aes(color = imd_decile)) + 
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Reported Prevalence to Expected")

# Create a second set Shapefile for Deprivation Prevalence 
lsoa_ccg_region <- merge(lsoa_ccg_shp, ccg_region, by = c('CCG21CD', 'CCG21NM', 'CCG21CDH'))

ne_imd <- subset(lsoa_ccg_region, NHSER21NM == 'North East and Yorkshire')
nw_imd <- subset(lsoa_ccg_region, NHSER21NM == 'North West')

imd_shp <- merge(ENG_LSOA11, LSOA_imd, by.x = 'geo_code', by.y = 'lsoa_code_2011') 

ne_imd_shp <- merge(ne_imd, LSOA_imd, by.x = 'geo_code', by.y = 'lsoa_code_2011')
nw_imd_shp <- merge(nw_imd, LSOA_imd, by.x = 'geo_code', by.y = 'lsoa_code_2011')

# analysing the relation between hypertension prevalence and IMD 
tm_shape(ne_imd_shp) + 
  tm_polygons(col = 'imd_decile', legend.title = "Index of Multiple Deprivation Decile by LSOA") 


northwest_imd <- tm_shape(nw_imd_shp) +
  tm_fill(col = 'imd_decile', title = "IMD Decile",  
          legend.hist = TRUE, palette = "RdBu") +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'IMD Scores in NW England', legend.outside = TRUE) +
  tm_shape(north_west_ccg) +
  tm_borders('black', lwd = 1)

tmap_arrange(nw_age_std_hyp, nw_ratio, northwest_imd)
