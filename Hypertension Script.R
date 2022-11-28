# set working directory 
setwd("~/Hypertension")

#### Installing Packages ####
library(tidyverse)
library(tmap)
library(sf)
library(janitor)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(ggQC)
library(scales)
library(waterfalls)


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

#### For GP ####
lsoa_age_dist <- read_csv("~/Hypertension/Population Age Distributions/lsoa_all_age_estimates.csv", skip = 4) %>%
  clean_names()

lsoa_age_dist_cl <- lsoa_age_dist %>%
  mutate(under79_pop = rowSums(select(., x0:x79)),
         over80_pop = rowSums(select(., x80:x90))) %>%
  select(-starts_with('x'))

#### Continue Loading Data #####
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

# Finding the Top Decile of LSOAs by Prevalence
lsoa_age_adj$percentile <- ntile(lsoa_age_adj$age_std_prev, 100)
lsoa_age_adj$obs_decile <- ntile(lsoa_age_adj$obs_hyper_prev, 10)


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

# Try and break down CCG by MSOA/LSOA
#### Including LSOA to MSOA Lookup file for Plotting Distribution of Prevalence ####
lsoa_msoa <- read_csv("Output_Area_to_LSOA_to_MSOA_to_Local_Authority_District_(December_2017)_Lookup.csv") %>%
  select(LSOA11CD, MSOA11CD, MSOA11NM)

# Selecting Stoke on Trent
stoke_on_trent_hyp <- subset(midlands_imd, CCG21CD == 'E38000175')
# Finding the Mean IMD Score in Stoke-on-Trent
mean(stoke_on_trent_hyp$imd_score) # 33.31
# Aggregating at MSOA for plotting purposes
stoke_msoa <- merge(stoke_on_trent_hyp, lsoa_msoa, by.x = 'lsoa_code', by.y = 'LSOA11CD') %>%
  group_by(MSOA11CD, MSOA11NM) %>%
  summarise(age_std_prev = mean(age_std_prev), 
            obs_over_exp = mean(obs_over_exp), 
            imd_score = mean(imd_score), 
            income_score = mean(income_score_rate), 
            employment_score = mean(employment_score_rate), 
            education_training_score = mean(education_skills_and_training_score), 
            health_disability_score = mean(health_deprivation_and_disability_score),
            crime_score = mean(crime_score), 
            living_env_score = mean(living_environment_score), 
            housing_score = mean(barriers_to_housing_and_services_score))

ggplot(stoke_msoa) + 
  aes(x = reorder(MSOA11NM, -age_std_prev), y = age_std_prev) + 
  geom_point(size = 3) + coord_flip() +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', 
             size = 1) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "MSOA", 
       title = "Standardised Hypertension Prevalence by MSOA in Stoke-on-Trent")

tm_shape(stoke_on_trent_hyp) +
  tm_fill(col = "age_std_prev", border.alpha = 0.5, title = "Hypertension Prevalence %", 
              legend.hist = TRUE, palette = "-RdBu", breaks = c(0, 5, 10, 15, 20, 25, 30, 35, Inf), 
              lables = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35+")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in Stoke-on-Trent', legend.outside = TRUE) +
tm_shape(stoke_msoa) + 
  tm_borders("black", lwd = 1)

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

# Try and break down CCG by MSOA/LSOA
# County Durham 
county_durham_hyp <- subset(north_east_imd, CCG21CD == 'E38000234')
# Finding the Mean IMD Score 
mean(county_durham_hyp$imd_score) # 27.43
# Aggregating at MSOA for plotting purposes
county_durham_msoa <- merge(county_durham_hyp, lsoa_msoa, by.x = 'lsoa_code', by.y = 'LSOA11CD') %>%
  group_by(MSOA11CD, MSOA11NM) %>%
  summarise(age_std_prev = mean(age_std_prev), 
            obs_over_exp = mean(obs_over_exp), 
            imd_score = mean(imd_score), 
            income_score = mean(income_score_rate), 
            employment_score = mean(employment_score_rate), 
            education_training_score = mean(education_skills_and_training_score), 
            health_disability_score = mean(health_deprivation_and_disability_score),
            crime_score = mean(crime_score), 
            living_env_score = mean(living_environment_score), 
            housing_score = mean(barriers_to_housing_and_services_score))

ggplot(county_durham_msoa) + 
  aes(x = reorder(MSOA11NM, -age_std_prev), y = age_std_prev, color = MSOA11NM) + 
  geom_point() + coord_flip() +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', 
             size = 1) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "MSOA", 
       title = "Standardised Hypertension Prevalence by MSOA in County Durham")

tm_shape(county_durham_msoa) + 
  tm_polygons(col = 'age_std_prev', border.alpha = 0.5, title = "Hypertension Prevalence %", 
              legend.hist = TRUE, palette = "-RdBu", breaks = c(0, 5, 10, 15, 20, 25, 30, 35, Inf), 
              lables = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35+")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in East England', legend.outside = TRUE)

# Sunderland
sunderland_hyp <- subset(north_east_imd, CCG21CD == 'E38000176')
# Finding the Mean IMD Score
mean(sunderland_hyp$imd_score) # 30.64
# Aggregating at MSOA for plotting purposes
sunderland_msoa <- merge(sunderland_hyp, lsoa_msoa, by.x = 'lsoa_code', by.y = 'LSOA11CD') %>%
  group_by(MSOA11CD, MSOA11NM) %>%
  summarise(age_std_prev = mean(age_std_prev), 
            obs_over_exp = mean(obs_over_exp), 
            imd_score = mean(imd_score), 
            income_score = mean(income_score_rate), 
            employment_score = mean(employment_score_rate), 
            education_training_score = mean(education_skills_and_training_score), 
            health_disability_score = mean(health_deprivation_and_disability_score),
            crime_score = mean(crime_score), 
            living_env_score = mean(living_environment_score), 
            housing_score = mean(barriers_to_housing_and_services_score))

ggplot(sunderland_msoa) + 
  aes(x = reorder(MSOA11NM, -age_std_prev), y = age_std_prev, color = MSOA11NM) + 
  geom_point() + coord_flip() +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', 
             size = 1) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "MSOA", 
       title = "Standardised Hypertension Prevalence by MSOA in Sunderland")

## North-West England ##
north_west_imd <- merge(north_west_ccg_hyper, LSOA_imd_cl, by.x = "lsoa_code", by.y = "lsoa_code_2011")
ggplot(north_west_imd, aes(x = imd_score, y = age_std_prev)) + 
  geom_point(aes(color = imd_decile)) +
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Age Standardised Prevalence Rate")

ggplot(north_west_imd, aes(x = imd_score, y = obs_over_exp)) + 
  geom_point(aes(color = imd_decile)) + 
  stat_smooth(method = 'lm', col = 'red', size = 1) + 
  labs(x = "IMD Score", y = "Reported Prevalence to Expected")

# Try and break down CCG by MSOA/LSOA
# St Helens
st_helens_hyp <- subset(north_west_imd, CCG21CD == 'E38000172')
# Finding the Mean IMD Score 
mean(st_helens_hyp$imd_score) # 31.51
# Aggregating at MSOA for plotting purposes
st_helens_msoa <- merge(st_helens_hyp, lsoa_msoa, by.x = 'lsoa_code', by.y = 'LSOA11CD') %>%
  group_by(MSOA11CD, MSOA11NM) %>%
  summarise(age_std_prev = mean(age_std_prev), 
            obs_over_exp = mean(obs_over_exp), 
            imd_score = mean(imd_score), 
            income_score = mean(income_score_rate), 
            employment_score = mean(employment_score_rate), 
            education_training_score = mean(education_skills_and_training_score), 
            health_disability_score = mean(health_deprivation_and_disability_score),
            crime_score = mean(crime_score), 
            living_env_score = mean(living_environment_score), 
            housing_score = mean(barriers_to_housing_and_services_score))

ggplot(st_helens_msoa) + 
  aes(x = reorder(MSOA11NM, -age_std_prev), y = age_std_prev, color = MSOA11NM) + 
  geom_point() + coord_flip() +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', 
             size = 1) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "MSOA", 
       title = "Standardised Hypertension Prevalence by MSOA in St Helens")

# Tameside and Glossop
tameside_and_glossop_hyp <- subset(north_west_imd, CCG21CD == 'E38000182')
# Finding the Mean IMD Score 
mean(tameside_and_glossop_hyp$imd_score) # 29.12
# Aggregating at MSOA for plotting purposes
tameside_and_glossop_msoa <- merge(tameside_and_glossop_hyp, lsoa_msoa, by.x = 'lsoa_code', by.y = 'LSOA11CD') %>%
  group_by(MSOA11CD, MSOA11NM) %>%
  summarise(age_std_prev = mean(age_std_prev), 
            obs_over_exp = mean(obs_over_exp), 
            imd_score = mean(imd_score), 
            income_score = mean(income_score_rate), 
            employment_score = mean(employment_score_rate), 
            education_training_score = mean(education_skills_and_training_score), 
            health_disability_score = mean(health_deprivation_and_disability_score),
            crime_score = mean(crime_score), 
            living_env_score = mean(living_environment_score), 
            housing_score = mean(barriers_to_housing_and_services_score))

ggplot(tameside_and_glossop_msoa) + 
  aes(x = reorder(MSOA11NM, -age_std_prev), y = age_std_prev, color = MSOA11NM) + 
  geom_point() + coord_flip() +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', 
             size = 1) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "MSOA", 
       title = "Standardised Hypertension Prevalence by MSOA in Tameside and Glossop")

# Halton
halton_hyp <- subset(north_west_imd, CCG21CD == 'E38000068')
# Finding the Mean IMD Score
mean(halton_hyp$imd_score) # 32.88
# Aggregating at MSOA for plotting purposes
halton_msoa <- merge(halton_hyp, lsoa_msoa, by.x = 'lsoa_code', by.y = 'LSOA11CD') %>%
  group_by(MSOA11CD, MSOA11NM) %>%
  summarise(age_std_prev = mean(age_std_prev), 
            obs_over_exp = mean(obs_over_exp), 
            imd_score = mean(imd_score), 
            income_score = mean(income_score_rate), 
            employment_score = mean(employment_score_rate), 
            education_training_score = mean(education_skills_and_training_score), 
            health_disability_score = mean(health_deprivation_and_disability_score),
            crime_score = mean(crime_score), 
            living_env_score = mean(living_environment_score), 
            housing_score = mean(barriers_to_housing_and_services_score))

ggplot(halton_msoa) + 
  aes(x = reorder(MSOA11NM, -age_std_prev), y = age_std_prev, color = MSOA11NM) + 
  geom_point() + coord_flip() +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', 
             size = 1) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "MSOA", 
       title = "Standardised Hypertension Prevalence by MSOA in Halton")

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

#### For Godspower #### 
lsoa_hyper_abs_age <- merge(lsoa_age_adj, lsoa_age_dist_cl, by = 'lsoa_code') %>%
  mutate(u79_total = round(age_std_u79_prev/100*under79_pop, digits = 0),
         o80_total = round(obs_o80_prev/100*over80_pop, digits = 0))

ccg_data <- merge(ccg_agg, ccg_pop, by.x = 'CCG21CD', by.y = 'ccg_code')
ccg_data <- merge(ccg_data, ccg_age_dist_21, by.x = 'CCG21CDH', by.y = 'ccg_code') %>%
  mutate(abs_hypertension = (age_std_prev_21_22/100)*all_ages)

ccg_data_cl <- ccg_data %>%
  subset(select = -c(36:53))

regional_hyper <- ccg_data %>%
  group_by(nhser21_name) %>%
  summarise(hypertension_prevalence = mean(avg_prevalence_21_22), 
            age_std_prev = mean(age_std_prev_21_22), 
            cases_hypertension = sum(abs_hypertension),
            population = sum(total_all), 
            total_male = sum(total_male), 
            total_female = sum(total_female), 
            male0_15 = sum(male0_15), 
            male16_24 = sum(male16_24), 
            male25_34 = sum(male25_34), 
            male35_44 = sum(male35_44), 
            male45_54 = sum(male45_54), 
            male55_64 = sum(male55_64), 
            male65_74 = sum(male65_74), 
            male75plus = sum(male75plus), 
            male_prev16_24 = mean(exp_hyp_16_24_male*male16_24_perc*100), 
            male_prev25_34 = mean(exp_hyp_25_34_male*male25_34_perc*100), 
            male_prev35_44 = mean(exp_hyp_35_44_male*male35_44_perc*100), 
            male_prev45_54 = mean(exp_hyp_45_54_male*male45_54_perc*100), 
            male_prev55_64 = mean(exp_hyp_55_64_male*male55_64_perc*100), 
            male_prev65_74 = mean(exp_hyp_65_74_male*male65_74_perc*100), 
            male_prev75plus = mean(exp_hyp_75plus_male*male75plus_perc*100), 
            female0_15 = sum(female0_15), 
            female16_24 = sum(female16_24), 
            female25_34 = sum(female25_34), 
            female35_44 = sum(female35_44), 
            female45_54 = sum(female45_54), 
            female55_64 = sum(female55_64), 
            female65_74 = sum(female65_74), 
            female75plus = sum(female75plus), 
            female_prev16_24 = mean(exp_hyp_16_24_female*female16_24_perc*100), 
            female_prev25_34 = mean(exp_hyp_25_34_female*female25_34_perc*100), 
            female_prev35_44 = mean(exp_hyp_35_44_female*female35_44_perc*100), 
            female_prev45_54 = mean(exp_hyp_45_54_female*female45_54_perc*100), 
            female_prev55_64 = mean(exp_hyp_55_64_female*female55_64_perc*100), 
            female_prev65_74 = mean(exp_hyp_65_74_female*female65_74_perc*100), 
            female_prev75plus = mean(exp_hyp_75plus_female*female75plus_perc*100))

 #### Objective 2 ####
# Comparing QOF Prevalence to HSE Prevalence by GP 
# Loading in HSE GP Prevalence data 
hse_hyp_prev <- read_csv("hypertension_prevalence_estimate_HSE.csv") %>%
  clean_names()

# Age Standardising HSE data 
hse_hyper_prev <- merge(GP_age_dist_cl, hse_hyp_prev, by.x = 'org_code', by.y = 'code') %>%
  # Create new column for the percentage of patients a GP serves in each LSOA 
  mutate(hse_exp_hyp = (exp_hyp_male*perc_male + exp_hyp_female*perc_female)*100)

# merging to QOF data from 21/22
hse_qof_comp <- merge(hse_hyp_prev, Hyper21_22_cl, by.x = 'code', by.y = 'practice_code', all.y = TRUE)

# finding difference in crude prevalence 
undiagnosed_hyp_prev <- hse_qof_comp %>%
  mutate(undiagnosed_hyp = percent - prevalence_percent_21_22) %>%
  rename(hse_prevalence = percent, 
         practice_code = code)

# finding absolute prevalence at GP level 
abs_gp_hyper <- merge(undiagnosed_hyp_prev, GP_lsoa_data, by = 'practice_code', all.y = TRUE) %>%
  # selecting variables of interest 
  select(practice_code, practice_name, number_of_patients, hse_prevalence, prevalence_percent_21_22, undiagnosed_hyp,
         lsoa_code, lsoa_pop, sub_icb_loc_ods_code, sub_icb_loc_ons_code, sub_icb_loc_name) %>%
  # Calculating total undiagnosed by GP 
  mutate(undiagnosed_totals = (undiagnosed_hyp/100)*number_of_patients, 
         undiagnosed_v2 = number_of_patients*0.1125633,
         gp_coverage = number_of_patients/lsoa_pop)

# Aggregating at LSOA level 
abs_undiagnosed_lsoa <- abs_gp_hyper %>%
  group_by(lsoa_code) %>%
  summarise(undiagnosed_prev = sum(gp_coverage*undiagnosed_hyp, na.rm = TRUE), 
            lsoa_pop = mean(lsoa_pop)) %>%
  mutate(undiagnosed = round(undiagnosed_prev*lsoa_pop/100), 
         undiagnosed_v2 = round(0.1125633*lsoa_pop))

abs_undiagnosed_lsoa_ccg <- merge(abs_undiagnosed_lsoa, lsoa_ccg_pop_merge, by = c('lsoa_code', 'lsoa_pop')) %>%
  select(-c(FID, LAD21CD, LAD21NM))

abs_undiagnosed_lsoa_region <- merge(abs_undiagnosed_lsoa_ccg, ccg_region, by = c('CCG21NM', 'CCG21CD', 'CCG21CDH'))

# Aggregating at ICS level
sub_icb_abs <- abs_gp_hyper %>%
  group_by(sub_icb_loc_ods_code, sub_icb_loc_ons_code, sub_icb_loc_name) %>%
  summarise(undiagnosed_hypertension = round(sum(undiagnosed_totals, na.rm = TRUE),digits = 0),
            undiagnosed_v2 = sum(undiagnosed_v2),
            sub_icb_pop = sum(number_of_patients))

# Aggregating at Regional Level
regional_abs_undiagnosed <- merge(sub_icb_abs, ccg_region, by.x = 'sub_icb_loc_ods_code', by.y = 'CCG21CDH') %>%
  group_by(NHSER21NM, NHSER21CD) %>%
  summarise(tot_undiagnosed = sum(undiagnosed_hypertension), 
            undiagnosed_v2 = sum(undiagnosed_v2),
            region_population = sum(sub_icb_pop)) %>%
  mutate(rate_per_100000 = round(tot_undiagnosed/(region_population/100000), digits = 2), 
         undiagnosed_prev = round(tot_undiagnosed/region_population*100, digits = 2))

# Mean HSE England percentage is 25.58151
# Subtract from QOF average of 14.32518 to get 11.25633 Undiagnosed 

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

# Dropping Practice J84602 as the Practice is in the process of shutting and therefore patient and hypertension prevalance
# rates don't match (Reported Hypertension Prevalence of 44100%)
Hyper15_16 <- Hyper15_16[!(Hyper15_16$practice_code == "J84602"),]

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

#### Addressing 10W ####
# Manually Calculating the Prevalence Value for CCG 10W which is missing it's prevalence
Hyper15_16_cl$avg_prevalence_15_16[Hyper15_16_cl$ccg_code=="10W"] <- 10.110175

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
  subset(., over80_denominator_19_20 != 0) %>%
  # Changing the reported prevalence rate for this GP to the previous year due to misreporting of prevalence rates due 
  # to GP merger/closure
  mutate(prevalence_percent_19_20=ifelse(practice_code=="Y03051",8.67,prevalence_percent_19_20))

Hyper19_20$under79_achievement_net_exceptions_19_20 <- as.numeric(Hyper19_20$under79_achievement_net_exceptions_19_20)
Hyper19_20$under79_percent_receiving_intervention_19_20 <- as.numeric(Hyper19_20$under79_percent_receiving_intervention_19_20)
Hyper19_20$over80_achievement_net_exceptions_19_20 <- as.numeric(Hyper19_20$over80_achievement_net_exceptions_19_20)
Hyper19_20$over80_percent_receiving_intervention_19_20 <- as.numeric(Hyper19_20$over80_percent_receiving_intervention_19_20)

Hyper19_20_cl <- Hyper19_20 %>%
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

## 20-21 ##
Hyper20_21 <- read_csv("~/Hypertension/QOF Data/QOF_Hypertension_20-21.csv", skip = 12) %>%
  clean_names() %>%
  rename(ccg_code = ccg_ods_code_practice_parent,
         ccg_geography_code = ccg_ons_code_practice_parent,
         ccg_name = ccg_name_practice_parent,
         list_size_20_21 = list_size_11, 
         register_20_21 = register_12, 
         prevalence_percent_20_21 = prevalence_percent_13)

Hyper20_21_cl <- Hyper20_21 %>%
  group_by(ccg_code) %>%
  summarise(tot_list_size_20_21 = sum(list_size_20_21), 
            tot_register_20_21 = sum(register_20_21), 
            avg_prevalence_20_21 = mean(prevalence_percent_20_21)) %>%
  subset(., !(ccg_code %in% c("_", "Copyright © 2021, Health and Social Care Information Centre. The Health and Social Care Information Centre is a non-departmental body created by statute, also known as NHS Digital.")))


# 2021-22
Hyper21_22_ccg <- Hyper21_22 %>%
  group_by(sub_icb_loc_ods_code) %>%
  summarise(tot_list_size_21_22 = sum(list_size_21_22), 
            tot_register_21_22 = sum(register_21_22), 
            avg_prevalence_21_22 = mean(prevalence_percent_21_22), 
            tot_u79_numerator_21_22 = sum(under79_numerator_21_22), 
            tot_u79_denominator_21_22 = sum(under79_denominator_21_22),
            avg_u79_achievement_21_22 = mean(under79_achievement_net_exceptions_21_22), 
            avg_u79_intervention_21_22 = mean(under79_percent_receiving_intervention_21_22), 
            tot_o80_numerator_21_22 = sum(over80_numerator_21_22), 
            tot_o80_denominator_21_22 = sum(over80_denominator_21_22), 
            avg_o80_achievement_21_22 = mean(over80_achievement_net_exceptions_21_22), 
            avg_o80_intervention_21_22 = mean(over80_percent_receiving_intervention_21_22))

# Load in Data to Change the CCG Codes
ccg_2019_codes <- read_csv("~/Hypertension/CCG merge data/merges_up_to_2019.csv") %>%
  clean_names()
ccg_2020_codes <- read_csv("~/Hypertension/CCG merge data/merges_2020.csv") %>%
  clean_names()
ccg_2021_codes <- read_csv("~/Hypertension/CCG merge data/merges_2021.csv") %>%
  clean_names()

#### Merging Data ####
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

QOF_20 <- merge(QOF_19_to_20, Hyper19_20_cl, by.x = "new_code", by.y = 'ccg_code', all = TRUE) %>%
  rename(ccg_code = new_code)

QOF_20_data <- merge(QOF_20, ccg_2021_codes, by.x = 'ccg_code', by.y = 'old_code', all = TRUE)

QOF_20_to_21 <- QOF_20_data %>%
  mutate(., new_code = case_when(ccg_code != QOF_20_data$new_code ~ QOF_20_data$new_code, 
                                 TRUE ~ QOF_20_data$ccg_code)) %>%
  subset(., select = -c(ccg_code))

QOF_21 <- merge(QOF_20_to_21, Hyper20_21_cl, by.x = "new_code", by.y = 'ccg_code', all = TRUE) %>%
  rename(ccg_code = new_code)
  
QOF_22 <- merge(QOF_21, Hyper21_22_ccg, by.x = 'ccg_code', by.y = 'sub_icb_loc_ods_code', all = TRUE) 

QOF_prev <- QOF_22 %>%
  group_by(ccg_code) %>%
  summarise(obsprev_15 = mean(avg_prevalence_14_15, na.rm = TRUE), 
            obsprev_16 = mean(avg_prevalence_15_16, na.rm = TRUE), 
            obsprev_17 = mean(avg_prevalence_16_17, na.rm = TRUE), 
            obsprev_18 = mean(avg_prevalence_17_18, na.rm = TRUE), 
            obsprev_19 = mean(avg_prevalence_18_19, na.rm = TRUE), 
            obsprev_20 = mean(avg_prevalence_19_20, na.rm = TRUE), 
            obsprev_21 = mean(avg_prevalence_20_21, na.rm = TRUE), 
            obsprev_22 = mean(avg_prevalence_21_22, na.rm = TRUE)) %>%
  drop_na(ccg_code)

#### Interrupted Time Series Analysis ####
# Transforming the Data for ITS purposes 
QOF_prev_long <- QOF_prev %>%
  pivot_longer(!ccg_code,
               names_to = c("category", "year"),
               names_pattern = "([A-Za-z]+)_(\\d+)", # separates variables by characters and then numbers, similar to name_sep but more sophisticated
               values_to = "score")

# having pivoted the data long, then want to re-integrate the columns for each variable of interest
QOF_prev_cl <- QOF_prev_long %>%
  pivot_wider(names_from = "category", 
              values_from = "score") %>%
  mutate_at('year', as.numeric) %>%
  mutate(covid = case_when(year >= 21 ~ 1, T ~ 0), 
         year = year - 15) # creating dummy variable for the covid years 

QOF_prev_21_22 <- QOF_prev_cl %>%
  filter(year >= 6)

QOF_prev_21_22_std <- QOF_prev_cl %>%
  filter(year >= 6)

# performing ITS 
itsa <- lm(obsprev ~ year + covid + year*covid, data = QOF_prev_cl)
summary(itsa)

## Using Predict
model <- lm(obsprev ~ year + as.factor(ccg_code) + year*as.factor(ccg_code), data = subset(QOF_prev_cl, year < 6))
QOF_prev_21_22$predicted_prev <- predict(model, subset(QOF_prev_cl, year >=6))

model2 <- lm(age_std_prevalence ~ year + as.factor(ccg_code) + year*as.factor(ccg_code), data = subset(QOF_prev_cl, year < 6))
QOF_prev_21_22_std$predicted_prev <- predict(model2, subset(QOF_prev_cl, year >=6))

QOF_prev_21_22 <- QOF_prev_21_22 %>%
  mutate(prev_diff = obsprev-predicted_prev)

QOF_prev_21_22_std <- QOF_prev_21_22_std %>%
  mutate(prev_diff = age_std_prevalence-predicted_prev)

### Coding CCG population ###
lsoa_pop_from_GP <- GP_lsoa_dist %>%
  group_by(lsoa_code) %>%
  summarise(lsoa_pop = sum(number_of_patients))

lsoa_ccg_pop_merge <- merge(lsoa_pop_from_GP, lsoa_ccg_la, by.x = 'lsoa_code', by.y = 'LSOA11CD')

ccg_pop <- lsoa_ccg_pop_merge %>%
  group_by(CCG21CD, CCG21CDH, CCG21NM) %>%
  summarise(sub_icb_pop = sum(lsoa_pop))

QOF_prev_pop <- merge(QOF_prev_21_22, ccg_pop, by.x = 'ccg_code', by.y = 'CCG21CDH') %>%
  # Calculating undiagnosed absolute totals from extrapolated population
  mutate(undiagnosed = round(prev_diff*sub_icb_pop/100, digits = 0)) 


QOF_prev_pop_std <- merge(QOF_prev_21_22_std, ccg_pop, by.x = 'ccg_code', by.y = 'CCG21CDH') %>%
  # Calculating undiagnosed absolute totals from extrapolated population
  mutate(undiagnosed = round(prev_diff*sub_icb_pop/100, digits = 0)) 

# Finding Absolute Values in Differences
missed_21 <- QOF_prev_pop %>%
  filter(year == 6) %>%
  select(., -c(exp_prevalence, ratio, year, covid)) %>%
  rename(obs_prev_21 = obs_prevalence, 
         age_std_prev_21 = age_std_prevalence, 
         pred_prev_21 = predicted_prev, 
         prev_diff_21 = prev_diff,
         undiagnosed_21 = undiagnosed)

missed_22 <- QOF_prev_pop %>%
  filter(year == 7) %>%
  select(., -c(exp_prevalence, ratio, year, covid)) %>%
  rename(obs_prev_22 = obs_prevalence, 
         age_std_prev_22 = age_std_prevalence, 
         pred_prev_22 = predicted_prev, 
         prev_diff_22 = prev_diff,
         undiagnosed_22 = undiagnosed)

missed_all <- merge(missed_21, missed_22, by = c("ccg_code", "CCG21CD", "CCG21NM", "sub_icb_pop"))

missed_21_std <- QOF_prev_pop_std %>%
  filter(year == 6) %>%
  select(., -c(exp_prevalence, ratio, year, covid)) %>%
  rename(obs_prev_21 = obs_prevalence, 
         age_std_prev_21 = age_std_prevalence, 
         pred_prev_21 = predicted_prev, 
         prev_diff_21 = prev_diff,
         undiagnosed_21 = undiagnosed)

missed_22_std <- QOF_prev_pop_std %>%
  filter(year == 7) %>%
  select(., -c(exp_prevalence, ratio, year, covid)) %>%
  rename(obs_prev_22 = obs_prevalence, 
         age_std_prev_22 = age_std_prevalence, 
         pred_prev_22 = predicted_prev, 
         prev_diff_22 = prev_diff,
         undiagnosed_22 = undiagnosed)

missed_all_std <- merge(missed_21_std, missed_22_std, by = c("ccg_code", "CCG21CD", "CCG21NM", "sub_icb_pop"))

# plotting differences
prev_diff_shp <- merge(Eng_CCG, QOF_prev_pop, by = c('CCG21CD', 'CCG21NM')) 

# 2021 difference
tm_shape(prev_diff_shp) + 
  tm_polygons(col = 'age_std_prev_diff_21', border.alpha = 0.5, title = "Unreported Hypertension %", 
              legend.hist = TRUE, palette = "Reds") +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Unreported Hypertension in the UK (2021)', legend.outside = TRUE) 

# 2022 difference
tm_shape(prev_diff_shp) + 
  tm_polygons(col = 'age_std_prev_diff_22', border.alpha = 0.5, title = "Unreported Hypertension %", 
              legend.hist = TRUE, palette = "RdBu") +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Unreported Hypertension in the UK (2022)', legend.outside = TRUE) 

# merge to regional data 
abs_hyper_region <- full_join(missed_all, ccg_region)

abs_hyper_region_std <- full_join(missed_all_std, ccg_region)

ggplot(abs_hyper_region, aes(x = NHSER21NM, y = undiagnosed_22, fill = CCG21NM)) + 
  geom_col() +
  labs(x = "Region", y = "Missed Diagnoses", title = "Missed Diagnoses by Region") +
#  scale_fill_brewer() + 
  stat_summary(fun = sum, aes(label = format(..y.., digits = 5), group = NHSER21NM), geom = "text") +
  theme(legend.position = "none")

# Find Regional values 
regional_totals <- abs_hyper_region %>%
  group_by(NHSER21NM) %>%
  summarise(missed_diagnoses_total = round(sum(undiagnosed_21), 0), 
            region_pop = sum(sub_icb_pop))

regional_totals_std <- abs_hyper_region_std %>%
  group_by(NHSER21NM) %>%
  summarise(missed_diagnoses_total = round(sum(undiagnosed_21), 0), 
            region_pop = sum(sub_icb_pop))


#### Objective 4 ####
# Looking into Number of Uncontrolled cases of Hypertension 
# Start by Calculating the weighted achievement and intervention rates at GP level 
gp_uncontrolled_prev <- lsoa_hyper_prev %>%
  mutate(gp_hypertension_cases = under79_denominator_21_22 + over80_denominator_21_22, 
         weighted_achievement_rate = (under79_denominator_21_22/gp_hypertension_cases)*under79_achievement_net_exceptions_21_22 + 
           (over80_denominator_21_22/gp_hypertension_cases)*over80_achievement_net_exceptions_21_22, 
         weighted_intervention_rate = (under79_denominator_21_22/gp_hypertension_cases)*under79_percent_receiving_intervention_21_22 +
           (over80_denominator_21_22/gp_hypertension_cases)*over80_percent_receiving_intervention_21_22)

# Extrapolating these to LSOA level using the same methods as before 
lsoa_uncontrolled_prev <- gp_uncontrolled_prev %>%
  group_by(lsoa_code) %>%
  summarise(u79_achievement = under79_achievement_net_exceptions_21_22*gp_coverage, 
            o80_achievement = over80_achievement_net_exceptions_21_22*gp_coverage, 
            u79_intervention = under79_percent_receiving_intervention_21_22*gp_coverage, 
            o80_intervention = over80_percent_receiving_intervention_21_22*gp_coverage,
            weighted_achievement_rate = weighted_achievement_rate*gp_coverage, 
            weighted_intervention_rate = weighted_intervention_rate*gp_coverage, 
            lsoa_pop = mean(lsoa_pop))

# Finding the Impact on Undiagnosed Hypertension 
undiagnosed_excess <- sub_icb_abs %>%
  mutate(excess_stroke = round(undiagnosed_hypertension/67),
         excess_stroke_LB = round(undiagnosed_hypertension/84),
         excess_stroke_UB = round(undiagnosed_hypertension/57), 
         excess_mi = round(undiagnosed_hypertension/118),
         excess_mi_LB = round(undiagnosed_hypertension/171), 
         excses_mi_UB = round(undiagnosed_hypertension/94), 
         undiagnosed_per_100000 = round(undiagnosed_hypertension/(sub_icb_pop/100000), digits = 2))

regional_undiagnosed_excess <- regional_abs_undiagnosed %>%
  mutate(excess_stroke = floor(tot_undiagnosed/67), 
         excess_stroke_LB = floor(tot_undiagnosed/84), 
         excess_stroke_UB = floor(tot_undiagnosed/57), 
         excess_mi = floor(tot_undiagnosed/118), 
         excess_mi_LB = floor(tot_undiagnosed/171), 
         excess_mi_UB = floor(tot_undiagnosed/94))

# Finding the impact of missed diagnosis due to COVID
sub_icb_missed_excess <- missed_all %>%
  mutate(excess_stroke = floor(undiagnosed_22/67),
         excess_stroke_LB = floor(undiagnosed_22/84),
         excess_stroke_UB = floor(undiagnosed_22/57), 
         excess_mi = floor(undiagnosed_22/118),
         excess_mi_LB = floor(undiagnosed_22/171), 
         excess_mi_UB = floor(undiagnosed_22/94), 
         missed_diagnoses_per_100000 = round(undiagnosed_22/(sub_icb_pop/100000), digits = 2)) %>%
#  select(-c(obs_prev_21:age_std_prev_22)) %>%
  rename(missed_diagnoses = undiagnosed_22, 
         missed_diagnoses_prev = prev_diff_22)

regional_missed_excess <- regional_totals %>%
  mutate(missed_diagnoses_total = missed_diagnoses_total*-1, 
         stroke = floor(missed_diagnoses_total/67),
         stroke_LB = floor(missed_diagnoses_total/84),
         stroke_UB = floor(missed_diagnoses_total/57), 
         mi = floor(missed_diagnoses_total/118),
         mi_LB = floor(missed_diagnoses_total/171), 
         mi_UB = floor(missed_diagnoses_total/94), 
         missed_diagnoses_per_100000 = round(missed_diagnoses_total/(region_pop/100000), digits = 2)) 

regional_missed_excess$NHSER21NM <- factor(regional_missed_excess$NHSER21NM, 
                                           levels = rev(sort(regional_missed_excess$NHSER21NM)))

regional_undiagnosed_excess$NHSER21NM <- factor(regional_undiagnosed_excess$NHSER21NM, 
                                                levels = rev(sort(regional_undiagnosed_excess$NHSER21NM)))

regional_missed_excess_long <- regional_missed_excess %>%
  pivot_longer(cols = c("stroke", "mi"), 
               names_to = "CVD",
               values_to = "excess") %>%
  select(-c(stroke_LB:mi_UB))

LB <- regional_missed_excess %>%
  pivot_longer(cols = c("stroke_LB", "mi_LB"), 
               names_to = "CVD_LB",
               values_to = "LB") %>%
  select(-c(stroke:mi_UB))

UB <- regional_missed_excess %>%
  pivot_longer(cols = c("stroke_UB", "mi_UB"), 
               names_to = "CVD_UB", 
               values_to = "UB") %>%
  select(-c(stroke:mi_LB))

region_missed_long <- cbind(regional_missed_excess_long, LB, UB) %>%
  select(-c(7:10, 13:16))

### Undiagnosed
regional_undiagnosed_excess_long <- regional_undiagnosed_excess %>%
  pivot_longer(cols = c("excess_stroke", "excess_mi"), 
               names_to = "CVD",
               values_to = "excess", 
               names_prefix = "excess_") %>%
  select(-c(excess_stroke_LB:excess_mi_UB))

undiagnosed_LB <- regional_undiagnosed_excess %>%
  pivot_longer(cols = c("excess_stroke_LB", "excess_mi_LB"), 
               names_to = "CVD_LB",
               names_prefix = "excess_",
               values_to = "LB") %>%
  select(-c(excess_stroke:excess_mi_UB))

undiagnosed_UB <- regional_undiagnosed_excess %>%
  pivot_longer(cols = c("excess_stroke_UB", "excess_mi_UB"), 
               names_to = "CVD_UB",
               names_prefix = "excess_",
               values_to = "UB") %>%
  select(-c(excess_stroke:excess_mi_LB))

region_undiagnosed_long <- cbind(regional_undiagnosed_excess_long, undiagnosed_LB, undiagnosed_UB) %>%
  select(-c(4:6,9:15,17:23)) %>%
  rename(NHSER21NM = NHSER21NM...1, 
         NHSER21CD = NHSER21CD...2, 
         undiagnosed = tot_undiagnosed...3)

ggplot(region_undiagnosed_long, aes(x = NHSER21NM, y = excess, 
                               fill = CVD)) +
  scale_fill_manual('CVD Type', values = c("#e93f6f", "#00a3c7"), 
                    labels = c("MI", "Stroke")) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = LB, ymax = UB), width = .2, position = position_dodge(1)) +
  coord_flip() + 
  labs(title = "CVD Events by Region", x = "Region", 
       y = "Number of Events") 

#### Plotting Missed ####
ggplot(region_missed_long, aes(x = NHSER21NM, y = excess, 
                              fill = CVD)) +
  scale_fill_manual('CVD Type', values = c("#e93f6f", "#00a3c7"), 
                    labels = c("MI", "Stroke")) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = LB, ymax = UB), width = .2, position = position_dodge(1)) +
  coord_flip() + 
  labs(title = "CVD Events by Region", x = "Region", 
       y = "Number of Events") 


# Then plot pareto and waterfall charts for Number of Cases of MI, Stroke and Hypertension
lsoa_undiagnosed <- merge(lsoa_age_adj, abs_undiagnosed_lsoa, by = c('lsoa_code', 'lsoa_pop')) %>%
  mutate(across(hypertension_classification, as_factor)) %>%
  mutate(across(obs_decile, as_factor)) %>%
  mutate(hypertension_cases = round(obs_hyper_prev*lsoa_pop/100))

# Done by Prevalence Decile
#### Creating Deciles ####
decile_cumulative <- lsoa_undiagnosed %>%
  group_by(obs_decile) %>%
  summarise(undiagnosed = sum(undiagnosed), 
            undiagnosed_hse_crude = sum(undiagnosed_v2),
            population = sum(lsoa_pop),
            hypertension_pop = sum(hypertension_cases), 
            undiagnosed_NCD = (undiagnosed*0.369)/0.631) %>%
  mutate(excess_stroke = floor(undiagnosed/67),
         excess_stroke_v2 = floor(undiagnosed_hse_crude/67), 
         excess_mi = floor(undiagnosed/118), 
         excess_mi_v2 = floor(undiagnosed_hse_crude/118),
         undiagnosed_per_100000 = round(undiagnosed/(population/100000), digits = 2))

undiagnosed_decile <- lsoa_undiagnosed %>%
  dplyr::mutate(ntile = ntile(undiagnosed, 5)) %>%
  mutate(across(ntile, as_factor))

undiagnosed_decile_grouped <- undiagnosed_decile %>%
  dplyr::group_by(ntile) %>%
  summarise(undiagnosed = sum(undiagnosed), 
            population = sum(lsoa_pop)) %>%
  mutate(excess_stroke = floor(undiagnosed/67), 
         excess_mi = floor(undiagnosed/118))

decile_cumulative_long <- decile_cumulative%>%
  pivot_longer(cols = c('undiagnosed', 'hypertension_pop'), 
               names_to = 'population_type', 
               values_to = 'occurances')

ggplot(decile_cumulative, aes(x = obs_decile, y = excess_stroke)) +
  stat_pareto(point.color = "red", 
              point.size = 1, 
              line.color = "black") + 
  labs(title = "Preventable Strokes by Hypertension Decile", 
       x = "Hypertension Prevalence Decile", 
       y = "Prevented Strokes")

ggplot(decile_cumulative, aes(x = obs_decile, y = excess_mi)) +
  stat_pareto(point.color = "red", 
              point.size = 1, 
              line.color = "black") +
  labs(title = "Preventable MI by Hypertension Decile", 
       x = "Hypertension Prevalence Decile", 
       y = "Prevented Cases of MI")

ggplot(undiagnosed_decile, aes(x = ntile, y = excess_stroke)) +
  stat_pareto(point.color = "red", 
              point.size = 1, 
              line.color = "black") + 
  labs(title = "Preventable Strokes by Undiagnosed Hypertension Decile", 
       x = "Undiagnosed Hypertension Decile", 
       y = "Prevented Strokes")

ggplot(decile_cumulative, aes(x = obs_decile, fill = obs_decile)) +
  scale_fill_brewer(palette = "RdBu", direction = -1) + 
  geom_col(aes(y = hypertension_pop/1000)) +
  geom_point(aes(y = undiagnosed_hse_crude/1000), size = 3) +
  geom_path(aes(y = undiagnosed_hse_crude/1000, group = 1)) +
  labs(title = "Hypertension Population Distribution", x = "Hypertension Prevalence Decile", 
       y = "Hypertension Population (in 1000s)") +
  geom_text(aes(1, 750, label = 'Undiagnosed Population', vjust = -1.5, hjust = 0.25)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Undiagnosed Population (in 1000s)")) +
  theme(legend.position =  "none")

ggplot(decile_cumulative, aes(x = obs_decile, fill = obs_decile)) +
  scale_fill_brewer(palette = "RdBu", direction = -1) + 
  geom_col(aes(y = hypertension_pop/1000)) +
  geom_point(aes(y = undiagnosed_NCD/1000), size = 3) +
  geom_path(aes(y = undiagnosed_NCD/1000, group = 1)) +
  labs(title = "Hypertension Population Distribution", x = "Hypertension Prevalence Decile", 
       y = "Hypertension Population (in 1000s)") +
  geom_text(aes(1, 750, label = 'Undiagnosed Population', vjust = 10, hjust = -1)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Undiagnosed Population (in 1000s)")) +
  theme(legend.position =  "none")

ggplot(decile_cumulative_long, aes(x = obs_decile, y = occurances/1000,
                                   fill = factor(population_type, levels = c("undiagnosed", "hypertension_pop")))) +
  scale_fill_manual('Population Type', values = c("#00a3c7", "#e93f6f"), 
                    labels = c("Undiagnosed Hypertension", "Diagnosed Hypertension")) + 
  geom_col() +
  labs(title = "Hypertension Distribution by Decile", x = "Hypertension Prevalence Decile", 
       y = "Hypertension Population (in 1000s)") 


# Done by Region
ggplot(regional_missed_excess, aes(x = NHSER21NM, y = excess_stroke)) +
  stat_pareto(point.color = "red", 
              point.size = 2, 
              line.color = "black") + 
  labs(title = "Preventable Stroke by Region", 
       x = "NHS England Region", 
       y = "Prevented Cases of Stroke")


# Midlands Specific 
midlands_undiagnosed <- abs_undiagnosed_lsoa_region %>%
  dplyr::filter(NHSER21NM == "Midlands") %>%
  dplyr::mutate(undiagnosed_quintile = ntile(undiagnosed, 5)) 

midlands_undiagnosed_grouped <- midlands_undiagnosed %>%
  group_by(undiagnosed_quintile) %>%
  summarise(pop = sum(lsoa_pop),
            undiagnosed = sum(undiagnosed)) %>%
  dplyr::mutate(across(undiagnosed_quintile, as_factor),
                excess_stroke = floor(undiagnosed/67), 
                excess_mi = floor(undiagnosed/118))

ggplot(midlands_undiagnosed_grouped, aes(x = undiagnosed_quintile, y = excess_stroke)) +
  stat_pareto(point.color = "red", 
              point.size = 2, 
              line.color = "black") +
  labs(title = "Preventable Strokes in the Midlands", 
       x = "Undiagnosed Quintile", 
       y = "Preventable Cases of Stroke")

# East of England 
east_england_undiagnosed <- abs_undiagnosed_lsoa_region %>%
  dplyr::filter(NHSER21NM == "East of England") %>%
  dplyr::mutate(undiagnosed_quintile = ntile(undiagnosed, 5)) 

east_england_undiagnosed_grouped <- east_england_undiagnosed %>%
  group_by(undiagnosed_quintile) %>%
  summarise(pop = sum(lsoa_pop),
            undiagnosed = sum(undiagnosed)) %>%
  dplyr::mutate(across(undiagnosed_quintile, as_factor),
                excess_stroke = floor(undiagnosed/67), 
                excess_mi = floor(undiagnosed/118))

ggplot(east_england_undiagnosed_grouped, aes(x = undiagnosed_quintile, y = excess_stroke)) +
  stat_pareto(point.color = "red", 
              point.size = 2, 
              line.color = "black") +
  labs(title = "Preventable Strokes in the East of England", 
       x = "Undiagnosed Quintile", 
       y = "Preventable Cases of Stroke")

# London 
london_undiagnosed <- abs_undiagnosed_lsoa_region %>%
  dplyr::filter(NHSER21NM == "London") %>%
  dplyr::mutate(undiagnosed_quintile = ntile(undiagnosed, 5)) 

london_undiagnosed_grouped <- london_undiagnosed %>%
  group_by(undiagnosed_quintile) %>%
  summarise(pop = sum(lsoa_pop),
            undiagnosed = sum(undiagnosed)) %>%
  dplyr::mutate(across(undiagnosed_quintile, as_factor),
                excess_stroke = floor(undiagnosed/67), 
                excess_mi = floor(undiagnosed/118))

ggplot(london_undiagnosed_grouped, aes(x = undiagnosed_quintile, y = excess_stroke)) +
  stat_pareto(point.color = "red", 
              point.size = 2, 
              line.color = "black") +
  labs(title = "Preventable Strokes in London", 
       x = "Undiagnosed Quintile", 
       y = "Preventable Cases of Stroke")


# North-East and Yorkshire 
north_east_undiagnosed <- abs_undiagnosed_lsoa_region %>%
  dplyr::filter(NHSER21NM == "North East and Yorkshire") %>%
  dplyr::mutate(undiagnosed_quintile = ntile(undiagnosed, 5)) 

north_east_undiagnosed_grouped <- north_east_undiagnosed %>%
  group_by(undiagnosed_quintile) %>%
  summarise(pop = sum(lsoa_pop),
            undiagnosed = sum(undiagnosed)) %>%
  dplyr::mutate(across(undiagnosed_quintile, as_factor),
                excess_stroke = floor(undiagnosed/67), 
                excess_mi = floor(undiagnosed/118))

ggplot(north_east_undiagnosed_grouped, aes(x = undiagnosed_quintile, y = excess_stroke)) +
  stat_pareto(point.color = "red", 
              point.size = 2, 
              line.color = "black") +
  labs(title = "Preventable Strokes in the North East and Yorkshire", 
       x = "Undiagnosed Quintile", 
       y = "Preventable Cases of Stroke")

# North-West
north_west_undiagnosed <- abs_undiagnosed_lsoa_region %>%
  dplyr::filter(NHSER21NM == "North West") %>%
  dplyr::mutate(undiagnosed_quintile = ntile(undiagnosed, 5)) 

north_west_undiagnosed_grouped <- north_west_undiagnosed %>%
  group_by(undiagnosed_quintile) %>%
  summarise(pop = sum(lsoa_pop),
            undiagnosed = sum(undiagnosed)) %>%
  dplyr::mutate(across(undiagnosed_quintile, as_factor),
                excess_stroke = floor(undiagnosed/67), 
                excess_mi = floor(undiagnosed/118))

ggplot(north_west_undiagnosed_grouped, aes(x = undiagnosed_quintile, y = excess_stroke)) +
  stat_pareto(point.color = "red", 
              point.size = 2, 
              line.color = "black") +
  labs(title = "Preventable Strokes in the North West", 
       x = "Undiagnosed Quintile", 
       y = "Preventable Cases of Stroke")

# South-East
south_east_undiagnosed <- abs_undiagnosed_lsoa_region %>%
  dplyr::filter(NHSER21NM == "South East") %>%
  dplyr::mutate(undiagnosed_quintile = ntile(undiagnosed, 5)) 

south_east_undiagnosed_grouped <- south_east_undiagnosed %>%
  group_by(undiagnosed_quintile) %>%
  summarise(pop = sum(lsoa_pop),
            undiagnosed = sum(undiagnosed)) %>%
  dplyr::mutate(across(undiagnosed_quintile, as_factor),
                excess_stroke = floor(undiagnosed/67), 
                excess_mi = floor(undiagnosed/118))

ggplot(south_east_undiagnosed_grouped, aes(x = undiagnosed_quintile, y = excess_stroke)) +
  stat_pareto(point.color = "red", 
              point.size = 2, 
              line.color = "black") +
  labs(title = "Preventable Strokes in the South East", 
       x = "Undiagnosed Quintile", 
       y = "Preventable Cases of Stroke")

# South-West
south_west_undiagnosed <- abs_undiagnosed_lsoa_region %>%
  dplyr::filter(NHSER21NM == "South West") %>%
  dplyr::mutate(undiagnosed_quintile = ntile(undiagnosed, 5)) 

south_west_undiagnosed_grouped <- south_west_undiagnosed %>%
  group_by(undiagnosed_quintile) %>%
  summarise(pop = sum(lsoa_pop),
            undiagnosed = sum(undiagnosed)) %>%
  dplyr::mutate(across(undiagnosed_quintile, as_factor),
                excess_stroke = floor(undiagnosed/67), 
                excess_mi = floor(undiagnosed/118))

ggplot(south_west_undiagnosed_grouped, aes(x = undiagnosed_quintile, y = excess_stroke)) +
  stat_pareto(point.color = "red", 
              point.size = 2, 
              line.color = "black") +
  labs(title = "Preventable Strokes in the South West", 
       x = "Undiagnosed Quintile", 
       y = "Preventable Cases of Stroke")


#### Waterfall Plots - Excess Strokes ####
waterfall(values = undiagnosed_decile_grouped$excess_stroke, 
          labels = undiagnosed_decile_grouped$ntile, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, 
          fill_colours = c("#dcfffd", "#c3f0fa", "#ade1f6", "#95d1f2", "#8bbee0")) +
  theme_minimal() +
  labs(title = "Excess Stroke in the England", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Excess Strokes") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))


waterfall(values = midlands_undiagnosed_grouped$excess_stroke, 
          labels = midlands_undiagnosed_grouped$undiagnosed_quintile, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, 
          fill_colours = c("#dcfffd", "#c3f0fa", "#ade1f6", "#95d1f2", "#8bbee0")) +
  theme_minimal() +
  labs(title = "Excess Stroke in the Midlands", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Excess Strokes") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

waterfall(values = east_england_undiagnosed_grouped$excess_stroke, 
          labels = east_england_undiagnosed_grouped$undiagnosed_quintile, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, 
          fill_colours = c("#dcfffd", "#c3f0fa", "#ade1f6", "#95d1f2", "#8bbee0")) +
  theme_minimal() +
  labs(title = "Excess Stroke in the East of England", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Excess Strokes") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

waterfall(values = london_undiagnosed_grouped$excess_stroke, 
          labels = london_undiagnosed_grouped$undiagnosed_quintile, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, 
          fill_colours = c("#dcfffd", "#c3f0fa", "#ade1f6", "#95d1f2", "#8bbee0")) +
  theme_minimal() +
  labs(title = "Excess Stroke in London", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Excess Strokes") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

waterfall(values = north_east_undiagnosed_grouped$excess_stroke, 
          labels = north_east_undiagnosed_grouped$undiagnosed_quintile, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, 
          fill_colours = c("#dcfffd", "#c3f0fa", "#ade1f6", "#95d1f2", "#8bbee0")) +
  theme_minimal() +
  labs(title = "Excess Stroke in the North East and Yorkshire", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Excess Strokes") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

waterfall(values = north_west_undiagnosed_grouped$excess_stroke, 
          labels = north_west_undiagnosed_grouped$undiagnosed_quintile, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, 
          fill_colours = c("#dcfffd", "#c3f0fa", "#ade1f6", "#95d1f2", "#8bbee0")) +
  theme_minimal() +
  labs(title = "Excess Stroke in the North West", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Excess Strokes") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

waterfall(values = south_east_undiagnosed_grouped$excess_stroke, 
          labels = south_east_undiagnosed_grouped$undiagnosed_quintile, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, 
          fill_colours = c("#dcfffd", "#c3f0fa", "#ade1f6", "#95d1f2", "#8bbee0")) +
  theme_minimal() +
  labs(title = "Excess Stroke in the South East", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Excess Strokes") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

waterfall(values = south_west_undiagnosed_grouped$excess_stroke, 
          labels = south_west_undiagnosed_grouped$undiagnosed_quintile, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, 
          fill_colours = c("#dcfffd", "#c3f0fa", "#ade1f6", "#95d1f2", "#8bbee0")) +
  theme_minimal() +
  labs(title = "Excess Stroke in the South West", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Excess Strokes") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

#### Waterfall Plots - Excess MI ####
waterfall(values = undiagnosed_decile_grouped$excess_mi, 
          labels = undiagnosed_decile_grouped$ntile, calc_total = T,
          total_rect_color = "#840034",
          fill_by_sign = FALSE, 
          fill_colours = c("#fde5e2", "#fbcdca", "#f9b6b5", "#f6a0a0", "#b9787f")) +
  theme_minimal() +
  labs(title = "Excess MI in the England", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Excess MI") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))


waterfall(values = midlands_undiagnosed_grouped$excess_mi, 
          labels = midlands_undiagnosed_grouped$undiagnosed_quintile, calc_total = T,
          total_rect_color = "#840034",
          fill_by_sign = FALSE, 
          fill_colours = c("#fde5e2", "#fbcdca", "#f9b6b5", "#f6a0a0", "#b9787f")) +
  theme_minimal() +
  labs(title = "Excess MI in the Midlands", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Excess MI") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

waterfall(values = east_england_undiagnosed_grouped$excess_mi, 
          labels = east_england_undiagnosed_grouped$undiagnosed_quintile, calc_total = T,
          total_rect_color = "#840034",
          fill_by_sign = FALSE, 
          fill_colours = c("#fde5e2", "#fbcdca", "#f9b6b5", "#f6a0a0", "#b9787f")) +
  theme_minimal() +
  labs(title = "Excess MI in the East of England", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Excess MIs") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

waterfall(values = london_undiagnosed_grouped$excess_mi, 
          labels = london_undiagnosed_grouped$undiagnosed_quintile, calc_total = T,
          total_rect_color = "#840034",
          fill_by_sign = FALSE, 
          fill_colours = c("#fde5e2", "#fbcdca", "#f9b6b5", "#f6a0a0", "#b9787f")) +
  theme_minimal() +
  labs(title = "Excess MI in London", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Excess MIs") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

waterfall(values = north_east_undiagnosed_grouped$excess_mi, 
          labels = north_east_undiagnosed_grouped$undiagnosed_quintile, calc_total = T,
          total_rect_color = "#840034",
          fill_by_sign = FALSE, 
          fill_colours = c("#fde5e2", "#fbcdca", "#f9b6b5", "#f6a0a0", "#b9787f")) +
  theme_minimal() +
  labs(title = "Excess MI in the North East and Yorkshire", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Excess MIs") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

waterfall(values = north_west_undiagnosed_grouped$excess_mi, 
          labels = north_west_undiagnosed_grouped$undiagnosed_quintile, calc_total = T,
          total_rect_color = "#840034",
          fill_by_sign = FALSE, 
          fill_colours = c("#fde5e2", "#fbcdca", "#f9b6b5", "#f6a0a0", "#b9787f")) +
  theme_minimal() +
  labs(title = "Excess MI in the North West", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Excess MIs") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

waterfall(values = south_east_undiagnosed_grouped$excess_mi, 
          labels = south_east_undiagnosed_grouped$undiagnosed_quintile, calc_total = T,
          total_rect_color = "#840034",
          fill_by_sign = FALSE, 
          fill_colours = c("#fde5e2", "#fbcdca", "#f9b6b5", "#f6a0a0", "#b9787f")) +
  theme_minimal() +
  labs(title = "Excess MI in the South East", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Excess MIs") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

waterfall(values = south_west_undiagnosed_grouped$excess_mi, 
          labels = south_west_undiagnosed_grouped$undiagnosed_quintile, calc_total = T,
          total_rect_color = "#840034",
          fill_by_sign = FALSE, 
          fill_colours = c("#fde5e2", "#fbcdca", "#f9b6b5", "#f6a0a0", "#b9787f")) +
  theme_minimal() +
  labs(title = "Excess MI in the South West", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Excess MIs") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

#### Mapping ####
# mapping where the events are
top_quintile_undiagnosed <- undiagnosed_decile %>%
  filter(ntile == 5)

top_quin_undiagnosed_shp <- merge(ENG_LSOA11, top_quintile_undiagnosed, by.x = "geo_code", by.y = "lsoa_code")

tm_shape(Eng_CCG) + 
  tm_borders(col = "black") +
tm_shape(top_quin_undiagnosed_shp) + 
  tm_fill(col = "undiagnosed", palette = "Reds", breaks = c(250, 300, 350, 400, 450, 500, Inf), 
          title = "No. Undiagnosed") +
  tm_compass(position = c("left", "top")) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(main.title = "Location of Top Quintile of Undiagnosed Hypertension LSOAs", legend.outside = T)
