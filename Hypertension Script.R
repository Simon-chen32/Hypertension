# set working directory 
setwd("~/Hypertension")

#### Installing Packages ####
library(tidyverse)
library(tmap)
library(janitor)
library(sf)
library(stringr)
library(ggplot2)
library(RColorBrewer)

#### Loading Data #####
# Loading in Shapefile
Eng_CCG <- st_read("~/Hypertension/CCG Geography/CCG_APR_2021_EN_BFC.shp")
ENG_LSOA21 <- st_read("~/Hypertension/LSOA Shapefiles/LSOA_2021_EW_BFE_V5.shp")
ENG_LSOA11 <- st_read("~/Hypertension/LSOA Shapefiles/infuse_lsoa_lyr_2011.shp") %>% 
  filter(str_detect(geo_code, '^E'))
London_2011 <- st_read("~/Hypertension/LSOA Shapefiles/statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp") %>%
  subset(., select = -c(RGN11CD:AVHHOLDSZ))

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

ccg_region <- read_csv("CCG_to_STP_and_NHS_England_(Region)_(April_2021)_Lookup_in_England.csv") %>%
  subset(., select = c(CCG21CD, CCG21CDH, CCG21NM, NHSER21CD, NHSER21NM))

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
                       over80_percent_receiving_intervention_21_22)) %>%
  mutate(over80_prev = over80_denominator_21_22/over80_21_22, 
         under79_prev = under79_denominator_21_22/under79_21_22)

Hyper21_22_cl$over80_achievement_net_exceptions_21_22 <- as.numeric(Hyper21_22_cl$over80_achievement_net_exceptions_21_22)
Hyper21_22_cl$over80_percent_receiving_intervention_21_22 <- as.numeric(Hyper21_22_cl$over80_percent_receiving_intervention_21_22)

GP_lsoa_age_data <- merge(GP_age_dist_cl, GP_lsoa_data, by.x = 'org_code', by.y = 'practice_code')

lsoa_hyper_prev <- merge(GP_lsoa_age_data, Hyper21_22_cl, by.x = 'org_code', by.y = 'practice_code') %>%
  # Create new column for the percentage of patients a GP serves in each LSOA 
  mutate(gp_coverage = (number_of_patients/lsoa_pop), 
         gp_share = (number_of_patients/list_size_21_22)*register_21_22, 
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
            u79_achievement = sum(gp_coverage*under79_achievement_net_exceptions_21_22), 
            u79_intervention = sum(gp_coverage*under79_percent_receiving_intervention_21_22), 
            o80_achievement = sum(gp_coverage*over80_achievement_net_exceptions_21_22), 
            o80_intervention = sum(gp_coverage*over80_percent_receiving_intervention_21_22))

# merging with expected hypertension rates for males and females 
lsoa_age_adj <- lsoa_grouped %>%
  mutate(obs_over_exp = obs_hyper_prev/exp_hyper_prev,
         u79_obs_over_exp = obs_u79_prev/exp_u79_prev,
         age_std_prev = obs_hyper_prev*obs_over_exp, 
         age_std_u79_prev = obs_u79_prev*u79_obs_over_exp)

# Merging at CCG 
ccg_grouped <- merge(lsoa_age_adj, lsoa_ccg_la, by.x = 'lsoa_code', by.y = 'LSOA11CD') 

# comparing mean and median in new df to data reported by GPs 
mean(lsoa_grouped$obs_hyper_prev) # 14.30%
median(lsoa_grouped$obs_hyper_prev) #14.55%

# Age Standardised Dataframe 
mean(lsoa_age_adj$age_std_prev) # 15.22

#### Plotting ####
# Creating a Shapefile to plot prevalence by LSOA
hyper_prev_shp <- merge(ENG_LSOA11, lsoa_age_adj, by.x = 'geo_code', by.y = 'lsoa_code')

tm_shape(hyper_prev_shp) + 
  tm_polygons(col = 'age_std_prev', border.alpha = 0.5, title = "Age Std. Prev.", 
              legend.hist = TRUE, palette = "-RdBu", breaks = c(0, 5, 10, 15, 20, 25, 30, 35, Inf), 
              lables = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35+")) + 
  tm_compass(position = c("right", "top")) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(main.title = "Age Standardised Hypertension Prevalence Rate", legend.outside = TRUE)

tm_shape(hyper_prev_shp) + 
  tm_polygons(col = 'obs_over_exp', border.alpha = 0.5, title = "Observed/Expected Ratio", 
              legend.hist = TRUE, palette = "-RdBu", breaks = c(0, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, Inf), 
              labels = c("<0.5", "0.5-0.7", "0.7-0.9", "0.9-1.1", "1.1-1.3", "1.3-1.5", ">1.5")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Hypertension Rates vs Expected', legend.outside = TRUE, legend.text.size = 2) 

# create a plot for London
london_hyper <- merge(London_2011, lsoa_age_adj, by.x = 'LSOA11CD', by.y = 'lsoa_code')

tm_shape(london_hyper) + 
  tm_polygons(col = 'obs_over_exp', border.alpha = 0.5, title = "Observed/Expected Ratio", 
              legend.hist = TRUE, palette = "-RdBu") +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Hypertension Rates vs Expected by LSOA', legend.outside = TRUE) 

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
  mutate(above_average = case_when(age_std_prev_21_22 > 15.23 ~ 1, 
                                   T ~ 0))

ccg_agg_shp <- merge(Eng_CCG, ccg_agg, by = c('CCG21CD', 'CCG21NM'))
ccg_agg_region <- merge(ccg_agg_shp, ccg_region) 

top10_ccg <- ccg_agg_region %>%
  arrange(desc(age_std_prev_21_22)) %>%
  slice(1:11)

# Plotting the Top Decile of CCGs by Prevalence
ggplot(top10_ccg, aes(x = reorder(CCG21NM, -age_std_prev_21_22), y = age_std_prev_21_22)) + 
  geom_point(size = 3) + 
  coord_flip() +
  theme(legend.position = "none") + 
  labs(x = "CCG", y = "Age Standardised Prevalence Rate (%)", 
       title = "Hypertension Prevalence Distribution Amongst Top 10% of CCGs")

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
london_ccg_hyper <- merge(London_2011, ccg_grouped, by.x = 'LSOA11CD', by.y = 'lsoa_code') %>%
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
# Achievement HYP003
ggplot(midlands_ccg_hyper) + 
  aes(x = reorder(CCG21NM, -u79_achievement), y = u79_achievement, color = CCG21NM) + 
  geom_boxplot() + coord_flip() +
  geom_hline(aes(yintercept=mean(u79_achievement)), color = 'black', 
             size = 1) +
  theme(legend.position = "none") +
  labs(y = "Achievement Rate (%)", x = "CCG", 
       title = "GP Achievement Rates by CCG")
# Intervention HYP003
ggplot(midlands_ccg_hyper) + 
  aes(x = reorder(CCG21NM, -u79_intervention), y = u79_intervention, color = CCG21NM) + 
  geom_boxplot() + coord_flip() +
  geom_hline(aes(yintercept=mean(u79_intervention)), color = 'black', 
             size = 1) +
  theme(legend.position = "none") +
  labs(y = "Intervention Rate (%)", x = "CCG", 
       title = "GP Intervention Rates by CCG")
# Achievement HYP007
ggplot(midlands_ccg_hyper) + 
  aes(x = reorder(CCG21NM, -o80_achievement), y =o80_achievement, color = CCG21NM) + 
  geom_boxplot() + coord_flip() +
  geom_hline(aes(yintercept=mean(o80_achievement)), color = 'black', 
             size = 1) +
  theme(legend.position = "none") +
  labs(y = "Over 80 Achievement Rate (%)", x = "CCG", 
       title = "GP Achievement Rates in Over 80s by CCG")
# Intervention HYP007
ggplot(midlands_ccg_hyper) + 
  aes(x = reorder(CCG21NM, -o80_intervention), y = o80_intervention, color = CCG21NM) + 
  geom_boxplot() + coord_flip() +
  geom_hline(aes(yintercept=mean(o80_intervention)), color = 'black', 
             size = 1) +
  theme(legend.position = "none") +
  labs(y = "Over 80 Intervention Rate (%)", x = "CCG", 
       title = "GP Intervention Rate in Over 80s by CCG")


tm_shape(midlands_ccg_hyper) +
  tm_fill(col = 'age_std_prev', border.alpha = 0.5, title = "Hypertension Prevalence %", 
          legend.hist = TRUE, 
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0, 5, 10, 15, 20, 25, 30, Inf), 
          lables = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30+")) +
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
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0, 5, 10, 15, 20, 25, 30, Inf), 
          lables = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30+")) + 
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
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0, 5, 10, 15, 20, 25, 30, Inf), 
          lables = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30+")) +
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
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0, 5, 10, 15, 20, 25, 30, Inf), 
          lables = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30+")) +
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
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0, 5, 10, 15, 20, 25, 30, Inf), 
          lables = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30+")) +
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
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0, 5, 10, 15, 20, 25, 30, Inf), 
          lables = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30+")) +
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
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0, 5, 10, 15, 20, 25, 30, Inf), 
          lables = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30+")) +
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

# Selecting North Staffordshire
north_staffordshire_hyp <- subset(midlands_imd, CCG21CD == 'E38000126')
# Finding the Mean IMD Score 
mean(north_staffordshire_hyp$imd_score) # 17.78
# Aggregating at MSOA for plotting purposes
north_staffordshire_msoa <- merge(north_staffordshire_hyp, lsoa_msoa, by.x = 'lsoa_code', by.y = 'LSOA11CD') %>%
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

ggplot(north_staffordshire_msoa) + 
  aes(x = reorder(MSOA11NM, -age_std_prev), y = age_std_prev, color = MSOA11NM) + 
  geom_point() + coord_flip() +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', 
             size = 1) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "MSOA", 
       title = "Standardised Hypertension Prevalence by MSOA in North Staffordshire")

tm_shape(north_staffordshire_hyp) + 
  tm_fill(col = "age_std_prev",  border.alpha = 0.5, title = "Hypertension Prevalence %", 
              legend.hist = TRUE, palette = "-RdBu", breaks = c(0, 5, 10, 15, 20, 25, 30, 35, Inf), 
              lables = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35+")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in East England', legend.outside = TRUE) + 
tm_shape(north_staffordshire_msoa) + 
  tm_borders("black", lwd = 1)

# Merge two regions together
stoke_staffordshire <- subset(midlands_imd, CCG21CD == 'E38000126' | CCG21CD == "E38000175")
stoke_staffordshire_borders <- subset(midlands_ccg, CCG21CD == "E38000126" | CCG21CD == "E38000175")

stoke_staff_prev <- tm_shape(stoke_staffordshire) + 
  tm_fill(col = "age_std_prev",  border.alpha = 0.5, title = "Age Std. Prev. %", 
          legend.hist = TRUE, palette = "-RdBu", breaks = c(0, 5, 10, 15, 20, 25, 30, 35, Inf), 
          lables = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35+")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in Stoke-on-Trent and North-Staffordshire', legend.outside = TRUE) + 
  tm_shape(stoke_staffordshire_borders) + 
  tm_borders("black", lwd = 1)

stoke_staff_ratio <- tm_shape(stoke_staffordshire) + 
  tm_fill(col = "obs_over_exp",  border.alpha = 0.5, title = "Obs/Exp Ratio", 
          legend.hist = TRUE, palette = "-RdBu", breaks = c(0, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, Inf), 
          labels = c("<0.5", "0.5-0.7", "0.7-0.9", "0.9-1.1", "1.1-1.3", "1.3-1.5", ">1.5")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence Compared to Expected', legend.outside = TRUE) + 
  tm_shape(stoke_staffordshire_borders) + 
  tm_borders("black", lwd = 1)

stoke_staff_imd <- tm_shape(stoke_staffordshire) + 
  tm_fill(col = "imd_decile",  border.alpha = 0.5, title = "IMD Decile", 
          legend.hist = TRUE, palette = "RdBu", breaks = c(0, 2, 5, 8, 10), 
          labels = c("High Deprivation", "Medium-High Deprivation", "Medium-Low Deprivation", "Low Deprivation")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'IMD Score in Stoke-on-Trent and North Staffordshire', legend.outside = TRUE) + 
  tm_shape(stoke_staffordshire_borders) + 
  tm_borders("black", lwd = 1)

tmap_arrange(stoke_staff_prev, stoke_staff_imd)

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

# East Riding of Yorkshire
east_riding_yorkshire_hyp <- subset(north_east_imd, CCG21CD == 'E38000052')
# Finding the Mean IMD Score 
mean(east_riding_yorkshire_hyp$imd_score) # 16.27
# Aggregating at MSOA for plotting purposes
east_yorkshire_msoa <- merge(east_riding_yorkshire_hyp, lsoa_msoa, by.x = 'lsoa_code', by.y = 'LSOA11CD') %>%
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

ggplot(east_yorkshire_msoa) + 
  aes(x = reorder(MSOA11NM, -age_std_prev), y = age_std_prev, color = MSOA11NM) + 
  geom_point() + coord_flip() +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', 
             size = 1) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "MSOA", 
       title = "Standardised Hypertension Prevalence by MSOA in the East Riding of Yorkshire")

# North Lincolnshire
north_lincolnshire_hyp <- subset(north_east_imd, CCG21CD == 'E38000122')
# Finding the Mean IMD Score 
mean(north_lincolnshire_hyp$imd_score) # 21.93
# Aggregating at MSOA for plotting purposes
north_lincolnshire_msoa <- merge(north_lincolnshire_hyp, lsoa_msoa, by.x = 'lsoa_code', by.y = 'LSOA11CD') %>%
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

ggplot(north_lincolnshire_msoa) + 
  aes(x = reorder(MSOA11NM, -age_std_prev), y = age_std_prev, color = MSOA11NM) + 
  geom_point() + coord_flip() +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', 
             size = 1) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "MSOA", 
       title = "Standardised Hypertension Prevalence by MSOA in North Lincolnshire")

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
# Blackpool
blackpool_hyp <- subset(north_west_imd, CCG21CD == 'E38000015')
# Finding the Mean IMD Score 
mean(blackpool_hyp$imd_score) # 19.48
# Aggregating at MSOA for plotting purposes
blackpool_msoa <- merge(blackpool_hyp, lsoa_msoa, by.x = 'lsoa_code', by.y = 'LSOA11CD') %>%
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

ggplot(blackpool_msoa) + 
  aes(x = reorder(MSOA11NM, -age_std_prev), y = age_std_prev, color = MSOA11NM) + 
  geom_point() + coord_flip() +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', 
             size = 1) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "MSOA", 
       title = "Standardised Hypertension Prevalence by MSOA in Blackpool")

# Fylde and Wyre
fylde_and_wyre_hyp <- subset(north_west_imd, CCG21CD == 'E38000226')
# Finding the Mean IMD Score 
mean(fylde_and_wyre_hyp$imd_score) # 45.91
# Aggregating at MSOA for plotting purposes
fylde_and_wyre_msoa <- merge(fylde_and_wyre_hyp, lsoa_msoa, by.x = 'lsoa_code', by.y = 'LSOA11CD') %>%
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

ggplot(fylde_and_wyre_msoa) + 
  aes(x = reorder(MSOA11NM, -age_std_prev), y = age_std_prev, color = MSOA11NM) + 
  geom_point() + coord_flip() +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', 
             size = 1) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "MSOA", 
       title = "Standardised Hypertension Prevalence by MSOA in Fylde and Wyre")

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

## South-West England0 ## 
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
london_imd <- merge(london_ccg_hyper, LSOA_imd_cl, by.x = "LSOA11CD", by.y = "lsoa_code_2011")
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
hse_hyper_prev <- merge(GP_age_dist_cl, hse_hyp_prev, by.x = 'area_code', by.y = 'code') %>%
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
         sub_icb_loc_ods_code, sub_icb_loc_ons_code, sub_icb_loc_name) %>%
  # Calculating total undiagnosed by GP 
  mutate(undiagnosed_totals = (undiagnosed_hyp/100)*number_of_patients)

# Aggregating at ICS level
sub_icb_abs <- abs_gp_hyper %>%
  group_by(sub_icb_loc_ods_code, sub_icb_loc_ons_code, sub_icb_loc_name) %>%
  summarise(undiagnosed_hypertension = round(sum(undiagnosed_totals, na.rm = TRUE),digits = 0), 
            avg_undiagnosed_percent = mean(undiagnosed_hyp, na.rm = TRUE),
            sub_icb_pop = sum(number_of_patients))

# Aggregating at Regional Level
regional_abs_undiagnosed <- merge(sub_icb_abs, ccg_region, by.x = 'sub_icb_loc_ods_code', by.y = 'CCG21CDH') %>%
  group_by(NHSER21NM, NHSER21CD) %>%
  summarise(tot_undiagnosed = sum(undiagnosed_hypertension), 
            region_population = sum(sub_icb_pop)) %>%
  mutate(rate_per_100000 = round(tot_undiagnosed/(region_population/100000), digits = 2), 
         undiagnosed_prev = round(tot_undiagnosed/region_population*100, digits = 2))


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

age_dist_15 <- read_csv("~/Hypertension/Population Age Distributions/gp_age_dist_july_2015.csv") %>%
  clean_names()

# Separating Data into Respective Age Groups
age_dist_15_cl <- age_dist_15 %>%
  mutate(male0_15 = rowSums(select(., male_0_4:male_10_14)), 
         male16_24 = rowSums(select(., male_15_19:male_20_24)), 
         male25_34 = rowSums(select(., male_25_29:male_30_34)), 
         male35_44 = rowSums(select(., male_35_39:male_40_44)), 
         male45_54 = rowSums(select(., male_45_49:male_50_54)), 
         male55_64 = rowSums(select(., male_55_59:male_60_64)), 
         male65_74 = rowSums(select(., male_65_69:male_70_74)), 
         male75plus = rowSums(select(., male_75_79:male_95)), 
         female0_15 = rowSums(select(., female_0_4:female_10_14)), 
         female16_24 = rowSums(select(., female_15_19:female_20_24)), 
         female25_34 = rowSums(select(., female_25_29:female_30_34)), 
         female35_44 = rowSums(select(., female_35_39:female_40_44)), 
         female45_54 = rowSums(select(., female_45_49:female_50_54)), 
         female55_64 = rowSums(select(., female_55_59:female_60_64)), 
         female65_74 = rowSums(select(., female_65_69:female_70_74)), 
         female75plus = rowSums(select(., female_75_79:female_95))) %>%
  select(ccg_code, total_all, total_male, total_female, male0_15, male16_24, male25_34, male35_44, male45_54, 
         male55_64, male65_74, male75plus, female0_15, female16_24, female25_34, female35_44, female45_54, female55_64, 
         female65_74, female75plus)

# Converting raw totals into percentages
ccg_age_dist_15 <- age_dist_15_cl %>%
  mutate(male0_15_perc = male0_15/total_male, 
         male16_24_perc = male16_24/total_male, 
         male25_34_perc = male25_34/total_male, 
         male35_44_perc = male35_44/total_male, 
         male45_54_perc = male45_54/total_male, 
         male55_64_perc = male55_64/total_male, 
         male65_74_perc = male65_74/total_male, 
         male75plus_perc = male75plus/total_male,
         female0_15_perc = female0_15/total_female,
         female16_24_perc = female16_24/total_female, 
         female25_34_perc = female25_34/total_female, 
         female35_44_perc = female35_44/total_female, 
         female45_54_perc = female45_54/total_female, 
         female55_64_perc = female55_64/total_female, 
         female65_74_perc = female65_74/total_female, 
         female75plus_perc = female75plus/total_female, 
         perc_male = total_male/total_all, 
         perc_female = total_female/total_all)

# calculating expected rates of hypertension - all years age standardised to 2019
ccg_age_dist_15 <- ccg_age_dist_15 %>%
  mutate(exp_hyp_16_24_male = male16_24_perc*0.01, 
         exp_hyp_25_34_male = male25_34_perc*0.01, 
         exp_hyp_35_44_male = male35_44_perc*0.03, 
         exp_hyp_45_54_male = male45_54_perc*0.11, 
         exp_hyp_55_64_male = male55_64_perc*0.26, 
         exp_hyp_65_74_male = male65_74_perc*0.38,
         exp_hyp_75plus_male = male75plus_perc*0.53,
         exp_hyp_16_24_female = female16_24_perc*0.00, 
         exp_hyp_25_34_female = female25_34_perc*0.01, 
         exp_hyp_35_44_female = female35_44_perc*0.02, 
         exp_hyp_45_54_female = female45_54_perc*0.09, 
         exp_hyp_55_64_female = female55_64_perc*0.19, 
         exp_hyp_65_74_female = female65_74_perc*0.32,
         exp_hyp_75plus_female = female75plus_perc*0.46, 
         exp_hyp_male_14_15 = exp_hyp_16_24_male + exp_hyp_25_34_male + exp_hyp_35_44_male + exp_hyp_45_54_male + 
           exp_hyp_55_64_male + exp_hyp_65_74_male + exp_hyp_75plus_male, 
         exp_hyp_female_14_15 = exp_hyp_16_24_female + exp_hyp_25_34_female + exp_hyp_35_44_female + exp_hyp_45_54_female +
           exp_hyp_55_64_female + exp_hyp_65_74_female + exp_hyp_75plus_female)

# getting expected rate for each ccg adjusted for age and gender
ccg_age_dist_15 <- ccg_age_dist_15 %>%
  mutate(exp_hyp_14_15 = (exp_hyp_male_14_15*perc_male + exp_hyp_female_14_15*perc_female)*100)

hyper_prev_14_15 <- merge(ccg_age_dist_15, Hyper14_15_cl, by = 'ccg_code') %>%
  # Creating Ratios and Age Standardising
  mutate(obs_over_exp_15 = avg_prevalence_14_15/exp_hyp_14_15, 
         age_std_prev_14_15 = avg_prevalence_14_15*obs_over_exp_15) %>%
  # selecting variables 
  select(ccg_code, avg_prevalence_14_15, exp_hyp_male_14_15, exp_hyp_female_14_15, exp_hyp_14_15, 
         tot_list_size_14_15, tot_register_14_15, tot_numerator_14_15, tot_denominator_14_15, avg_achievement_14_15, 
         avg_intervention_14_15, obs_over_exp_15, age_std_prev_14_15)

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



age_dist_16 <- read_csv("~/Hypertension/Population Age Distributions/gp_age_dist_jan_2016.csv") %>%
  clean_names()

# Separating Data into Respective Age Groups
age_dist_16_cl <- age_dist_16 %>%
  mutate(male0_15 = rowSums(select(., male_0_1:male_15_16)), 
         male16_24 = rowSums(select(., male_16_17:male_24_25)), 
         male25_34 = rowSums(select(., male_25_26:male_34_35)), 
         male35_44 = rowSums(select(., male_35_36:male_44_45)), 
         male45_54 = rowSums(select(., male_45_46:male_54_55)), 
         male55_64 = rowSums(select(., male_55_56:male_64_65)), 
         male65_74 = rowSums(select(., male_65_66:male_74_75)), 
         male75plus = rowSums(select(., male_75_76:male_95)), 
         female0_15 = rowSums(select(., female_0_1:female_15_16)), 
         female16_24 = rowSums(select(., female_16_17:female_24_25)), 
         female25_34 = rowSums(select(., female_25_26:female_34_35)), 
         female35_44 = rowSums(select(., female_35_36:female_44_45)), 
         female45_54 = rowSums(select(., female_45_46:female_54_55)), 
         female55_64 = rowSums(select(., female_55_56:female_64_65)), 
         female65_74 = rowSums(select(., female_65_66:female_74_75)), 
         female75plus = rowSums(select(., female_75_76:female_95))) %>%
  select(practice_code, ccg_code, total_all, total_male, total_female, male0_15, male16_24, male25_34, male35_44, male45_54, 
         male55_64, male65_74, male75plus, female0_15, female16_24, female25_34, female35_44, female45_54, female55_64, 
         female65_74, female75plus)

# Aggregating data at CCG level
ccg_age_dist_16 <- age_dist_16_cl %>%
  group_by(ccg_code) %>%
  summarise(total_all = sum(total_all), 
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
            female0_15 = sum(female0_15), 
            female16_24 = sum(female16_24), 
            female25_34 = sum(female25_34), 
            female35_44 = sum(female35_44), 
            female45_54 = sum(female45_54), 
            female55_64 = sum(female55_64), 
            female65_74 = sum(female65_74), 
            female75plus = sum(female75plus))

# Converting raw totals into percentages
ccg_age_dist_16 <- ccg_age_dist_16 %>%
  mutate(male0_15_perc = male0_15/total_male, 
         male16_24_perc = male16_24/total_male, 
         male25_34_perc = male25_34/total_male, 
         male35_44_perc = male35_44/total_male, 
         male45_54_perc = male45_54/total_male, 
         male55_64_perc = male55_64/total_male, 
         male65_74_perc = male65_74/total_male, 
         male75plus_perc = male75plus/total_male,
         female0_15_perc = female0_15/total_female,
         female16_24_perc = female16_24/total_female, 
         female25_34_perc = female25_34/total_female, 
         female35_44_perc = female35_44/total_female, 
         female45_54_perc = female45_54/total_female, 
         female55_64_perc = female55_64/total_female, 
         female65_74_perc = female65_74/total_female, 
         female75plus_perc = female75plus/total_female, 
         perc_male = total_male/total_all, 
         perc_female = total_female/total_all)

# calculating expected rates of hypertension 
ccg_age_dist_16 <- ccg_age_dist_16 %>%
  mutate(exp_hyp_16_24_male = male16_24_perc*0.01, 
         exp_hyp_25_34_male = male25_34_perc*0.01, 
         exp_hyp_35_44_male = male35_44_perc*0.03, 
         exp_hyp_45_54_male = male45_54_perc*0.11, 
         exp_hyp_55_64_male = male55_64_perc*0.26, 
         exp_hyp_65_74_male = male65_74_perc*0.38,
         exp_hyp_75plus_male = male75plus_perc*0.53,
         exp_hyp_16_24_female = female16_24_perc*0.00, 
         exp_hyp_25_34_female = female25_34_perc*0.01, 
         exp_hyp_35_44_female = female35_44_perc*0.02, 
         exp_hyp_45_54_female = female45_54_perc*0.09, 
         exp_hyp_55_64_female = female55_64_perc*0.19, 
         exp_hyp_65_74_female = female65_74_perc*0.32,
         exp_hyp_75plus_female = female75plus_perc*0.46, 
         exp_hyp_male_15_16 = exp_hyp_16_24_male + exp_hyp_25_34_male + exp_hyp_35_44_male + exp_hyp_45_54_male + 
           exp_hyp_55_64_male + exp_hyp_65_74_male + exp_hyp_75plus_male, 
         exp_hyp_female_15_16 = exp_hyp_16_24_female + exp_hyp_25_34_female + exp_hyp_35_44_female + exp_hyp_45_54_female +
           exp_hyp_55_64_female + exp_hyp_65_74_female + exp_hyp_75plus_female)

# getting expected rate for each ccg adjusted for age and gender
ccg_age_dist_16 <- ccg_age_dist_16 %>%
  mutate(exp_hyp_15_16 = (exp_hyp_male_15_16*perc_male + exp_hyp_female_15_16*perc_female)*100)

hyper_prev_15_16 <- merge(ccg_age_dist_16, Hyper15_16_cl, by = 'ccg_code') %>%
  # Creating Ratios and Age Standardising
  mutate(obs_over_exp_16 = avg_prevalence_15_16/exp_hyp_15_16, 
         age_std_prev_15_16 = avg_prevalence_15_16*obs_over_exp_16) %>%
  # selecting variables 
  select(ccg_code, avg_prevalence_15_16, exp_hyp_male_15_16, exp_hyp_female_15_16, exp_hyp_15_16, 
         tot_list_size_15_16, tot_register_15_16, tot_numerator_15_16, tot_denominator_15_16, avg_achievement_15_16, 
         avg_intervention_15_16, obs_over_exp_16, age_std_prev_15_16)

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

age_dist_17 <- read_csv("~/Hypertension/Population Age Distributions/gp-reg-pat-prac-quin-age-apr-17.csv") %>%
  clean_names()

age_dist_17_cl <- age_dist_17 %>%
  filter(org_type == "CCG") %>%
  pivot_wider(names_from = c(age_group_5, sex), 
              values_from = number_of_patients) %>%
  clean_names() %>%
  rename(ccg_code = org_code)

# Separating Data into Respective Age Groups
age_dist_17_cl <- age_dist_17_cl %>%
  mutate(male0_15 = rowSums(select(., x0_4_male:x10_14_male)), 
         male16_24 = rowSums(select(., x15_19_male:x20_24_male)), 
         male25_34 = rowSums(select(., x25_29_male:x30_34_male)), 
         male35_44 = rowSums(select(., x35_39_male:x40_44_male)), 
         male45_54 = rowSums(select(., x45_49_male:x50_54_male)), 
         male55_64 = rowSums(select(., x55_59_male:x60_64_male)), 
         male65_74 = rowSums(select(., x65_69_male:x70_74_male)), 
         male75plus = rowSums(select(., x75_79_male:x95_male)), 
         female0_15 = rowSums(select(., x0_4_female:x10_14_female)), 
         female16_24 = rowSums(select(., x15_19_female:x20_24_female)), 
         female25_34 = rowSums(select(., x25_29_female:x30_34_female)), 
         female35_44 = rowSums(select(., x35_39_female:x40_44_female)), 
         female45_54 = rowSums(select(., x45_49_female:x50_54_female)), 
         female55_64 = rowSums(select(., x55_59_female:x60_64_female)), 
         female65_74 = rowSums(select(., x65_69_female:x70_74_female)), 
         female75plus = rowSums(select(., x75_79_female:x95_female))) %>%
  select(ccg_code, all_all, all_male, all_female, male0_15, male16_24, male25_34, male35_44, male45_54, 
         male55_64, male65_74, male75plus, female0_15, female16_24, female25_34, female35_44, female45_54, female55_64, 
         female65_74, female75plus)

# Aggregating data at CCG level
ccg_age_dist_17 <- age_dist_17_cl %>%
  group_by(ccg_code) %>%
  summarise(total_all = sum(all_all), 
            total_male = sum(all_male), 
            total_female = sum(all_female), 
            male0_15 = sum(male0_15), 
            male16_24 = sum(male16_24), 
            male25_34 = sum(male25_34), 
            male35_44 = sum(male35_44), 
            male45_54 = sum(male45_54), 
            male55_64 = sum(male55_64), 
            male65_74 = sum(male65_74), 
            male75plus = sum(male75plus), 
            female0_15 = sum(female0_15), 
            female16_24 = sum(female16_24), 
            female25_34 = sum(female25_34), 
            female35_44 = sum(female35_44), 
            female45_54 = sum(female45_54), 
            female55_64 = sum(female55_64), 
            female65_74 = sum(female65_74), 
            female75plus = sum(female75plus))

# Converting raw totals into percentages
ccg_age_dist_17 <- ccg_age_dist_17 %>%
  mutate(male0_15_perc = male0_15/total_male, 
         male16_24_perc = male16_24/total_male, 
         male25_34_perc = male25_34/total_male, 
         male35_44_perc = male35_44/total_male, 
         male45_54_perc = male45_54/total_male, 
         male55_64_perc = male55_64/total_male, 
         male65_74_perc = male65_74/total_male, 
         male75plus_perc = male75plus/total_male,
         female0_15_perc = female0_15/total_female,
         female16_24_perc = female16_24/total_female, 
         female25_34_perc = female25_34/total_female, 
         female35_44_perc = female35_44/total_female, 
         female45_54_perc = female45_54/total_female, 
         female55_64_perc = female55_64/total_female, 
         female65_74_perc = female65_74/total_female, 
         female75plus_perc = female75plus/total_female, 
         perc_male = total_male/total_all, 
         perc_female = total_female/total_all)

# calculating expected rates of hypertension 
ccg_age_dist_17 <- ccg_age_dist_17 %>%
  mutate(exp_hyp_16_24_male = male16_24_perc*0.01, 
         exp_hyp_25_34_male = male25_34_perc*0.01, 
         exp_hyp_35_44_male = male35_44_perc*0.03, 
         exp_hyp_45_54_male = male45_54_perc*0.11, 
         exp_hyp_55_64_male = male55_64_perc*0.26, 
         exp_hyp_65_74_male = male65_74_perc*0.38,
         exp_hyp_75plus_male = male75plus_perc*0.53,
         exp_hyp_16_24_female = female16_24_perc*0.00, 
         exp_hyp_25_34_female = female25_34_perc*0.01, 
         exp_hyp_35_44_female = female35_44_perc*0.02, 
         exp_hyp_45_54_female = female45_54_perc*0.09, 
         exp_hyp_55_64_female = female55_64_perc*0.19, 
         exp_hyp_65_74_female = female65_74_perc*0.32,
         exp_hyp_75plus_female = female75plus_perc*0.46, 
         exp_hyp_male_16_17 = exp_hyp_16_24_male + exp_hyp_25_34_male + exp_hyp_35_44_male + exp_hyp_45_54_male + 
           exp_hyp_55_64_male + exp_hyp_65_74_male + exp_hyp_75plus_male, 
         exp_hyp_female_16_17 = exp_hyp_16_24_female + exp_hyp_25_34_female + exp_hyp_35_44_female + exp_hyp_45_54_female +
           exp_hyp_55_64_female + exp_hyp_65_74_female + exp_hyp_75plus_female)

# getting expected rate for each ccg adjusted for age and gender
ccg_age_dist_17 <- ccg_age_dist_17 %>%
  mutate(exp_hyp_16_17 = (exp_hyp_male_16_17*perc_male + exp_hyp_female_16_17*perc_female)*100)

hyper_prev_16_17 <- merge(ccg_age_dist_17, Hyper16_17_cl, by = 'ccg_code') %>%
  # Creating Ratios and Age Standardising
  mutate(obs_over_exp_17 = avg_prevalence_16_17/exp_hyp_16_17, 
         age_std_prev_16_17 = avg_prevalence_16_17*obs_over_exp_17) %>%
  # selecting variables 
  select(ccg_code, avg_prevalence_16_17, exp_hyp_male_16_17, exp_hyp_female_16_17, exp_hyp_16_17, 
         tot_list_size_16_17, tot_register_16_17, tot_numerator_16_17, tot_denominator_16_17, avg_achievement_16_17, 
         avg_intervention_16_17, obs_over_exp_17, age_std_prev_16_17)


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

age_dist_18 <- read_csv("~/Hypertension/Population Age Distributions/gp-reg-pat-prac-quin-age-apr-18.csv") %>%
  clean_names() 

age_dist_18_cl <- age_dist_18 %>%
  filter(org_type == "CCG") %>%
  pivot_wider(names_from = c(age_group_5, sex), 
              values_from = number_of_patients) %>%
  clean_names() %>%
  rename(ccg_code = org_code)

# Separating Data into Respective Age Groups
age_dist_18_cl <- age_dist_18_cl %>%
  mutate(male0_15 = rowSums(select(., x0_4_male:x10_14_male)), 
         male16_24 = rowSums(select(., x15_19_male:x20_24_male)), 
         male25_34 = rowSums(select(., x25_29_male:x30_34_male)), 
         male35_44 = rowSums(select(., x35_39_male:x40_44_male)), 
         male45_54 = rowSums(select(., x45_49_male:x50_54_male)), 
         male55_64 = rowSums(select(., x55_59_male:x60_64_male)), 
         male65_74 = rowSums(select(., x65_69_male:x70_74_male)), 
         male75plus = rowSums(select(., x75_79_male:x95_male)), 
         female0_15 = rowSums(select(., x0_4_female:x10_14_female)), 
         female16_24 = rowSums(select(., x15_19_female:x20_24_female)), 
         female25_34 = rowSums(select(., x25_29_female:x30_34_female)), 
         female35_44 = rowSums(select(., x35_39_female:x40_44_female)), 
         female45_54 = rowSums(select(., x45_49_female:x50_54_female)), 
         female55_64 = rowSums(select(., x55_59_female:x60_64_female)), 
         female65_74 = rowSums(select(., x65_69_female:x70_74_female)), 
         female75plus = rowSums(select(., x75_79_female:x95_female))) %>%
  select(ccg_code, all_all, all_male, all_female, male0_15, male16_24, male25_34, male35_44, male45_54, 
         male55_64, male65_74, male75plus, female0_15, female16_24, female25_34, female35_44, female45_54, female55_64, 
         female65_74, female75plus)

# Aggregating data at CCG level
ccg_age_dist_18 <- age_dist_18_cl %>%
  group_by(ccg_code) %>%
  summarise(total_all = sum(all_all), 
            total_male = sum(all_male), 
            total_female = sum(all_female), 
            male0_15 = sum(male0_15), 
            male16_24 = sum(male16_24), 
            male25_34 = sum(male25_34), 
            male35_44 = sum(male35_44), 
            male45_54 = sum(male45_54), 
            male55_64 = sum(male55_64), 
            male65_74 = sum(male65_74), 
            male75plus = sum(male75plus), 
            female0_15 = sum(female0_15), 
            female16_24 = sum(female16_24), 
            female25_34 = sum(female25_34), 
            female35_44 = sum(female35_44), 
            female45_54 = sum(female45_54), 
            female55_64 = sum(female55_64), 
            female65_74 = sum(female65_74), 
            female75plus = sum(female75plus))

# Converting raw totals into percentages
ccg_age_dist_18 <- ccg_age_dist_18 %>%
  mutate(male0_15_perc = male0_15/total_male, 
         male16_24_perc = male16_24/total_male, 
         male25_34_perc = male25_34/total_male, 
         male35_44_perc = male35_44/total_male, 
         male45_54_perc = male45_54/total_male, 
         male55_64_perc = male55_64/total_male, 
         male65_74_perc = male65_74/total_male, 
         male75plus_perc = male75plus/total_male,
         female0_15_perc = female0_15/total_female,
         female16_24_perc = female16_24/total_female, 
         female25_34_perc = female25_34/total_female, 
         female35_44_perc = female35_44/total_female, 
         female45_54_perc = female45_54/total_female, 
         female55_64_perc = female55_64/total_female, 
         female65_74_perc = female65_74/total_female, 
         female75plus_perc = female75plus/total_female, 
         perc_male = total_male/total_all, 
         perc_female = total_female/total_all)

# calculating expected rates of hypertension 
ccg_age_dist_18 <- ccg_age_dist_18 %>%
  mutate(exp_hyp_16_24_male = male16_24_perc*0.01, 
         exp_hyp_25_34_male = male25_34_perc*0.01, 
         exp_hyp_35_44_male = male35_44_perc*0.03, 
         exp_hyp_45_54_male = male45_54_perc*0.11, 
         exp_hyp_55_64_male = male55_64_perc*0.26, 
         exp_hyp_65_74_male = male65_74_perc*0.38,
         exp_hyp_75plus_male = male75plus_perc*0.53,
         exp_hyp_16_24_female = female16_24_perc*0.00, 
         exp_hyp_25_34_female = female25_34_perc*0.01, 
         exp_hyp_35_44_female = female35_44_perc*0.02, 
         exp_hyp_45_54_female = female45_54_perc*0.09, 
         exp_hyp_55_64_female = female55_64_perc*0.19, 
         exp_hyp_65_74_female = female65_74_perc*0.32,
         exp_hyp_75plus_female = female75plus_perc*0.46, 
         exp_hyp_male_17_18 = exp_hyp_16_24_male + exp_hyp_25_34_male + exp_hyp_35_44_male + exp_hyp_45_54_male + 
           exp_hyp_55_64_male + exp_hyp_65_74_male + exp_hyp_75plus_male, 
         exp_hyp_female_17_18 = exp_hyp_16_24_female + exp_hyp_25_34_female + exp_hyp_35_44_female + exp_hyp_45_54_female +
           exp_hyp_55_64_female + exp_hyp_65_74_female + exp_hyp_75plus_female)

# getting expected rate for each ccg adjusted for age and gender
ccg_age_dist_18 <- ccg_age_dist_18 %>%
  mutate(exp_hyp_17_18 = (exp_hyp_male_17_18*perc_male + exp_hyp_female_17_18*perc_female)*100)

hyper_prev_17_18 <- merge(ccg_age_dist_18, Hyper17_18_cl, by = 'ccg_code') %>%
  # Creating Ratios and Age Standardising
  mutate(obs_over_exp_18 = avg_prevalence_17_18/exp_hyp_17_18, 
         age_std_prev_17_18 = avg_prevalence_17_18*obs_over_exp_18) %>%
  # selecting variables 
  select(ccg_code, avg_prevalence_17_18, exp_hyp_male_17_18, exp_hyp_female_17_18, exp_hyp_17_18, 
         tot_list_size_17_18, tot_register_17_18, tot_numerator_17_18, tot_denominator_17_18, avg_achievement_17_18, 
         avg_intervention_17_18, obs_over_exp_18, age_std_prev_17_18)


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

age_dist_19 <- read_csv("~/Hypertension/Population Age Distributions/gp-reg-pat-prac-quin-age-apr-19.csv") %>%
  clean_names() 

age_dist_19_cl <- age_dist_19 %>%
  filter(org_type == "CCG") %>%
  pivot_wider(names_from = c(age_group_5, sex), 
              values_from = number_of_patients) %>%
  clean_names() %>%
  rename(ccg_code = org_code)

# Separating Data into Respective Age Groups
age_dist_19_cl <- age_dist_19_cl %>%
  mutate(male0_15 = rowSums(select(., x0_4_male:x10_14_male)), 
         male16_24 = rowSums(select(., x15_19_male:x20_24_male)), 
         male25_34 = rowSums(select(., x25_29_male:x30_34_male)), 
         male35_44 = rowSums(select(., x35_39_male:x40_44_male)), 
         male45_54 = rowSums(select(., x45_49_male:x50_54_male)), 
         male55_64 = rowSums(select(., x55_59_male:x60_64_male)), 
         male65_74 = rowSums(select(., x65_69_male:x70_74_male)), 
         male75plus = rowSums(select(., x75_79_male:x95_male)), 
         female0_15 = rowSums(select(., x0_4_female:x10_14_female)), 
         female16_24 = rowSums(select(., x15_19_female:x20_24_female)), 
         female25_34 = rowSums(select(., x25_29_female:x30_34_female)), 
         female35_44 = rowSums(select(., x35_39_female:x40_44_female)), 
         female45_54 = rowSums(select(., x45_49_female:x50_54_female)), 
         female55_64 = rowSums(select(., x55_59_female:x60_64_female)), 
         female65_74 = rowSums(select(., x65_69_female:x70_74_female)), 
         female75plus = rowSums(select(., x75_79_female:x95_female))) %>%
  select(ccg_code, all_all, all_male, all_female, male0_15, male16_24, male25_34, male35_44, male45_54, 
         male55_64, male65_74, male75plus, female0_15, female16_24, female25_34, female35_44, female45_54, female55_64, 
         female65_74, female75plus)

# Aggregating data at CCG level
ccg_age_dist_19 <- age_dist_19_cl %>%
  group_by(ccg_code) %>%
  summarise(total_all = sum(all_all), 
            total_male = sum(all_male), 
            total_female = sum(all_female), 
            male0_15 = sum(male0_15), 
            male16_24 = sum(male16_24), 
            male25_34 = sum(male25_34), 
            male35_44 = sum(male35_44), 
            male45_54 = sum(male45_54), 
            male55_64 = sum(male55_64), 
            male65_74 = sum(male65_74), 
            male75plus = sum(male75plus), 
            female0_15 = sum(female0_15), 
            female16_24 = sum(female16_24), 
            female25_34 = sum(female25_34), 
            female35_44 = sum(female35_44), 
            female45_54 = sum(female45_54), 
            female55_64 = sum(female55_64), 
            female65_74 = sum(female65_74), 
            female75plus = sum(female75plus))

# Converting raw totals into percentages
ccg_age_dist_19 <- ccg_age_dist_19 %>%
  mutate(male0_15_perc = male0_15/total_male, 
         male16_24_perc = male16_24/total_male, 
         male25_34_perc = male25_34/total_male, 
         male35_44_perc = male35_44/total_male, 
         male45_54_perc = male45_54/total_male, 
         male55_64_perc = male55_64/total_male, 
         male65_74_perc = male65_74/total_male, 
         male75plus_perc = male75plus/total_male,
         female0_15_perc = female0_15/total_female,
         female16_24_perc = female16_24/total_female, 
         female25_34_perc = female25_34/total_female, 
         female35_44_perc = female35_44/total_female, 
         female45_54_perc = female45_54/total_female, 
         female55_64_perc = female55_64/total_female, 
         female65_74_perc = female65_74/total_female, 
         female75plus_perc = female75plus/total_female, 
         perc_male = total_male/total_all, 
         perc_female = total_female/total_all)

# calculating expected rates of hypertension 
ccg_age_dist_19 <- ccg_age_dist_19 %>%
  mutate(exp_hyp_16_24_male = male16_24_perc*0.01, 
         exp_hyp_25_34_male = male25_34_perc*0.01, 
         exp_hyp_35_44_male = male35_44_perc*0.03, 
         exp_hyp_45_54_male = male45_54_perc*0.11, 
         exp_hyp_55_64_male = male55_64_perc*0.26, 
         exp_hyp_65_74_male = male65_74_perc*0.38,
         exp_hyp_75plus_male = male75plus_perc*0.53,
         exp_hyp_16_24_female = female16_24_perc*0.00, 
         exp_hyp_25_34_female = female25_34_perc*0.01, 
         exp_hyp_35_44_female = female35_44_perc*0.02, 
         exp_hyp_45_54_female = female45_54_perc*0.09, 
         exp_hyp_55_64_female = female55_64_perc*0.19, 
         exp_hyp_65_74_female = female65_74_perc*0.32,
         exp_hyp_75plus_female = female75plus_perc*0.46, 
         exp_hyp_male_18_19 = exp_hyp_16_24_male + exp_hyp_25_34_male + exp_hyp_35_44_male + exp_hyp_45_54_male + 
           exp_hyp_55_64_male + exp_hyp_65_74_male + exp_hyp_75plus_male, 
         exp_hyp_female_18_19 = exp_hyp_16_24_female + exp_hyp_25_34_female + exp_hyp_35_44_female + exp_hyp_45_54_female +
           exp_hyp_55_64_female + exp_hyp_65_74_female + exp_hyp_75plus_female)

# getting expected rate for each ccg adjusted for age and gender
ccg_age_dist_19 <- ccg_age_dist_19 %>%
  mutate(exp_hyp_18_19 = (exp_hyp_male_18_19*perc_male + exp_hyp_female_18_19*perc_female)*100)

hyper_prev_18_19 <- merge(ccg_age_dist_19, Hyper18_19_cl, by = 'ccg_code') %>%
  # Creating Ratios and Age Standardising
  mutate(obs_over_exp_19 = avg_prevalence_18_19/exp_hyp_18_19, 
         age_std_prev_18_19 = avg_prevalence_18_19*obs_over_exp_19) %>%
  # selecting variables 
  select(ccg_code, avg_prevalence_18_19, exp_hyp_18_19, exp_hyp_male_18_19, exp_hyp_female_18_19,
         tot_list_size_18_19, tot_register_18_19, tot_numerator_18_19, tot_denominator_18_19, avg_achievement_18_19, 
         avg_intervention_18_19, obs_over_exp_19, age_std_prev_18_19)

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

age_dist_20 <- read_csv("~/Hypertension/Population Age Distributions/gp-reg-pat-prac-quin-age-apr-20.csv") %>%
  clean_names() 

age_dist_20_cl <- age_dist_20 %>%
  filter(org_type == "CCG") %>%
  pivot_wider(names_from = c(age_group_5, sex), 
              values_from = number_of_patients) %>%
  clean_names() %>%
  rename(ccg_code = org_code)

# Separating Data into Respective Age Groups
age_dist_20_cl <- age_dist_20_cl %>%
  mutate(male0_15 = rowSums(select(., x0_4_male:x10_14_male)), 
         male16_24 = rowSums(select(., x15_19_male:x20_24_male)), 
         male25_34 = rowSums(select(., x25_29_male:x30_34_male)), 
         male35_44 = rowSums(select(., x35_39_male:x40_44_male)), 
         male45_54 = rowSums(select(., x45_49_male:x50_54_male)), 
         male55_64 = rowSums(select(., x55_59_male:x60_64_male)), 
         male65_74 = rowSums(select(., x65_69_male:x70_74_male)), 
         male75plus = rowSums(select(., x75_79_male:x95_male)), 
         female0_15 = rowSums(select(., x0_4_female:x10_14_female)), 
         female16_24 = rowSums(select(., x15_19_female:x20_24_female)), 
         female25_34 = rowSums(select(., x25_29_female:x30_34_female)), 
         female35_44 = rowSums(select(., x35_39_female:x40_44_female)), 
         female45_54 = rowSums(select(., x45_49_female:x50_54_female)), 
         female55_64 = rowSums(select(., x55_59_female:x60_64_female)), 
         female65_74 = rowSums(select(., x65_69_female:x70_74_female)), 
         female75plus = rowSums(select(., x75_79_female:x95_female))) %>%
  select(ccg_code, all_all, all_male, all_female, male0_15, male16_24, male25_34, male35_44, male45_54, 
         male55_64, male65_74, male75plus, female0_15, female16_24, female25_34, female35_44, female45_54, female55_64, 
         female65_74, female75plus)

# Aggregating data at CCG level
ccg_age_dist_20 <- age_dist_20_cl %>%
  group_by(ccg_code) %>%
  summarise(total_all = sum(all_all), 
            total_male = sum(all_male), 
            total_female = sum(all_female), 
            male0_15 = sum(male0_15), 
            male16_24 = sum(male16_24), 
            male25_34 = sum(male25_34), 
            male35_44 = sum(male35_44), 
            male45_54 = sum(male45_54), 
            male55_64 = sum(male55_64), 
            male65_74 = sum(male65_74), 
            male75plus = sum(male75plus), 
            female0_15 = sum(female0_15), 
            female16_24 = sum(female16_24), 
            female25_34 = sum(female25_34), 
            female35_44 = sum(female35_44), 
            female45_54 = sum(female45_54), 
            female55_64 = sum(female55_64), 
            female65_74 = sum(female65_74), 
            female75plus = sum(female75plus))

# Converting raw totals into percentages
ccg_age_dist_20 <- ccg_age_dist_20 %>%
  mutate(male0_15_perc = male0_15/total_male, 
         male16_24_perc = male16_24/total_male, 
         male25_34_perc = male25_34/total_male, 
         male35_44_perc = male35_44/total_male, 
         male45_54_perc = male45_54/total_male, 
         male55_64_perc = male55_64/total_male, 
         male65_74_perc = male65_74/total_male, 
         male75plus_perc = male75plus/total_male,
         female0_15_perc = female0_15/total_female,
         female16_24_perc = female16_24/total_female, 
         female25_34_perc = female25_34/total_female, 
         female35_44_perc = female35_44/total_female, 
         female45_54_perc = female45_54/total_female, 
         female55_64_perc = female55_64/total_female, 
         female65_74_perc = female65_74/total_female, 
         female75plus_perc = female75plus/total_female, 
         perc_male = total_male/total_all, 
         perc_female = total_female/total_all)

# calculating expected rates of hypertension 
ccg_age_dist_20 <- ccg_age_dist_20 %>%
  mutate(exp_hyp_16_24_male = male16_24_perc*0.01, 
         exp_hyp_25_34_male = male25_34_perc*0.01, 
         exp_hyp_35_44_male = male35_44_perc*0.03, 
         exp_hyp_45_54_male = male45_54_perc*0.11, 
         exp_hyp_55_64_male = male55_64_perc*0.26, 
         exp_hyp_65_74_male = male65_74_perc*0.38,
         exp_hyp_75plus_male = male75plus_perc*0.53,
         exp_hyp_16_24_female = female16_24_perc*0.00, 
         exp_hyp_25_34_female = female25_34_perc*0.01, 
         exp_hyp_35_44_female = female35_44_perc*0.02, 
         exp_hyp_45_54_female = female45_54_perc*0.09, 
         exp_hyp_55_64_female = female55_64_perc*0.19, 
         exp_hyp_65_74_female = female65_74_perc*0.32,
         exp_hyp_75plus_female = female75plus_perc*0.46, 
         exp_hyp_male_19_20 = exp_hyp_16_24_male + exp_hyp_25_34_male + exp_hyp_35_44_male + exp_hyp_45_54_male + 
           exp_hyp_55_64_male + exp_hyp_65_74_male + exp_hyp_75plus_male, 
         exp_hyp_female_19_20 = exp_hyp_16_24_female + exp_hyp_25_34_female + exp_hyp_35_44_female + exp_hyp_45_54_female +
           exp_hyp_55_64_female + exp_hyp_65_74_female + exp_hyp_75plus_female)

# getting expected rate for each ccg adjusted for age and gender
ccg_age_dist_20 <- ccg_age_dist_20 %>%
  mutate(exp_hyp_19_20 = (exp_hyp_male_19_20*perc_male + exp_hyp_female_19_20*perc_female)*100)

hyper_prev_19_20 <- merge(ccg_age_dist_20, Hyper19_20_cl, by = 'ccg_code') %>%
  # Creating Ratios and Age Standardising
  mutate(obs_over_exp_20 = avg_prevalence_19_20/exp_hyp_19_20, 
         age_std_prev_19_20 = avg_prevalence_19_20*obs_over_exp_20) %>%
  # selecting variables 
  select(ccg_code, avg_prevalence_19_20, exp_hyp_19_20, exp_hyp_male_19_20, exp_hyp_female_19_20,
         tot_list_size_19_20, tot_register_19_20, tot_u79_numerator_19_20, tot_u79_denominator_19_20, 
         avg_u79_achievement_19_20, avg_u79_intervention_19_20, tot_o80_numerator_19_20, 
         tot_o80_denominator_19_20, avg_o80_achievement_19_20, avg_o80_intervention_19_20,
         obs_over_exp_20, age_std_prev_19_20)

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
            avg_prevalence_20_21 = mean(prevalence_percent_20_21))

# Age Distribution Data
age_dist_21 <- read_csv("~/Hypertension/Population Age Distributions/gp-reg-pat-prac-quin-age-may-21.csv") %>%
  clean_names() 

age_dist_21_cl <- age_dist_21 %>%
  filter(org_type == "CCG") %>%
  pivot_wider(names_from = c(age_group_5, sex), 
              values_from = number_of_patients) %>%
  clean_names() %>%
  rename(ccg_code = org_code)

# Separating Data into Respective Age Groups
age_dist_21_cl <- age_dist_21_cl %>%
  mutate(male0_15 = rowSums(select(., x0_4_male:x10_14_male)), 
         male16_24 = rowSums(select(., x15_19_male:x20_24_male)), 
         male25_34 = rowSums(select(., x25_29_male:x30_34_male)), 
         male35_44 = rowSums(select(., x35_39_male:x40_44_male)), 
         male45_54 = rowSums(select(., x45_49_male:x50_54_male)), 
         male55_64 = rowSums(select(., x55_59_male:x60_64_male)), 
         male65_74 = rowSums(select(., x65_69_male:x70_74_male)), 
         male75plus = rowSums(select(., x75_79_male:x95_male)), 
         female0_15 = rowSums(select(., x0_4_female:x10_14_female)), 
         female16_24 = rowSums(select(., x15_19_female:x20_24_female)), 
         female25_34 = rowSums(select(., x25_29_female:x30_34_female)), 
         female35_44 = rowSums(select(., x35_39_female:x40_44_female)), 
         female45_54 = rowSums(select(., x45_49_female:x50_54_female)), 
         female55_64 = rowSums(select(., x55_59_female:x60_64_female)), 
         female65_74 = rowSums(select(., x65_69_female:x70_74_female)), 
         female75plus = rowSums(select(., x75_79_female:x95_female))) %>%
  select(ccg_code, all_all, all_male, all_female, male0_15, male16_24, male25_34, male35_44, male45_54, 
         male55_64, male65_74, male75plus, female0_15, female16_24, female25_34, female35_44, female45_54, female55_64, 
         female65_74, female75plus)

# Aggregating data at CCG level
ccg_age_dist_21 <- age_dist_21_cl %>%
  group_by(ccg_code) %>%
  summarise(total_all = sum(all_all), 
            total_male = sum(all_male), 
            total_female = sum(all_female), 
            male0_15 = sum(male0_15), 
            male16_24 = sum(male16_24), 
            male25_34 = sum(male25_34), 
            male35_44 = sum(male35_44), 
            male45_54 = sum(male45_54), 
            male55_64 = sum(male55_64), 
            male65_74 = sum(male65_74), 
            male75plus = sum(male75plus), 
            female0_15 = sum(female0_15), 
            female16_24 = sum(female16_24), 
            female25_34 = sum(female25_34), 
            female35_44 = sum(female35_44), 
            female45_54 = sum(female45_54), 
            female55_64 = sum(female55_64), 
            female65_74 = sum(female65_74), 
            female75plus = sum(female75plus))

# Converting raw totals into percentages
ccg_age_dist_21 <- ccg_age_dist_21 %>%
  mutate(male0_15_perc = male0_15/total_male, 
         male16_24_perc = male16_24/total_male, 
         male25_34_perc = male25_34/total_male, 
         male35_44_perc = male35_44/total_male, 
         male45_54_perc = male45_54/total_male, 
         male55_64_perc = male55_64/total_male, 
         male65_74_perc = male65_74/total_male, 
         male75plus_perc = male75plus/total_male,
         female0_15_perc = female0_15/total_female,
         female16_24_perc = female16_24/total_female, 
         female25_34_perc = female25_34/total_female, 
         female35_44_perc = female35_44/total_female, 
         female45_54_perc = female45_54/total_female, 
         female55_64_perc = female55_64/total_female, 
         female65_74_perc = female65_74/total_female, 
         female75plus_perc = female75plus/total_female, 
         perc_male = total_male/total_all, 
         perc_female = total_female/total_all)

# calculating expected rates of hypertension 
ccg_age_dist_21 <- ccg_age_dist_21 %>%
  mutate(exp_hyp_16_24_male = male16_24_perc*0.01, 
         exp_hyp_25_34_male = male25_34_perc*0.01, 
         exp_hyp_35_44_male = male35_44_perc*0.03, 
         exp_hyp_45_54_male = male45_54_perc*0.11, 
         exp_hyp_55_64_male = male55_64_perc*0.26, 
         exp_hyp_65_74_male = male65_74_perc*0.38,
         exp_hyp_75plus_male = male75plus_perc*0.53,
         exp_hyp_16_24_female = female16_24_perc*0.00, 
         exp_hyp_25_34_female = female25_34_perc*0.01, 
         exp_hyp_35_44_female = female35_44_perc*0.02, 
         exp_hyp_45_54_female = female45_54_perc*0.09, 
         exp_hyp_55_64_female = female55_64_perc*0.19, 
         exp_hyp_65_74_female = female65_74_perc*0.32,
         exp_hyp_75plus_female = female75plus_perc*0.46, 
         exp_hyp_male_20_21 = exp_hyp_16_24_male + exp_hyp_25_34_male + exp_hyp_35_44_male + exp_hyp_45_54_male + 
           exp_hyp_55_64_male + exp_hyp_65_74_male + exp_hyp_75plus_male, 
         exp_hyp_female_20_21 = exp_hyp_16_24_female + exp_hyp_25_34_female + exp_hyp_35_44_female + exp_hyp_45_54_female +
           exp_hyp_55_64_female + exp_hyp_65_74_female + exp_hyp_75plus_female)

# getting expected rate for each ccg adjusted for age and gender
ccg_age_dist_21 <- ccg_age_dist_21 %>%
  mutate(exp_hyp_20_21 = (exp_hyp_male_20_21*perc_male + exp_hyp_female_20_21*perc_female)*100)

hyper_prev_20_21 <- merge(ccg_age_dist_21, Hyper20_21_cl, by = 'ccg_code') %>%
  # Creating Ratios and Age Standardising
  mutate(obs_over_exp_21 = avg_prevalence_20_21/exp_hyp_20_21, 
         age_std_prev_20_21 = avg_prevalence_20_21*obs_over_exp_21) %>%
  # selecting variables 
  select(ccg_code, avg_prevalence_20_21, exp_hyp_20_21, exp_hyp_male_20_21, exp_hyp_female_20_21,
         tot_list_size_20_21, tot_register_20_21, 
         obs_over_exp_21, age_std_prev_20_21)


# Load in Data to Change the CCG Codes
ccg_2019_codes <- read_csv("~/Hypertension/CCG merge data/merges_up_to_2019.csv") %>%
  clean_names()
ccg_2020_codes <- read_csv("~/Hypertension/CCG merge data/merges_2020.csv") %>%
  clean_names()
ccg_2021_codes <- read_csv("~/Hypertension/CCG merge data/merges_2021.csv") %>%
  clean_names()

#### Merging Data ####
# Merging by CCG 
QOF_16 <- merge(hyper_prev_14_15, hyper_prev_15_16, by = "ccg_code", all = TRUE)

QOF_17 <- merge(QOF_16, hyper_prev_16_17, by = 'ccg_code', all = TRUE)
# Changing CCG codes as CCGs merge
QOF_17$ccg_code[QOF_17$ccg_code == "00W"] <- "14L"
QOF_17$ccg_code[QOF_17$ccg_code == "01M"] <- "14L"
QOF_17$ccg_code[QOF_17$ccg_code == "01N"] <- "14L"

QOF_18 <- merge(QOF_17, hyper_prev_17_18, by = 'ccg_code', all = TRUE)
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
  

QOF_19 <- merge(QOF_18, hyper_prev_18_19, by = 'ccg_code', all = TRUE)
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

QOF_20 <- merge(QOF_19_to_20, hyper_prev_19_20, by.x = "new_code", by.y = 'ccg_code', all = TRUE) %>%
  rename(ccg_code = new_code)

QOF_20_data <- merge(QOF_20, ccg_2021_codes, by.x = 'ccg_code', by.y = 'old_code', all = TRUE)

QOF_20_to_21 <- QOF_20_data %>%
  mutate(., new_code = case_when(ccg_code != QOF_20_data$new_code ~ QOF_20_data$new_code, 
                                 TRUE ~ QOF_20_data$ccg_code)) %>%
  subset(., select = -c(ccg_code))

QOF_21 <- merge(QOF_20_to_21, hyper_prev_20_21, by.x = "new_code", by.y = 'ccg_code', all = TRUE) %>%
  rename(ccg_code = new_code)
  
QOF_22 <- merge(QOF_21, ccg_agg, by.x = 'ccg_code', by.y = 'CCG21CDH', all = TRUE) 

QOF_prev <- QOF_22 %>%
  group_by(ccg_code, CCG21CD, CCG21NM) %>%
  summarise(obsprev_15 = mean(avg_prevalence_14_15, na.rm = TRUE), 
            expprev_15 = mean(exp_hyp_14_15, na.rm = TRUE),
            agestdprev_15 = mean(age_std_prev_14_15, na.rm = TRUE),
            obs_exp_ratio_15 = mean(obs_over_exp_15, na.rm = TRUE),
            obsprev_16 = mean(avg_prevalence_15_16, na.rm = TRUE), 
            expprev_16 = mean(exp_hyp_15_16, na.rm = TRUE),
            agestdprev_16 = mean(age_std_prev_15_16, na.rm = TRUE),
            obs_exp_ratio_16 = mean(obs_over_exp_16, na.rm = TRUE),
            obsprev_17 = mean(avg_prevalence_16_17, na.rm = TRUE), 
            expprev_17 = mean(exp_hyp_16_17, na.rm = TRUE),
            agestdprev_17 = mean(age_std_prev_16_17, na.rm = TRUE),
            obs_exp_ratio_17 = mean(obs_over_exp_17, na.rm = TRUE),
            obsprev_18 = mean(avg_prevalence_17_18, na.rm = TRUE), 
            expprev_18 = mean(exp_hyp_17_18, na.rm = TRUE),
            agestdprev_18 = mean(age_std_prev_17_18, na.rm = TRUE),
            obs_exp_ratio_18 = mean(obs_over_exp_18, na.rm = TRUE),
            obsprev_19 = mean(avg_prevalence_18_19, na.rm = TRUE), 
            expprev_19 = mean(exp_hyp_18_19, na.rm = TRUE),
            agestdprev_19 = mean(age_std_prev_18_19, na.rm = TRUE),
            obs_exp_ratio_19 = mean(obs_over_exp_19, na.rm = TRUE),
            obsprev_20 = mean(avg_prevalence_19_20, na.rm = TRUE), 
            expprev_20 = mean(exp_hyp_19_20, na.rm = TRUE),
            agestdprev_20 = mean(age_std_prev_19_20, na.rm = TRUE),
            obs_exp_ratio_20 = mean(obs_over_exp_20, na.rm = TRUE),
            obsprev_21 = mean(avg_prevalence_20_21, na.rm = TRUE), 
            expprev_21 = mean(exp_hyp_20_21, na.rm = TRUE), 
            agestdprev_21 = mean(age_std_prev_20_21, na.rm = TRUE), 
            obs_exp_ratio_21 = mean(obs_over_exp_21, na.rm = TRUE), 
            patients_22 = mean(lsoa_pop_21_22, na.rm = TRUE),
            obsprev_22 = mean(avg_prevalence_21_22, na.rm = TRUE), 
            expprev_22 = mean(exp_hyp_21_22, na.rm = TRUE), 
            agestdprev_22 = mean(age_std_prev_21_22, na.rm = TRUE), 
            obs_exp_ratio_22 = mean(obs_over_exp_22, na.rm = TRUE))

#### Interupted Time Series Analysis ####
# Transforming the Data for ITS purposes 
QOF_prev_long <- QOF_prev %>%
  pivot_longer(!c(ccg_code, CCG21CD, CCG21NM, patients_22),
               names_to = c("category", "year"),
               names_pattern = "([A-Za-z]+)_(\\d+)", # separates variables by characters and then numbers, similar to name_sep but more sophisticated
               values_to = "score")

# having pivoted the data long, then want to re-integrate the columns for each variable of interest
QOF_prev_cl <- QOF_prev_long %>%
  pivot_wider(names_from = "category", 
              values_from = "score") %>%
  rename(age_std_prevalence = agestdprev, 
         exp_prevalence = expprev, 
         obs_prevalence = obsprev) %>%
  mutate_at('year', as.numeric) %>%
  mutate(covid = case_when(year >= 21 ~ 1, T ~ 0), 
         year = year - 15) # creating dummy variable for the covid years 

# Subsetting for the First Four Years
QOF_prev_14_19 <- QOF_prev_cl %>%
  filter(., year <=  5)

# performing ITS 
itsa <- lm(age_std_prevalence ~ year + covid + year*covid, data = QOF_prev_cl)
summary(itsa)

# Finding what the average yearly increase in prevalence is from 2014-15 to 2017-18 
fit_14_19 <- lm(age_std_prevalence ~ year, data = QOF_prev_14_19)
summary(fit_14_19)

# Fitting the yearly increase (0.11226) to the 2019 data to account for the change in prevalence 
# When shifted to include 2019 (as the threshold change doesn't impact prevalance), coefficient = 0.12606
QOF_prev_20_22 <- QOF_prev %>%
  select(CCG21CD, agestdprev_20, agestdprev_21, agestdprev_22, patients_22)

# Calculating the expected prevalence 
QOF_prev_20_22 <- QOF_prev_20_22 %>%
  mutate(exp_age_std_prev_21 = agestdprev_20 + 0.12606, 
         exp_age_std_prev_22 = agestdprev_20 + 0.12606*2, 
         # Now calculating the difference between the expected change and obs change to get the covid effect
         age_std_prev_diff_21 = case_when(agestdprev_21 - exp_age_std_prev_21 <= 0 ~ agestdprev_21 - exp_age_std_prev_21,
                                          T ~ 0),
         age_std_prev_diff_22 = (agestdprev_22-agestdprev_21) - (exp_age_std_prev_22-exp_age_std_prev_21))
  

# plotting differences
prev_diff_shp <- merge(Eng_CCG, QOF_prev_20_22, by = 'CCG21CD') 

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

# Finding Absolute Values in Differences
# calculate absolute totals
abs_hyper_shp <- prev_diff_shp %>%
  mutate(undiagnosed_21 = (age_std_prev_diff_21/100)*patients_22, 
         undiagnosed_22 = case_when(age_std_prev_diff_22 > 0 ~ ((age_std_prev_diff_22+age_std_prev_diff_21)/100)*patients_22, 
                                    T ~ ((age_std_prev_diff_22-age_std_prev_diff_21)/100)*patients_22))

# Merge all the undiagnosed data into one column 
abs_hyper_long <- abs_hyper_shp %>%
  pivot_longer(cols = starts_with("undiagnosed"), 
               names_to = "year", names_prefix = "undiagnosed_", 
               values_to = "undiagnosed_totals") 

# merge to regional data 
abs_hyper_region <- full_join(abs_hyper_long, ccg_region)

ggplot(abs_hyper_region, aes(x = NHSER21NM, y = undiagnosed_totals, fill = year)) + 
  geom_col() +
  labs(x = "Region", y = "Missed Diagnoses", title = "Missed Diagnoses by Region") +
  scale_fill_manual(values = c("#00a3c7", "#d8b2b4")) + 
  stat_summary(fun = sum, aes(label = format(..y.., digits = 5), group = NHSER21NM), geom = "text")

# Find Regional values 
regional_totals <- abs_hyper_region %>%
  group_by(NHSER21NM, year) %>%
  summarise(undiagnosed_total = round(sum(undiagnosed_totals), 0))



