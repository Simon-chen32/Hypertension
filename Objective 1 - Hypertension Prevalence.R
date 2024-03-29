# set working directory 
setwd("C:/Users/SIMC/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - 03 CVD Secondary prevention/Analysis")

#### Installing Packages ####
library(tidyverse)
library(gt)
library(gtExtras)
library(tmap)
library(sf)
library(janitor)
library(ggplot2)
library(ggpubr)
library(gridExtra)

lcp_blue_to_red <- c("#002f5f", "#345c80", "#627e9b",  "#8da8ad", "#c1d3d6", "#fcdadb", "#f595a3", "#e93f6f", "#821a4d")

#### Loading Data #####
# Loading in Shapefile
Eng_ICS <- st_read("~/Shapefiles/Sub-ICB Shapefiles/SICBL_JUL_2022_EN_BUC.shp")
# Rename the three ICS with "Isle" so there are no difficulties with merging
Eng_ICS$SICBL22NM <- gsub("NHS Cornwall and The Isles Of Scilly ICB - 11N", "NHS Cornwall and the Isles of Scilly ICB - 11N", Eng_ICS$SICBL22NM)
Eng_ICS$SICBL22NM <- gsub("NHS Hampshire and Isle Of Wight ICB - 10R", "NHS Hampshire and Isle of Wight ICB - 10R", Eng_ICS$SICBL22NM)
Eng_ICS$SICBL22NM <- gsub("NHS Hampshire and Isle Of Wight ICB - D9Y0V", "NHS Hampshire and Isle of Wight ICB - D9Y0V", Eng_ICS$SICBL22NM)

ENG_LSOA11 <- st_read("~/Shapefiles/LSOA (2011) Shapefiles/infuse_lsoa_lyr_2011.shp") %>% 
  filter(str_detect(geo_code, '^E'))

# Loading in Practice Data
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
         female80plus = rowSums(select(., x80_84_female:x95_female))) %>%
  select(-starts_with('x'))

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
         female80plus_perc = female80plus/all_female,
         # calculating the expected hypertension prevalence by age group
         exp_hyp_16_24_male = male16_24_perc*0.01, 
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
         exp_hyp_80plus_female = female80plus_perc*0.46, 
         # calculating the total expected hypertension by GP 
         exp_hyp_male = exp_hyp_16_24_male + exp_hyp_25_34_male + exp_hyp_35_44_male + exp_hyp_45_54_male + exp_hyp_55_64_male + exp_hyp_65_74_male + exp_hyp_75_79_male + exp_hyp_80plus_male, 
         exp_u79_hyp_male = exp_hyp_16_24_male + exp_hyp_25_34_male + exp_hyp_35_44_male + exp_hyp_45_54_male + exp_hyp_55_64_male + exp_hyp_65_74_male + exp_hyp_75_79_male,
         exp_hyp_female = exp_hyp_16_24_female + exp_hyp_25_34_female + exp_hyp_35_44_female + exp_hyp_45_54_female + exp_hyp_55_64_female + exp_hyp_65_74_female + exp_hyp_75_79_female + exp_hyp_80plus_female, 
         exp_u79_hyp_female = exp_hyp_16_24_female + exp_hyp_25_34_female + exp_hyp_35_44_female + exp_hyp_45_54_female + exp_hyp_55_64_female + exp_hyp_65_74_female + exp_hyp_75_79_female,
         perc_male = all_male/all_all, 
         perc_female = all_female/all_all, 
         male_u79_perc = (all_male-male80plus)/all_all, 
         male_o80_perc = male80plus/all_all, 
         female_u79_perc = (all_female-female80plus)/all_all, 
         female_o80_perc = female80plus/all_all)

LSOA_imd <- read_csv("~/Hypertension/LSOA_IMD_Scores_Deciles.csv") %>%
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

# Loading in some lookup files
lsoa_ics <- read_csv("~/Lookup Files/LSOA_(2011)_to_Sub_ICB_Locations_to_Integrated_Care_Boards_(July_2022)_Lookup_in_England.csv") %>%
  select(-ObjectId)
ics_region <- read_csv("~/Lookup Files/Sub_ICB_Locations_to_ICB_to_NHS_England_(Region)_(July_2022)_Lookup.csv") %>%
  select(-ObjectId)
lsoa_region <- inner_join(lsoa_ics, ics_region)

ics_population <- merge(GP_lsoa_data, lsoa_ics, by.x = "lsoa_code", by.y = "LSOA11CD") %>%
  group_by(SICBL22CD, SICBL22NM, SICBL22CDH) %>%
  summarise(population = sum(number_of_patients))

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
  filter(under79_denominator_21_22 != 0 |
           over80_denominator_21_22 != 0)

Hyper21_22_cl <- Hyper21_22 %>%
  subset(., select = c(sub_icb_loc_ods_code, sub_icb_loc_ons_code, sub_icb_loc_name, 
                       practice_code, practice_name, list_size_21_22, under79_21_22, over80_21_22, register_21_22, 
                       prevalence_percent_21_22, under79_numerator_21_22, under79_denominator_21_22, 
                       under79_achievement_net_exceptions_21_22, under79_percent_receiving_intervention_21_22,
                       over80_numerator_21_22, over80_denominator_21_22, over80_achievement_net_exceptions_21_22,
                       over80_percent_receiving_intervention_21_22)) %>%
  mutate(over80_prev = over80_denominator_21_22/over80_21_22, 
         under79_prev = under79_denominator_21_22/under79_21_22) %>%
  filter(over80_achievement_net_exceptions_21_22 != ":")

Hyper21_22_cl$over80_achievement_net_exceptions_21_22 <- as.numeric(Hyper21_22_cl$over80_achievement_net_exceptions_21_22)
Hyper21_22_cl$over80_percent_receiving_intervention_21_22 <- as.numeric(Hyper21_22_cl$over80_percent_receiving_intervention_21_22)

GP_lsoa_age_data <- merge(GP_age_dist_cl, GP_lsoa_data, by.x = 'org_code', by.y = 'practice_code')


sum(is.na(Hyper21_22_cl$over80_achievement_net_exceptions_21_22))

lsoa_hypertension_prev <- merge(GP_lsoa_age_data, Hyper21_22_cl, by.x = 'org_code', by.y = 'practice_code') %>%
  # Create new column for the percentage of patients a GP serves in each LSOA 
  mutate(gp_coverage = (number_of_patients/lsoa_pop), 
         total80plus = male80plus + female80plus,
         tot_o80 = (number_of_patients/all_all)*total80plus,
         exp_hyp = (exp_hyp_male*perc_male + exp_hyp_female*perc_female)*100, 
         exp_u79_hyp = (exp_u79_hyp_male*male_u79_perc + exp_u79_hyp_female*female_u79_perc)*100)
n_distinct(lsoa_hypertension_prev$sub_icb_loc_name)
n_distinct(lsoa_hypertension_prev$sub_icb_loc_ods_code)
n_distinct(lsoa_hypertension_prev$sub_icb_loc_ons_code)

################################# Hypertension Analysis ###############################################
# checking the mean and median of hypertension prevalence by GP 
mean(lsoa_hypertension_prev$prevalence_percent_21_22) # 12.61%
median(lsoa_hypertension_prev$prevalence_percent_21_22) # 13.03%

# aggregating prevalence from practice to lsoa level
lsoa_grouped <- lsoa_hypertension_prev %>%
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
         age_std_u79_prev = mean(obs_u79_prev)*u79_obs_over_exp, 
         hypertension_cases = round((age_std_prev/100)*lsoa_pop)) 

lsoa_hypertension_imd <- merge(lsoa_age_adj, LSOA_imd_cl, by.x = 'lsoa_code', by.y = 'lsoa_code_2011') 

lsoa_hypertension_imd_decile <- lsoa_hypertension_imd %>%
  mutate(across(imd_decile, as_factor)) %>%
  group_by(imd_decile) %>%
  summarise(population = sum(lsoa_pop), 
            hypertension_pop = sum(hypertension_cases),
            alternative_prev = mean(age_std_prev),
            age_std_prevalence = sum((lsoa_pop/population)*age_std_prev)) 

ggplot(lsoa_hypertension_imd_decile, aes(x = imd_decile, fill = imd_decile)) +
  scale_fill_manual(values = c("#fcdadb", "#f5c2c3", "#efaaaa", "#e89192", "#e2797a", "#db6161", "#d54949", "#ce3031", "#c81818", "#c10000")) + 
  geom_col(aes(y = age_std_prevalence)) +
  labs(x = "IMD Decile (1 is Most Deprived)", 
       y = "Age Standardised Prevalence") +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 20),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.position = "none") 

hypertension_imd <- lm(age_std_prev ~ imd_decile, data = lsoa_hypertension_imd)
summary(hypertension_imd)

# Merging at ICS and ranking 
ics_grouped <- merge(lsoa_age_adj, lsoa_ics, by.x = 'lsoa_code', by.y = 'LSOA11CD') %>%
  group_by(SICBL22CD, SICBL22NM, SICBL22CDH) %>%
  mutate(age_std_prev_rank = rank(age_std_prev, ties.method = "min")) 

# comparing mean and median in new df to data reported by GPs 
mean(lsoa_grouped$obs_hyper_prev) # 14.30%
median(lsoa_grouped$obs_hyper_prev) #14.55%

# Age Standardised Dataframe 
mean(lsoa_age_adj$age_std_prev) # 15.11

#### Plotting ####
# Creating a Shapefile to plot prevalence by LSOA
hypertension_prev_shp <- merge(ENG_LSOA11, lsoa_age_adj, by.x = 'geo_code', by.y = 'lsoa_code')

national_age_std_prev <- tm_shape(hypertension_prev_shp) + 
  tm_polygons(col = 'age_std_prev', border.alpha = 0.5, title = "Age Std. Prev.", 
              legend.hist = TRUE, palette = "-RdBu", breaks = c(0, 5, 10, 15, 20, 25, Inf), 
              lables = c("0-5", "5-10", "10-15", "15-20", "20-25", "25+")) + 
  tm_compass(position = c("right", "top")) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(main.title = "Age Standardised Hypertension Prevalence Rate", legend.outside = TRUE)

# removing Ealing
hyper_prev_no_ealing <- hypertension_prev_shp %>%
  filter(!grepl('Ealing', name))

# Plot at CCG Level 
ics_borders <- merge(Eng_ICS, ics_region, by = c('SICBL22CD', 'SICBL22NM'))

inter_ics_lsoa_borders <- merge(ENG_LSOA11, ics_grouped, by.x = 'geo_code', by.y = 'lsoa_code')
shp_df <- inner_join(inter_ics_lsoa_borders, ics_region) %>%
  select(-c(geo_label:name, LAD22CD, LAD22NM))

# separate file that aggregates the data
ics_hypertension_prev <- ics_grouped %>%
  group_by(SICBL22CD, SICBL22NM, SICBL22CDH) %>%
  summarise(population = sum(lsoa_pop),
            avg_prevalence_21_22 = sum(obs_hyper_prev*(lsoa_pop/population)), 
            exp_hyp_21_22 = mean(exp_hyper_prev), 
            obs_over_exp_22 = mean(obs_over_exp), 
            age_std_prev_21_22 = sum(age_std_prev*(lsoa_pop/population)),
            avg_u79_achievement_21_22 = mean(u79_achievement), 
            avg_u79_intervention_21_22 = mean(u79_intervention), 
            avg_o80_achievement_21_22 = mean(o80_achievement), 
            avg_o80_intervention_21_22 = mean(o80_intervention)) %>%
  ungroup() %>%
  mutate(age_std_prev_rank = rank(age_std_prev_21_22, ties.method = "min")) # 1 is Lowest Prevalence

regional_hypertension_prev <- inner_join(ics_hypertension_prev, ics_region) %>%
  group_by(NHSER22NM, NHSER22CDH) %>%
  summarise(region_pop = sum(population), 
            hypertension_prev_21_22 = sum((population/region_pop)*avg_prevalence_21_22), 
            age_std_prev_21_22 = sum((population/region_pop)*age_std_prev_21_22))
  

ics_hypertension_shp <- merge(ics_borders, ics_hypertension_prev, by = c('SICBL22CD', 'SICBL22NM', 'SICBL22CDH'))

# Creating Table and Plot for Top 25 LSOAs by Hypertension
top25_lsoa <- ics_grouped %>%
  filter(age_std_prev > 20.86) %>%
  ungroup() %>%
  select(-c(obs_u79_prev:u79_obs_over_exp, age_std_u79_prev))

top25_lsoa_table <- top25_lsoa %>%
  select(SICBL22NM, LSOA11NM, age_std_prev) %>%
  arrange(SICBL22NM, desc(age_std_prev)) %>%
  gt(groupname_col = "SICBL22NM", rowname_col = "LSOA11NM") %>%
  tab_style(style = cell_text(align = "left", indent = px(25)), locations = cells_stub()) %>%
  fmt_percent(columns = c(age_std_prev), decimals = 2, scale_values = F) %>%
  tab_header(title = "Top 25 Highest Prevalence LSOAs") %>%
  gt_theme_538() %>%
  cols_label(SICBL22NM = "ICS", 
             LSOA11NM = "LSOA Name", 
             age_std_prev = "Age Standardised Prevalence")
top25_lsoa_table

top25_lsoa_prev <- ics_grouped %>%
  arrange(age_std_prev) %>%
  tail(25) %>%
  ungroup() %>%
  select(SICBL22NM, LSOA11NM, age_std_prev) 

top25_lsoa_table %>% gtsave('Top 25 LSOA Hypertension Rates.png')

bottom25_lsoa_prev <- ics_grouped %>%
  arrange(age_std_prev) %>%
  head(25) %>%
  ungroup() %>%
  select(SICBL22NM, LSOA11NM, age_std_prev)
  
bottom25_lsoa_prev_table <- bottom25_lsoa_prev %>%
  arrange(SICBL22NM, age_std_prev) %>%
  gt(groupname_col = "SICBL22NM", rowname_col = "LSOA11NM") %>%
  tab_style(style = cell_text(align = "left", indent = px(25)), locations = cells_stub()) %>%
  fmt_percent(columns = c(age_std_prev), decimals = 2, scale_values = F) %>%
  tab_header(title = "Top 25 Highest Prevalence LSOAs") %>%
  gt_theme_538() %>%
  cols_label(SICBL22NM = "ICS", 
             LSOA11NM = "LSOA Name", 
             age_std_prev = "Age Standardised Prevalence")
bottom25_lsoa_prev_table

write_csv(top25_lsoa_prev, "Highest Age-Standardised Hypertension Prevalence LSOAs.csv")
write_csv(bottom25_lsoa_prev, "Lowest Age-Standardised Hypertension Prevalence LSOAs.csv")

top25_lsoa_shp <- inner_join(Eng_ICS, top25_lsoa)
top25_lsoa_map <- tm_shape(Eng_ICS) + 
  tm_borders(col = "black", alpha = 1) +
  tm_shape(top25_lsoa_shp) + 
  tm_graticules(alpha = 0.1, lines = F) +
  tm_fill(col = "#f2748c", alpha = 0.8) +
  tm_compass(position = c("left", "top")) + 
  tm_scale_bar() + 
  tm_layout(frame = F) 
top25_lsoa_map

par(mfrow = c(1,2))
top25_lsoa_table
top25_lsoa_map

jpeg("Top 25 Highest Hypertension Prevalence LSOA.jpeg", width = 1400, height= 1200, res = 300)
print(top25_lsoa_map)
dev.off()

# Plot ICS Distribution  
hypertension_prev21_map <- tm_shape(ics_hypertension_shp) + 
  tm_polygons(col = "age_std_prev_21_22", palette =lcp_blue_to_red, title = "A) ii) Age Std. Hypertension Prevalence (2021)", 
              breaks = c(-Inf, 14, 14.5, 15, 15.5, 16, 16.5, Inf), 
              labels = c("Less than 14%", "14 to 14.5%", "14.5 to 15%", "15 to 15.5%", 
                         "15.5 to 16%", "16 to 16.5%", "More than 16.5%")) +
  tm_compass(position = c("right", "top")) + 
  tm_scale_bar() +
  tm_layout(frame = F, legend.outside = T, legend.outside.position = "top", fontfamily = "serif", 
            legend.title.fontface = "bold", legend.title.size = 1,
            legend.text.size = 0.6)

# Creating Deciles for ICS 
ics_hypertension_shp$percentile <- ntile(ics_hypertension_shp$age_std_prev_21_22, 100)

top10_ics <- ics_hypertension_shp %>%
  filter(percentile >= 91) %>%
  select(-c(OBJECTID:SHAPE_Area, ICB22CD:ICB22NM, avg_u79_achievement_21_22:percentile)) %>%
  st_drop_geometry()

bottom10_ics <- ics_hypertension_shp %>%
  arrange(age_std_prev_21_22) %>%
  head(10) %>%
  select(-c(OBJECTID:SHAPE_Area, ICB22CD:ICB22NM, avg_u79_achievement_21_22:percentile)) %>%
  st_drop_geometry()

top10_hypertension_prev <- top10_ics %>%
  select(SICBL22NM, age_std_prev_21_22, NHSER22NM) %>%
  arrange(desc(age_std_prev_21_22)) %>%
  gt() %>%
  fmt_percent(columns = age_std_prev_21_22, decimals = 2, scale_values = F) %>%
  gt_theme_538() %>%
  cols_label(SICBL22NM = "ICS", 
             age_std_prev_21_22 = "Age Std. Hypertension Prev.", 
             NHSER22NM = "Region") %>%
  as_raw_html()

bottom10_hypertension_prev <- bottom10_ics %>%
  select(SICBL22NM, age_std_prev_21_22, NHSER22NM) %>%
  arrange(age_std_prev_21_22) %>%
  gt() %>%
  fmt_percent(columns = age_std_prev_21_22, decimals = 2, scale_values = F) %>%
  gt_theme_538() %>%
  cols_label(SICBL22NM = "ICS", 
             age_std_prev_21_22 = "Age Std. Hypertension Prev.", 
             NHSER22NM = "Region") %>%
  as_raw_html()

hyp_prevalence_tables <- data.frame(top10 = top10_hypertension_prev, 
                                   bottom10 = bottom10_hypertension_prev)

hyp_prevalence_tables %>%
  gt() %>%
  fmt_markdown(columns = everything()) %>%
  cols_label(top10 = "10 Highest Hypertension Prevalence ICS", 
             bottom10 = "10 Lowest Hypertension Prevalence ICS") %>%
  gtsave("Hypertension Prevalence Tables.png", vwidth = 1920, vheight = 1080)



write_csv(top10_ics, "Top 10 Hypertension Prevalence by ICS.csv")
write_csv(bottom10_ics, "Bottom 10 Hypertension Prevalence by ICS.csv")

# Plotting the Top Decile of CCGs by Prevalence
top10_ics_map <- tm_shape(Eng_ICS) + 
  tm_borders(col = "black") +
  tm_shape(top10_ics) + 
  tm_graticules(alpha = 0.2, lines = F) +
  tm_fill(col = "#f2748c", alpha = 0.8) +
  tm_compass(position = c("left", "top")) + 
  tm_scale_bar() + 
  tm_layout(frame = F, compass.type = "4star") 
top10_ics_map
jpeg("Top 10 Highest Hypertension Prevalence ICS.jpeg", width= 1400, height= 1200, res = 300)
print(top10_ics_map)
dev.off()

top10_ics_table <- top10_ics %>%
  select(-c(SICBL22CD, SICBL22CDH, NHSER22CD, NHSER22CDH, exp_hyp_21_22, obs_over_exp_22)) %>%
  st_drop_geometry() %>%
  arrange(NHSER22NM, desc(age_std_prev_21_22)) %>%
  gt(groupname_col = "NHSER22NM", rowname_col = "SICBL22NM") %>%
  tab_style(style = cell_text(align = "left", indent = px(25)), locations = cells_stub()) %>%
  fmt_percent(columns = c(age_std_prev_21_22, avg_prevalence_21_22), decimals = 2, scale_values = F) %>%
  tab_header(title = "Top 10 Highest Prevalence ICS") %>%
  gt_theme_538() %>%
  cols_label(SICBL22NM = "ICS", 
             NHSER22NM = "Region", 
             age_std_prev_21_22 = "Age Standardised Prevalence",
             avg_prevalence_21_22 = "Observed Prevalence")
top10_ics_table

top10_ics_table %>% gtsave('Top 10 ICS Hypertension Rates.docx')

# Subsetting CCGs by Region
# Obtaining the Region Boundaries
midlands_ics_borders <- subset(ics_borders, NHSER22NM == 'Midlands')
north_east_ics_borders <- subset(ics_borders, NHSER22NM == 'North East and Yorkshire')
north_west_ics_borders <- subset(ics_borders, NHSER22NM == 'North West')
london_ics_borders <- subset(ics_borders, NHSER22NM == 'London')
south_east_ics_borders <- subset(ics_borders, NHSER22NM == 'South East')
south_west_ics_borders <- subset(ics_borders, NHSER22NM == 'South West')
east_eng_ics_borders <- subset(ics_borders, NHSER22NM == 'East of England')

# Obtaining LSOA boundaries within Regions
midlands_lsoa_hypertension <- subset(shp_df, NHSER22NM == 'Midlands') %>%
  rename(lsoa_code = geo_code)
north_east_lsoa_hypertension <- subset(shp_df, NHSER22NM == 'North East and Yorkshire') %>%
  rename(lsoa_code = geo_code)
north_west_lsoa_hypertension <- subset(shp_df, NHSER22NM == 'North West') %>%
  rename(lsoa_code = geo_code)
london_lsoa_hypertension <- subset(shp_df, NHSER22NM == 'London') %>%
  rename(lsoa_code = geo_code)
south_east_lsoa_hypertension <- subset(shp_df, NHSER22NM == 'South East') %>%
  rename(lsoa_code = geo_code)
south_west_lsoa_hypertension <- subset(shp_df, NHSER22NM == 'South West')  %>%
  rename(lsoa_code = geo_code)
east_eng_lsoa_hypertension <- subset(shp_df, NHSER22NM == 'East of England')  %>%
  rename(lsoa_code = geo_code)

# Looking at Age standardised Prevalence
ggplot(ics_grouped) + 
  aes(x = reorder(SICBL22NM, -age_std_prev), y = age_std_prev, color = SICBL22NM) + 
  geom_boxplot() + coord_flip() +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', 
             size = 1) +
  theme(legend.position = "none")

#### Breaking it Down by Region ####
# Midlands - Prevalence 
ggplot(midlands_lsoa_hypertension) + 
  aes(x = reorder(SICBL22NM, -age_std_prev), y = age_std_prev, fill = SICBL22NM) +
  #  scale_fill_brewer(palette = "RdBu") + 
  geom_boxplot(fatten = NULL) + 
  coord_flip() +
  stat_summary(fun = "mean",geom = "point", size = 2, color = "white") +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', lwd = 1) +
  geom_text(aes(0, mean(age_std_prev), label = 'Midlands Mean', vjust = -0.5, hjust = -0.1)) +
  geom_hline(yintercept = 17.09, color = 'red', lwd = 1, linetype = 'dashed') +
  geom_text(aes(0, 17.09, label = 'National Average', color = 'red', vjust = -0.5, hjust = 1.1)) +
  theme(legend.position = "none") +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "ICS", 
       title = "Standardised Hypertension Prevalence in the Midlands")

tm_shape(midlands_lsoa_hypertension) +
  tm_fill(col = 'age_std_prev', border.alpha = 0.5, title = "Hypertension Prevalence %", 
          legend.hist = TRUE, 
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0,10, 12.5, 15, 17.5, 20, Inf), 
          lables = c("<10", "10-12.5", "12.5-15", "15-17.5", "17.5-20", "20+")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in the Midlands', legend.outside = TRUE) +
  tm_shape(midlands_ics_borders) +
  tm_borders()

################################### Superseded ################################# 
tm_shape(midlands_lsoa_hypertension) +
  tm_fill(col = 'obs_over_exp', title = "Obs:Exp Ratio", 
          legend.hist = TRUE, palette = "-RdBu",
          breaks = c(0, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, Inf), 
          labels = c("<0.5", "0.5-0.7", "0.7-0.9", "0.9-1.1", "1.1-1.3", "1.3-1.5", ">1.5")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Expected vs Observed Hypertension Rates in the Midlands', legend.outside = TRUE) +
  tm_shape(midlands_ics_borders) +
  tm_borders('black', lwd = 1)
####------------------------------------------------------------------------####

# North East and Yorkshire
ggplot(north_east_lsoa_hypertension) + 
  aes(x = reorder(SICBL22NM, -age_std_prev), y = age_std_prev, fill = SICBL22NM) + 
  geom_boxplot(fatten = NULL) + 
  coord_flip() +
  stat_summary(fun = "mean",geom = "point", size = 2, color = "white") +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', lwd = 1) +
  geom_text(aes(0, mean(age_std_prev), label = 'NE England Mean', vjust = -0, hjust = -0.1)) +
  geom_hline(yintercept = 17.09, color = 'red', lwd = 1, linetype = 'dashed') +
  geom_text(aes(0, 17.09, label = 'National Average', color = 'red', vjust = -0.5, hjust = 1.1)) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "CCG", 
       title = "Standardised Hypertension Prevalence in NE England")

tm_shape(north_east_lsoa_hypertension) +
  tm_fill(col = 'age_std_prev', title = "Hypertension Prevalence %", 
          legend.hist = TRUE,  
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0,10, 12.5, 15, 17.5, 20, Inf), 
          lables = c("<10", "10-12.5", "12.5-15", "15-17.5", "17.5-20", "20+")) + 
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in NE England', legend.outside = TRUE) +
  tm_shape(north_east_ics_borders) +
  tm_borders('black', lwd = 1)

# NW England
ggplot(north_west_lsoa_hypertension) + 
  aes(x = reorder(SICBL22NM, -age_std_prev), y = age_std_prev, fill = SICBL22NM) + 
  geom_boxplot(fatten = NULL) + 
  coord_flip() +
  stat_summary(fun = "mean",geom = "point", size = 2, color = "white") +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', lwd = 1) +
  geom_text(aes(0, mean(age_std_prev), label = 'NW England Mean', vjust = -0.1, hjust = -0.1)) +
  geom_hline(yintercept = 17.09, color = 'red', lwd = 1, linetype = 'dashed') +
  geom_text(aes(0, 17.09, label = 'National Average', color = 'red', vjust = -0.5, hjust = 1.1)) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "CCG", 
       title = "Standardised Hypertension Prevalence in NW England")

tm_shape(north_west_lsoa_hypertension) +
  tm_fill(col = 'age_std_prev', title = "Hypertension Prevalence %", 
          legend.hist = TRUE,  
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0,10, 12.5, 15, 17.5, 20, Inf), 
          lables = c("<10", "10-12.5", "12.5-15", "15-17.5", "17.5-20", "20+")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in NW England', legend.outside = TRUE) +
  tm_shape(north_west_ics_borders) +
  tm_borders('black', lwd = 1)

# London 
ggplot(london_lsoa_hypertension) + 
  aes(x = reorder(SICBL22NM, -age_std_prev), y = age_std_prev, fill = SICBL22NM) + 
  geom_boxplot(fatten = NULL) + 
  coord_flip() +
  stat_summary(fun = "mean",geom = "point", size = 2, color = "white") +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', lwd = 1) +
  geom_text(aes(0, mean(age_std_prev), label = 'London Mean', vjust = -0.5, hjust = 1.1)) +
  geom_hline(yintercept = 17.09, color = 'red', lwd = 1, linetype = 'dashed') +
  geom_text(aes(0, 17.09, label = 'National Average', color = 'red', vjust = -0.5, hjust = -0.1)) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "CCG", 
       title = "Standardised Hypertension Prevalence in Lonon")

tm_shape(london_lsoa_hypertension) +
  tm_fill(col = 'age_std_prev', title = "Hypertension Prevalence %", 
          legend.hist = TRUE, 
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0,10, 12.5, 15, 17.5, 20, Inf), 
          lables = c("<10", "10-12.5", "12.5-15", "15-17.5", "17.5-20", "20+")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in London', legend.outside = TRUE) +
  tm_shape(london_ics_borders) +
  tm_borders('black', lwd = 1)

# SE England
ggplot(south_east_lsoa_hypertension) + 
  aes(x = reorder(SICBL22NM, -age_std_prev), y = age_std_prev, fill = SICBL22NM) + 
  geom_boxplot(fatten = NULL) + 
  coord_flip() +
  stat_summary(fun = "mean",geom = "point", size = 2, color = "white") +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', lwd = 1) +
  geom_text(aes(0, mean(age_std_prev), label = 'SE England Mean', vjust = -0.5, hjust = 1.1)) +
  geom_hline(yintercept = 17.09, color = 'red', lwd = 1, linetype = 'dashed') +
  geom_text(aes(0, 17.09, label = 'National Average', color = 'red', vjust = -0.5, hjust = -0.1)) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "CCG", 
       title = "Standardised Hypertension Prevalence in SE England")

tm_shape(south_east_lsoa_hypertension) +
  tm_fill(col = 'age_std_prev', title = "Hypertension Prevalence %", 
          legend.hist = TRUE, 
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0,10, 12.5, 15, 17.5, 20, Inf), 
          lables = c("<10", "10-12.5", "12.5-15", "15-17.5", "17.5-20", "20+")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in SE England', legend.outside = TRUE) +
  tm_shape(south_east_ics_borders) +
  tm_borders('black', lwd = 1)

# SW England
ggplot(south_west_lsoa_hypertension) + 
  aes(x = reorder(SICBL22NM, -age_std_prev), y = age_std_prev, fill = SICBL22NM) + 
  geom_boxplot(fatten = NULL) + 
  coord_flip() +
  stat_summary(fun = "mean",geom = "point", size = 2, color = "white") +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', lwd = 1) +
  geom_text(aes(0, mean(age_std_prev), label = 'SW England Mean', vjust = -0.5, hjust = 1.1)) +
  geom_hline(yintercept = 17.09, color = 'red', lwd = 1, linetype = 'dashed') +
  geom_text(aes(0, 17.09, label = 'National Average', color = 'red', vjust = -0.5, hjust = -0.1)) +
  theme(legend.position = "none")  +
  labs(y = "Age Standardised Prevalence Rate (%)", x = "CCG", 
       title = "Standardised Hypertension Prevalence in SW England")

tm_shape(south_west_lsoa_hypertension) +
  tm_fill(col = 'age_std_prev', title = "Hypertension Prevalence %", 
          legend.hist = TRUE, 
          palette = c("#2166AC", "#4393C3", "#D1E5F0", "#FDDBC7", "#D6604D", "#B2182B"), 
          breaks = c(0,10, 12.5, 15, 17.5, 20, Inf), 
          lables = c("<10", "10-12.5", "12.5-15", "15-17.5", "17.5-20", "20+")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(main.title = 'Hypertension Prevalence in SW England', legend.outside = TRUE) +
  tm_shape(south_west_ics_borders) +
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

# Investigating Regionally
ggplot(ics_hypertension_prev) + 
  aes(x = reorder(NHSER22NM, -age_std_prev), y = age_std_prev, fill = NHSER22NM) + 
  geom_boxplot(fatten = NULL) +
  stat_summary(fun = "mean", geom = "point", size = 2, color = "white") + 
  coord_flip() +
  geom_hline(aes(yintercept=mean(age_std_prev)), color = 'black', size = 1) + 
  geom_text(aes(0, mean(age_std_prev), label = "National Average", vjust = -0.5, hjust = 1.05)) +
  theme(legend.position = "none") +
  labs(x = "Region", y = "Age Standardised Prevalence Rate (%)", 
       title = "Standardised Hypertension Prevalence by Region")

#### Exporting Data for Further Analysis ####
write_csv(Hyper21_22_cl, "QOF Data 2021_22.csv")
write_csv(ics_grouped, "Age-Adjusted LSOA Hypertension Data.csv")
write_csv(ics_hypertension_prev, "ICS Hypertension Data.csv")
write_csv(GP_age_dist_cl, "Cleaned GP Age Distribution.csv")
write_csv(GP_lsoa_data, "GP LSOA Distribution Data.csv")
write_csv(lsoa_region, "LSOA to Region Lookup File.csv")
write_csv(ics_population, "ICS Population.csv")
write_csv(lsoa_hypertension_prev, "GP to LSOA Hypertension Data.csv")
write_csv(LSOA_imd_cl, "Cleaned LSOA IMD.csv")

##### GP Coverage Analysis ###########
gp_coverage_rate <- lsoa_hypertension_prev %>%
  select(org_code, lsoa_code, lsoa_pop, number_of_patients, practice_name, 
         gp_coverage, prevalence_percent_21_22, exp_hyp)

gp_cov90 <- gp_coverage_rate %>%
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

exp_diff5plus <- merge(diff_10plus, lsoa_med_age, by = c('lsoa_code', 'lsoa_name')) %>%
  select(-c(age0_15:age75plus_perc, starts_with('la'))) %>%
  mutate(exp_diff = exp_hyp - exp_hyp_no_sex*100) %>%
  filter(exp_diff <= -5 | exp_diff >= 5)

obs_diff5plus <-  merge(diff_10plus, lsoa_med_age, by = c('lsoa_code', 'lsoa_name')) %>%
  select(-c(age0_15:age75plus_perc, starts_with('la'))) %>%
  mutate(obs_diff = prevalence_percent_21_22 - exp_hyp_no_sex*100) %>%
  filter(obs_diff <= -5 | obs_diff >= 5)
