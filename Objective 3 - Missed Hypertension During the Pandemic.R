# set working directory 
setwd("C:/Users/SIMC/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - 03 CVD Secondary prevention/Analysis")

#### Installing Packages ####
library(tidyverse)
library(tmap)
library(sf)
library(janitor)
library(stringr)
library(ggplot2)
library(extrafont)
loadfonts(device = "win")

# loading palette 
lcp_blue_to_red <- c("#002f5f", "#345c80", "#627e9b",  "#8da8ad", "#c1d3d6", "#fcdadb", "#f595a3", "#e93f6f", "#821a4d")

# Loading Data
lsoa_ics <- read_csv("~/Lookup Files/LSOA_(2011)_to_Sub_ICB_Locations_to_Integrated_Care_Boards_(July_2022)_Lookup_in_England.csv")
# Population Data 
ics_population <- read_csv("ICS Population.csv")
# Lookup File
ics_region <- read_csv("~/Lookup Files/Sub_ICB_Locations_to_ICB_to_NHS_England_(Region)_(July_2022)_Lookup.csv") %>%
  select(-ObjectId)
# ICS Shapefile
Eng_ICS <- st_read("~/Shapefiles/Sub-ICB Shapefiles/SICBL_JUL_2022_EN_BUC.shp")
# Rename the three ICS with "Isle" so there are no difficulties with merging
Eng_ICS$SICBL22NM <- gsub("NHS Cornwall and The Isles Of Scilly ICB - 11N", "NHS Cornwall and the Isles of Scilly ICB - 11N", Eng_ICS$SICBL22NM)
Eng_ICS$SICBL22NM <- gsub("NHS Hampshire and Isle Of Wight ICB - 10R", "NHS Hampshire and Isle of Wight ICB - 10R", Eng_ICS$SICBL22NM)
Eng_ICS$SICBL22NM <- gsub("NHS Hampshire and Isle Of Wight ICB - D9Y0V", "NHS Hampshire and Isle of Wight ICB - D9Y0V", Eng_ICS$SICBL22NM)

GP_age_dist_cl <- read_csv("Cleaned GP Age Distribution.csv")

GP_lsoa_dist <- read_csv("GP LSOA Distribution Data.csv")

# Load in Data to Change the CCG Codes
ccg_2019_codes <- read_csv("~/Hypertension/CCG merge data/merges_up_to_2019.csv") %>%
  clean_names()
ccg_2020_codes <- read_csv("~/Hypertension/CCG merge data/merges_2020.csv") %>%
  clean_names()
ccg_2021_codes <- read_csv("~/Hypertension/CCG merge data/merges_2021.csv") %>%
  clean_names()

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
  drop_na(ccg_geography_code) %>%
  subset(., under79_denominator_19_20 != 0) %>% # Drops 25 GPs (6720 to 6695)
  subset(., over80_denominator_19_20 != 0) %>%
  # Changing the reported prevalence rate for this GP to the previous year due to misreporting of prevalence rates due 
  # to GP merger/closure
  mutate(prevalence_percent_19_20=ifelse(practice_code=="Y03051",8.67,prevalence_percent_19_20), 
         prevalence_percent_19_20=ifelse(practice_code =="F85002", 13.36, prevalence_percent_19_20))

Hyper19_20$under79_achievement_net_exceptions_19_20 <- as.numeric(Hyper19_20$under79_achievement_net_exceptions_19_20)
Hyper19_20$under79_percent_receiving_intervention_19_20 <- as.numeric(Hyper19_20$under79_percent_receiving_intervention_19_20)
Hyper19_20$over80_achievement_net_exceptions_19_20 <- as.numeric(Hyper19_20$over80_achievement_net_exceptions_19_20)
Hyper19_20$over80_percent_receiving_intervention_19_20 <- as.numeric(Hyper19_20$over80_percent_receiving_intervention_19_20)

Hyper19_20_age_adj <- merge(Hyper19_20, GP_age_dist_cl, by.x = "practice_code", by.y = "org_code") %>%
  mutate(exp_hyp = (exp_hyp_male*perc_male + exp_hyp_female*perc_female)*100) %>%
  select(-c(percentage_point_16:max_14_per_practice, all_female:exp_hyp_80plus_female)) %>%
   mutate(obs_over_exp = prevalence_percent_19_20/exp_hyp, 
         age_std_prev = mean(prevalence_percent_19_20)*obs_over_exp)

hse_hyp_prev <- read_csv("~/Hypertension/hypertension_prevalence_estimate_HSE.csv") %>%
  clean_names() %>%
  rename(hse_prevalence = percent, 
         practice_code = code)

total_burden <- inner_join(Hyper19_20_age_adj, hse_hyp_prev) %>%
  mutate(prev_diff = prevalence_percent_19_20 - hse_prevalence)

lsoa_burden <- inner_join(total_burden, GP_lsoa_dist) %>%
  mutate(gp_coverage = number_of_patients/lsoa_pop) %>%
  group_by(lsoa_code) %>%
  summarise(lsoa_pop = mean(lsoa_pop), 
            obsprev_19 = sum(gp_coverage*prevalence_percent_19_20, na.rm = T),
            hse_prev = sum(gp_coverage*hse_prevalence, na.rm = T), 
            undiagnosed_prev = sum(gp_coverage*prev_diff*-1, na.rm = T)) %>%
  mutate(hypertension_cases = round(obsprev_19*lsoa_pop/100), 
         undiagnosed_cases = round(undiagnosed_prev*lsoa_pop/100),
         total_burden_rate = obsprev_19 + undiagnosed_prev, 
         total_burden_cases = hypertension_cases+undiagnosed_cases) %>%
  merge(., lsoa_ics, by.x = "lsoa_code", by.y = "LSOA11CD") %>%
  group_by(SICBL22CD, SICBL22NM, SICBL22CDH) %>%
  rename(LSOA11CD = lsoa_code) %>%
  mutate(hypertension_prev_rank = rank(obsprev_19, ties.method = "min"), 
         undiagnosed_prev_rank = rank(undiagnosed_prev, ties.method = "min"),
         hypertension_prev_quintile = ntile(obsprev_19, 5),
         hypertension_case_quintile = ntile(hypertension_cases, 5), 
         undiagnosed_prev_quintile = ntile(undiagnosed_prev, 5), 
         undiagnosed_cases_quintile = ntile(undiagnosed_cases, 5),
         burden_rate_quintile = ntile(total_burden_rate, 5), 
         burden_cases_quintile = ntile(total_burden_cases, 5))

write_csv(lsoa_burden, "lsoa_total_burden_2019_data.csv")

ics_total_burden <- total_burden %>%
  merge(., ccg_2021_codes, by.x = 'ccg_code', 
        by.y = 'old_code', all = TRUE) %>%
  # encoding so that if new code is different, it will take the new code
  mutate(., new_code = case_when(ccg_code != new_code ~ new_code, 
                                 T ~ ccg_code)) %>%
  # removing old code column and renaming the column
  select(-ccg_code) %>%
  rename(SICBL22CDH = new_code) %>%
  group_by(SICBL22CDH) %>%
  summarise(pop = sum(list_size_19_20), 
            obsprev_19 = sum(prevalence_percent_19_20*(list_size_19_20/pop)), 
            undiagnosed_prev = sum(prev_diff*-1*(list_size_19_20/pop))) %>%
  select(-pop) %>%
  inner_join(ics_population) %>%
  mutate(hypertension_cases = round(obsprev_19*population/100), 
         undiagnosed_cases = round(undiagnosed_prev*population/100),
         total_burden_rate = obsprev_19 + undiagnosed_prev, 
         total_burden_cases = hypertension_cases+undiagnosed_cases,
         hypertension_prev_rank = rank(obsprev_19, ties.method = "min"), 
         undiagnosed_prev_rank = rank(undiagnosed_prev, ties.method = "min"),
         hypertension_prev_quintile = ntile(obsprev_19, 5),
         hypertension_case_quintile = ntile(hypertension_cases, 5), 
         undiagnosed_prev_quintile = ntile(undiagnosed_prev, 5), 
         undiagnosed_cases_quintile = ntile(undiagnosed_cases, 5),
         burden_rate_quintile = ntile(total_burden_rate, 5), 
         burden_cases_quintile = ntile(total_burden_cases, 5)) %>%
  select(SICBL22CD, SICBL22NM, SICBL22CDH, population, obsprev_19, undiagnosed_prev, 
         hypertension_cases:burden_cases_quintile)

write_csv(ics_total_burden, "ics_total_burden_2019_data.csv")
  
lsoa_19_20_age_adj <- inner_join(Hyper19_20_age_adj, GP_lsoa_dist) %>%
  mutate(gp_coverage = number_of_patients/lsoa_pop, 
         under80_rate = under79_denominator_19_20/register_19_20) %>%
  group_by(lsoa_code) %>%
  summarise(lsoa_pop = mean(lsoa_pop), 
            obsprev_19 = sum(gp_coverage*prevalence_percent_19_20, na.rm = T), 
            agestdprev_19 = sum(gp_coverage*age_std_prev, na.rm = T), 
            under80_treatment = sum(gp_coverage*under79_achievement_net_exceptions_19_20, na.rm = T),
            over80_treatment = sum(gp_coverage*over80_achievement_net_exceptions_19_20, na.rm = T), 
            under80_rate = sum(gp_coverage*under80_rate)) %>%
  mutate(all_treated_percent = round((under80_rate*under80_treatment + (1-under80_rate)*over80_treatment), digits = 2), 
         treated_population = round(lsoa_pop*(all_treated_percent/100)*(obsprev_19/100))) %>%
  drop_na() %>%
  merge(., lsoa_ics, by.x = "lsoa_code", by.y = "LSOA11CD") %>%
  group_by(SICBL22CD, SICBL22NM, SICBL22CDH) %>%
  mutate(treatment_rank = rank(all_treated_percent, ties.method = "min"), 
         hypertension_treatment_quintile = ntile(all_treated_percent, 5), 
         population_treated_quintile = ntile(treated_population, 5)) %>%
  rename(LSOA11CD = lsoa_code) %>%
  select(LSOA11CD, LSOA11NM, lsoa_pop:agestdprev_19, all_treated_percent, treated_population, 
         treatment_rank:population_treated_quintile, SICBL22CD:ICB22NM, under80_treatment, over80_treatment)

write_csv(lsoa_19_20_age_adj, 
          "C:/Users/SIMC/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - 03 CVD Secondary prevention/Analysis/Dashboard Data/lsoa_hypertension_treatment_2019_data.csv")

mean(Hyper19_20_age_adj$age_std_prev)
mean(Hyper19_20_age_adj$prevalence_percent_19_20)

Hyper19_20_cl <- Hyper19_20_age_adj %>%
  group_by(ccg_code) %>%
  summarise(tot_list_size_19_20 = sum(list_size_19_20), 
            tot_register_19_20 = sum(register_19_20), 
            avg_prevalence_19_20 = mean(prevalence_percent_19_20), 
            age_std_prev_19_20 = mean(age_std_prev),
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
  
Hyper21_22_ics <- Hyper21_22 %>%
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

QOF_22 <- merge(QOF_21, Hyper21_22_ics, by.x = 'ccg_code', by.y = 'sub_icb_loc_ods_code', all = TRUE) 

treatment_2019 <- QOF_22 %>%
  select(ccg_code, tot_list_size_19_20:avg_o80_intervention_19_20) %>%
  group_by(ccg_code) %>%
  summarise(obsprev_19 = mean(avg_prevalence_19_20, na.rm = T), 
            agestdprev_19 = mean(age_std_prev_19_20, na.rm = T), 
            u79_denominator = sum(tot_u79_denominator_19_20, na.rm = T), 
            registered_patients = sum(tot_register_19_20), 
            treated_under80 = mean(avg_u79_achievement_19_20, na.rm = T), 
            treated_over80 = mean(avg_o80_achievement_19_20, na.rm = T)) %>%
  mutate(under80_rate = round((u79_denominator/registered_patients),2),
         all_treated_percent = round((under80_rate*treated_under80 + (1-under80_rate)*treated_over80), digits = 2)) %>%
  drop_na() %>%
  rename(SICBL22CDH = ccg_code) %>%
  inner_join(ics_population)

treatment_2019_data <- treatment_2019[,c(10,11,1,2,9,12)]
ics_treatment_2019_data <- treatment_2019_data %>%
  rename(ics_pop = population, 
         ics_treatment_rate = all_treated_percent, 
         hypertension_prev19 = obsprev_19) %>%
  mutate(treated_population = round(ics_pop*(ics_treatment_rate/100)*(hypertension_prev19/100)), 
         treatment_rank = rank(desc(ics_treatment_rate), ties.method = "min"), 
         hypertension_treatment_quintile = ntile(ics_treatment_rate, 5), 
         population_treated_quintile = ntile(treated_population, 5))

write_csv(ics_treatment_2019_data, "C:/Users/SIMC/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - 03 CVD Secondary prevention/Analysis/Dashboard Data/ics_hypertension_treatment_2019_data.csv")

QOF_prev <- QOF_22 %>%
  group_by(ccg_code) %>%
  summarise(obsprev_15 = mean(avg_prevalence_14_15, na.rm = TRUE), 
            obsprev_16 = mean(avg_prevalence_15_16, na.rm = TRUE), 
            obsprev_17 = mean(avg_prevalence_16_17, na.rm = TRUE), 
            obsprev_18 = mean(avg_prevalence_17_18, na.rm = TRUE), 
            obsprev_19 = mean(avg_prevalence_18_19, na.rm = TRUE), 
            agestdprev_20 = mean(age_std_prev_19_20, na.rm = T),
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
               values_to = "score") %>%
  rename(SICBL22CDH = ccg_code)

# having pivoted the data long, then want to re-integrate the columns for each variable of interest
QOF_prev_cl <- QOF_prev_long %>%
  pivot_wider(names_from = "category", 
              values_from = "score") %>%
  mutate_at('year', as.numeric) %>%
  mutate(covid = case_when(year >= 21 ~ 1, T ~ 0), 
         year = year - 15) # creating dummy variable for the covid years 

QOF_prev_cl$obsprev[QOF_prev_cl$year == 3 & QOF_prev_cl$SICBL22CDH == '14Y'] <- 13.71257

QOF_prev_20_22 <- QOF_prev_cl %>%
  filter(year >= 5)

# performing ITS 
itsa <- lm(obsprev ~ year + covid + year*covid, data = QOF_prev_cl)
summary(itsa)

model <- lm(obsprev ~ year + as.factor(SICBL22CDH) + year*as.factor(SICBL22CDH), 
            data = subset(QOF_prev_cl, year < 6))

## Using Predict
QOF_prev_20_22 <- QOF_prev_20_22 %>%
  mutate(predicted_prev = case_when(year >= 6 ~ predict(model, subset(QOF_prev_cl, year >=5)), 
                                    T ~ obsprev))

QOF_pop <- inner_join(QOF_prev_cl, ics_population)

national_model <- lm(obsprev ~ year, data = subset(QOF_pop, year < 6), weights = population) 

national_prev <- inner_join(QOF_prev_cl, ics_population) %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(total_pop = sum(population), 
            obsprev = sum((population/total_pop)*obsprev))

national_predicted <- national_prev %>%
  mutate(predicted_prev = case_when(year >= 6 ~ predict(national_model, national_prev), 
                                    T ~ obsprev))

national_pred_int <- predict(national_model, subset(national_prev, year > 5), interval = "confidence", level = 0.95)
national_prev_se <- cbind(national_predicted, national_pred_int) %>%
  mutate(prev_diff = predicted_prev - obsprev, 
         prev_diff_lb = lwr - obsprev, 
         prev_diff_ub = upr - obsprev, 
         missed_diagnoses = round(total_pop*prev_diff/100), 
         missed_diagnoses_lb = round(total_pop*prev_diff_lb/100), 
         missed_diagnoses_ub = round(total_pop*prev_diff_ub/100))

prediction.int <- predict(model, subset(QOF_prev_cl, year >= 5),interval = "prediction", level = 0.95)
prediction.int2 <- predict(model, subset(QOF_prev_cl, year >= 5),interval = "confidence", level = 0.95)
test <- cbind(QOF_prev_20_22, prediction.int)
test2 <- cbind(QOF_prev_20_22, prediction.int2) # consistent S.E for all values 

QOF_prev_20_22 <- QOF_prev_20_22 %>%
  mutate(prev_diff = obsprev-predicted_prev)

QOF_prev_pop <- merge(QOF_prev_20_22, ics_population, by = 'SICBL22CDH') %>%
  # Calculating missed absolute totals from extrapolated population
  mutate(missed = round(prev_diff*population/100, digits = 0), 
         estimated = round(predicted_prev*(population/100))) 

QOF_prev_pop_se <- inner_join(QOF_prev_pop, test2) %>%
  select(-c(agestdprev, covid)) %>%
  mutate(prev_diff_lb = case_when(year == 5 ~ 0, T ~ obsprev - lwr), 
         prev_diff_ub = case_when(year == 5 ~ 0, T ~ obsprev - upr), 
         missed_lb = round(prev_diff_lb*population/100, digits = 0), 
         missed_ub = round(prev_diff_ub*population/100, digits = 0))

QOF_prev_se <- QOF_prev_pop_se %>%
  filter(year == 6)

# 2019 Age Standardised Prevalence 
age_std_19_map <- merge(Eng_ICS, QOF_prev_pop, by = c('SICBL22CD', 'SICBL22NM')) %>%
  filter(year == 5) %>%
  select(-c(year, covid)) 

hypertension_prev19 <- tm_shape(age_std_19_map) +
  tm_polygons(col = "agestdprev", palette =lcp_blue_to_red, title = "A) i) Age Std. Hypertension Prevalence (2019)", 
              breaks = c(-Inf, 14, 14.5, 15, 15.5, 16, 16.5, Inf), 
              labels = c("Less than 14%", "14 to 14.5%", "14.5 to 15%", "15 to 15.5%", 
                         "15.5 to 16%", "16 to 16.5%", "More than 16.5%")) +
  tm_compass(position = c("right", "top")) + 
  tm_scale_bar() +
  tm_layout(frame = F, legend.outside = T, legend.outside.position = "top", fontfamily = "serif", 
            legend.title.fontface = "bold", legend.title.size = 1,
            legend.text.size = 0.6)
hypertension_prev19

lcp_red_to_blue_7col <- c("#821a4d", "#e93f6f", "#f8b7bd", "#dce6e7", "#c1cdd8", "#627e9b", "#002f5f")

hypertension_treatment_19 <- merge(Eng_ICS, treatment_2019, by = c("SICBL22CD", "SICBL22NM")) 
treatment_2019_map <- tm_shape(hypertension_treatment_19) + 
  tm_polygons(col = "all_treated_percent", palette = lcp_red_to_blue_7col, title = "Treatment Rate (2019)", 
              breaks = c(-Inf, 60, 65, 70, 75, 80, Inf)) + 
  tm_compass(position = c("right", "top")) + 
  tm_scale_bar() + 
  tm_layout(frame = FALSE, legend.title.fontface = "bold", fontfamily = "serif", 
            legend.title.size = 2, legend.text.size = .8)
treatment_2019_map



# Finding Absolute Values in Differences
missed_21 <- QOF_prev_pop %>%
  filter(year == 6) %>%
  select(., -c(year, covid)) %>%
  rename(obs_prev_21 = obsprev, 
         prev_diff_21 = prev_diff,
         missed_21 = missed, 
         predicted_prev_21 = predicted_prev,
         estimated_hypertension_pop_21 = estimated)

top_decile_missed_ics <- missed_21 %>%
  mutate(decile = ntile(missed_21, 10)) %>%
  filter(decile == 1)

top_decile_missed_ics_shp <- merge(Eng_ICS, top_decile_missed_ics, by = c('SICBL22CD', 'SICBL22NM'))

tm_shape(Eng_ICS) + 
  tm_borders(col = "black") + 
  tm_shape(top_decile_missed_ics_shp) + 
  tm_fill(col = "#00a3c7", alpha = 0.5) + 
  tm_layout(main.title = "Top Decile of Missed Diagnoses") + 
  tm_compass(position = c("left", "top")) + 
  tm_scale_bar(position = c("right", "bottom"))

missed_22 <- QOF_prev_pop %>%
  filter(year == 7) %>%
  select(., -c(year, covid)) %>%
  rename(obs_prev_22 = obsprev, 
         prev_diff_22 = prev_diff,
         undiagnosed_22 = missed,
         predicted_prev_22 = predicted_prev,
         estimated_hypertension_pop_22 = estimated)

missed_all <- inner_join(missed_21, missed_22)

ics_missed_diagnoses <- missed_all %>%
  select(SICBL22CD, SICBL22NM, SICBL22CDH, population, prev_diff_21, missed_21) %>%
  mutate(missed_diagnoses_rate = (missed_21/population)*-100, 
         missed_diagnoses_rank = rank(missed_diagnoses_rate, ties.method = "min"),
         missed_diagnoses_rate = round(case_when(missed_diagnoses_rate < 0 ~ 0, 
                                           T ~ missed_diagnoses_rate), digits = 2), 
         missed_diagnoses = case_when(missed_21 > 0 ~ 0, T ~ missed_21*-1))

# plotting differences
prev_diff_shp <- merge(Eng_ICS, ics_missed_diagnoses, by = c('SICBL22CD', 'SICBL22NM')) 

# Plotting National Trend 
national_prev <- QOF_prev_cl %>%
  group_by(year) %>%
  summarise(hypertension_prev = mean(obsprev, na.rm = T))
  
national_post_covid_prev <- QOF_prev_20_22 %>%
  group_by(year) %>%
  summarise(hypertension_prev = mean(obsprev), 
            predicted_prev = mean(predicted_prev))

ggplot() +
  geom_line(data = national_prev, 
            aes(x = year, y = hypertension_prev), lwd = 3, linetype = 1, col = "#00a3c7") + 
  geom_line(data = national_post_covid_prev, aes(x = year, y = predicted_prev), linetype = 2, 
            lwd = 3, col = "#e93f6f") +
  labs(x = "Year", y = "National Hypertension Prevalence (%)") +
  scale_x_continuous(breaks = 0:7, 
                     labels = c("2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22")) +
  scale_y_continuous(breaks = round(seq(min(national_prev$hypertension_prev), 
                                        max(national_post_covid_prev$predicted_prev), by = 0.2),1)) +
  theme(panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 20))

# 2021 difference
hyp_diff <- tm_shape(prev_diff_shp) + 
  tm_polygons(col = 'prev_diff_21', border.alpha = 0.5, title = "A) iii) Difference from Predicted Hypertension Prev.", 
              palette = c("#e93f6f", "#f595a3", "#fcdadb", "#d4dc5c", "#ffd8a1"),
              breaks = c(-Inf, -0.5, -0.25, -0.05, 0.05, Inf), 
              labels = c("More than -0.5%", "-0.25 to -0.5%", "0 to -0.25%", "No Change", 
                         "0 to 0.5%")) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(frame = F, fontfamily = "serif", 
            legend.title.fontface = "bold", legend.title.size = 1,
            legend.text.size = 0.6, legend.outside.position = "top", 
            legend.outside = T) 

hypertension_comparison_map <- tmap_arrange(hypertension_prev19, hypertension_prev21_map, hyp_diff)
tmap_save(hypertension_comparison_map, "Comparison of Hypertension Pre and Post Covid.pdf", width = 3840, 
          height = 1920, dpi = 320)


# Map of Missed Diagnoses
tm_shape(prev_diff_shp) + 
  tm_polygons(col = "missed_diagnoses", title = "Missed Diagnoses due to Covid", 
              palette = c("#d4dc5c", "#c9e6ee", "#95d1e3", "#5ebfd8", "#00b0cf", "#00a3c7"), 
              breaks = c(-Inf, 1, 500, 1000, 1500, 2500, Inf), 
              labels = c("No Missed", "0 to 500", "500 to 1,000", "1,000 to 1,500", 
                         "1,500 to 2,500", "2,500+")) + 
  tm_layout(frame = F) + 
  tm_compass(position = c("right", "top")) + 
  tm_scale_bar(position = c("right", "bottom"))

# merge to regional data 
regional_missed <- inner_join(QOF_prev_se, ics_region)

# Find Regional values 
regional_totals <- regional_missed %>%
  group_by(NHSER22NM) %>%
  summarise(missed_diagnoses = sum(missed)*-1, 
            pred_prev_lb = sum(missed_lb)*-1, 
            missed_diagnoses_ub = sum(missed_ub)*-1, 
            region_pop = sum(population), 
            hypertension_prev = round(sum((population/region_pop)*obsprev),2),
            pred_prev_lb = round(sum((population/region_pop)*lwr),2), 
            pred_prev_ub = round(sum((population/region_pop)*upr),2)) %>%
  st_drop_geometry()

################################# Exporting Data ###############################
write_csv(missed_all, "Missed Hypertension Cases by ICS.csv")
write_csv(regional_totals, "Missed Hypertension Cases by Region.csv")
write_csv(ics_missed_diagnoses, "Missed Diagnoses of Hypertension Ranked.csv")
