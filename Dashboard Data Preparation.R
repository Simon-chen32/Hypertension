setwd("C:/Users/SIMC/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - 03 CVD Secondary prevention/Analysis")

library(tidyverse)

################################# Load Data ####################################
# Hypertension
lsoa_hypertension_data <- read_csv("Age-Adjusted LSOA Hypertension Data.csv") %>%
  mutate(hypertension_prevalence_quintile = ntile(age_std_prev, 5), 
         hypertension_cases_quintile = ntile(hypertension_cases, 5))

ics_hypertension_data <- read_csv("ICS Hypertension Data.csv") %>%
  select(-c(exp_hyp_21_22,obs_over_exp_22, avg_u79_achievement_21_22:avg_o80_intervention_21_22)) %>%
  rename(observed_hypertension_prevalence = avg_prevalence_21_22, 
         age_standardised_prevalence = age_std_prev_21_22, 
         ics_pop = population) %>%
  mutate(hypertension_cases = round(ics_pop*observed_hypertension_prevalence/100), 
         hypertension_prevalence_quintile = ntile(age_standardised_prevalence, 5), 
         hypertension_cases_quintile = ntile(hypertension_cases, 5))

# Treatment
lsoa_treatment_data <- read_csv("LSOA Treatment Indicators Ranked.csv") %>%
  select(-c(untreated_perc_under80, untreated_perc_over80, untreated_100perc:untreated_pop)) %>%
  rename(LSOA11CD = lsoa_code, 
         observed_hypertension_prevalence = hypertension_prev, 
         under80_treatment_rate = treated_under80_percent_lsoa, 
         over80_treatment_rate = treated_over80_percent_lsoa, 
         lsoa_treatment_rate = all_treated_percent_lsoa, 
         treated_population = treated_pop, 
         needed_for_80percent_treatment = untreated_80perc) %>%
  mutate(hypertension_treatment_quintile = ntile(lsoa_treatment_rate, 5), 
         population_treated_quintile = ntile(treated_population, 5))

ics_treatment_data <- read_csv("ICS Treatment Indicators Ranked.csv") %>%
  select(-c(untreated_perc_under80, untreated_perc_over80, untreated_100perc, untreated_pop80, untreated_pop)) %>%
  rename(SICBL22CD = sub_icb_loc_ons_code, 
         SICBL22NM = sub_icb_loc_name, 
         SICBL22CDH = sub_icb_loc_ods_code,
         under80_treatment_rate = treated_under80_percent, 
         over80_treatment_rate = treated_over80_percent, 
         ics_treatment_rate = all_treated_percent, 
         treated_population = treated_pop, 
         needed_for_80percent_treatment = untreated_80perc) %>%
  mutate(hypertension_treatment_quintile = ntile(ics_treatment_rate, 5), 
         population_treated_quintile = ntile(treated_population, 5))

# Undiagnosed Hypertension
lsoa_undiagnosed_data <- read_csv("Undiagnosed Hypertension by LSOA Ranked.csv") %>%
  rename(LSOA11CD = lsoa_code, 
         observed_hypertension_prevalence = hypertension_prev, 
         undiagnosed_rate = undiagnosed_prev, 
         number_undiagnosed = undiagnosed, 
         inter_ics_undiagnosed_hypertension_rank = sub_icb_hypertension_prev_rank) %>%
  mutate(undiagnosed_rate_quintile = ntile(undiagnosed_rate, 5), 
         undiagnosed_population_quintile = ntile(number_undiagnosed, 5))

lsoa_total_burden <- lsoa_undiagnosed_data %>%
  mutate(total_hypertension_pop = hypertension_cases+number_undiagnosed, 
         burden_rate = round(total_hypertension_pop/lsoa_pop*100, digits = 2), 
         hypertension_prevalence_quintile = ntile(burden_rate, 5),
         hypertension_cases_quintile = ntile(total_hypertension_pop, 5)) %>%
  group_by(SICBL22CD, SICBL22NM, SICBL22CDH) %>%
  mutate(inter_ics_all_hypertension_rank = rank(burden_rate, ties.method = "min"))


ics_undiagnosed <- read_csv("ICS Undiagnosed Hypertension.csv") %>%
  rename(SICBL22CD = sub_icb_loc_ons_code, 
         SICBL22NM = sub_icb_loc_name, 
         SICBL22CDH = sub_icb_loc_ods_code) %>%
  mutate(undiagnosed_rate_quintile = ntile(undiagnosed_rate, 5), 
         undiagnosed_population_quintile = ntile(undiagnosed_hypertension, 5))

ics_total_burden <- inner_join(ics_undiagnosed, ics_hypertension_data) %>%
  rename(number_undiagnosed = undiagnosed_hypertension) %>%
  mutate(total_hypertension_pop = hypertension_cases + number_undiagnosed, 
         burden_rate = round(total_hypertension_pop/ics_pop*100, digits = 2),
         national_all_hypertension_rank = rank(burden_rate, ties.method = "min"), 
         total_prevalence_quintile = ntile(burden_rate, 5),
         total_cases_quintile = ntile(total_hypertension_pop, 5)) 

# Missed Hypertension 
ics_missed_diagnoses <- read_csv("Missed Diagnoses of Hypertension Ranked.csv")

ics_missed_diagnoses_no_negative <- ics_missed_diagnoses %>%
  rename(missed_rate_rank = missed_diagnoses_rank) %>%
  mutate(missed_diagnoses = case_when(missed_21 > 0 ~ 0, T ~ missed_21*-1), 
         missed_diagnoses_rate = case_when(missed_diagnoses_rate > 0 ~ 0, T ~ missed_diagnoses_rate*-1), 
         missed_prevalence_quintile = ntile(missed_diagnoses_rate, 5), 
         missed_diagnoses_quintile = ntile(missed_diagnoses, 5))

# IMD Data 
lsoa_imd <- read_csv("Cleaned LSOA IMD.csv")

#### Preparing Data for Ed ####
lsoa_hypertension_imd <- merge(lsoa_hypertension_data, lsoa_imd, by.x = "lsoa_code", by.y = "lsoa_code_2011") %>%
  select(-c(obs_u79_prev:exp_u79_prev, over80_pop:u79_obs_over_exp, age_std_u79_prev, LAD22CD, LAD22NM, 
            lsoa_name_2011:local_authority_district_name_2019,income_score_rate:barriers_to_housing_and_services_score)) %>%
  rename(LSOA11CD = lsoa_code,
         observed_hypertension_prevalence = obs_hyper_prev, 
         age_standardised_prevalence = age_std_prev) %>%
  mutate(hypertension_cases = round(lsoa_pop*observed_hypertension_prevalence/100))

lsoa_treatment_imd <- merge(lsoa_treatment_data, lsoa_imd, by.x = "LSOA11CD", by.y = "lsoa_code_2011") 
lm <- lm(lsoa_treatment_rate ~  imd_decile, data = lsoa_treatment_imd)
summary(lm)

lsoa_hypertension_prevalence_data <- lsoa_hypertension_imd[,c(1,6,2,4,3,5,13:15,17,7:12)]
lsoa_undiagnosed_hypertension_data <- lsoa_undiagnosed_data[,c(1,7,3,2,4,5,14:16,8:13)]
lsoa_hypertension_treatment_data <- lsoa_treatment_data[,c(1,10,3,6,2,14:17,11:13,5,4,7:9)]
lsoa_total_burden_data <- lsoa_total_burden[,c(1,7,3,2,18,4,6,5,17,19:21,8:13)]

ics_hypertension_treatment_data <- ics_treatment_data[,c(1:3,5,9,4,11,25:27,18:23,6,7,10)]
ics_hypertension_prevalence_data <- ics_hypertension_data[,c(1:6,8,7,9,10)]
ics_missed_hypertension_data <- ics_missed_diagnoses_no_negative[,c(1:3,7,4,8:11)]
ics_undiagnosed_hypertension_data <- ics_undiagnosed[,c(2,3,1,6,5,4,7:9)]
ics_total_burden_data <- ics_total_burden[,c(2,3,1,10,6,18,5,14,4,17,19:21)]

setwd("C:/Users/SIMC/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - 03 CVD Secondary prevention/Analysis/Dashboard Data")

# Exporting as CSV
write_csv(lsoa_hypertension_prevalence_data, "lsoa_hypertension_prevalence_data.csv")
write_csv(lsoa_hypertension_treatment_data, "lsoa_hypertension_treatment_data.csv") 
write_csv(lsoa_undiagnosed_hypertension_data, "lsoa_undiagnosed_hypertension_data.csv")
write_csv(lsoa_total_burden_data, "lsoa_total_burden_of_hypertension.csv")

write_csv(ics_hypertension_prevalence_data, "ics_hypertension_prevalence_data.csv")
write_csv(ics_hypertension_treatment_data, "ics_hypertension_treatment_data.csv")
write_csv(ics_missed_hypertension_data, "ics_missed_hypertension_diagnoses_data.csv")
write_csv(ics_undiagnosed_hypertension_data, "ics_undiagnosed_hypertension_data.csv")
write_csv(ics_total_burden_data, "ics_total_burden_of_hypertension.csv")
