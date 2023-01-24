setwd("C:/Users/SIMC/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - 03 Secondary prevention/Analysis/Dashboard Data")

library(tidyverse)

################################# Load Data ####################################
# Hypertension

# Treatment

# Undiagnosed Hypertension

# Missed Hypertension 

#### Preparing Data for Ed ####
lsoa_undiagnosed_imd <- merge(abs_undiagnosed_lsoa_region, LSOA_imd_cl, by.x = 'lsoa_code', by.y = 'lsoa_code_2011')

lsoa_hypertension_data <- merge(lsoa_age_adj, lsoa_undiagnosed_imd, by = c('lsoa_code', 'lsoa_pop')) %>%
  select(-c(obs_u79_prev:u79_obs_over_exp, age_std_u79_prev, lsoa_name_2011:local_authority_district_name_2019, 
            income_score_rate:barriers_to_housing_and_services_score)) %>%
  rename(LSOA11CD = lsoa_code, 
         observed_hypertension_prevalence = obs_hyper_prev, 
         age_std_hypertension_prevalence = age_std_prev,
         undiagnosed_rate = undiagnosed_prev, 
         number_undiagnosed = undiagnosed, 
         SICBL22NM = CCG21NM, 
         SICBL22CDH = CCG21CDH, 
         SICBL22CD = CCG21CD) %>%
  mutate(excess_stroke = floor(number_undiagnosed/67), 
         excess_mi = floor(number_undiagnosed/118))

lsoa_treatment_data <- merge(lsoa_uncontrolled_prev, lsoa_region, by.x = "lsoa_code", by.y = "LSOA11CD") %>%
  select(-c(untreated_perc_under80:needed_for_80_treat, FID)) %>%
  rename(LSOA11CD = lsoa_code, 
         SICBL22NM = CCG21NM, 
         SICBL22CDH = CCG21CDH, 
         SICBL22CD = CCG21CD, 
         observed_hypertension_prevalence = hypertension_prev, 
         over80_treatment_rate = treated_over80_percent_lsoa, 
         under80_treatment_rate = treated_under80_percent_lsoa, 
         overall_treatment_rate = all_treated_percent_lsoa)

lsoa_hypertension_data <- lsoa_hypertension_data[,c(1,10,3,4,2,8,9,15,16,13,14,5:7,11,12)]
lsoa_treatment_data <- lsoa_treatment_data[,c(1,11,2,3,5,4,6:10,12:16)]

ics_treatment_data <- lsoa_treatment_data %>%
  group_by(SICBL22CD, SICBL22CDH, SICBL22NM) %>%
  summarise(sub_icb_pop = sum(lsoa_pop),
            treated_80percent = sum(untreated_80percent),
            tot_untreated = sum(tot_untreated),
            observed_hypertension_prev = mean(observed_hypertension_prevalence),
            under80_treatment_rate = mean(under80_treatment_rate), 
            over80_treatment_rate = mean(over80_treatment_rate), 
            overall_treatment_rate = mean(overall_treatment_rate),
            NHSER21NM = paste(unique(NHSER21NM)), 
            NHSER21CD = paste(unique(NHSER21CD))) %>%
  mutate(preventable_strokes = floor(tot_untreated/67), 
         preventable_mi = floor(tot_untreated/118), 
         strokes_80 = floor(treated_80percent/67), 
         mi_80 = floor(treated_80percent/118), 
         total_costs_excess_stroke_per_yr = (preventable_strokes)*(50850.72/5), 
         total_costs_excess_mi_per_yr = (preventable_mi)*2052.12,
         total_costs_strokes80 = (strokes_80)*(50850.72/5), 
         total_costs_mi80 = (mi_80)*2052.12,
         hypertension_costs = (tot_untreated)*161.67, 
         hypertension_costs80 = (treated_80percent)*161.67,
         costs_saved = total_costs_excess_stroke_per_yr+total_costs_excess_mi_per_yr-hypertension_costs, 
         costs_saved80 = total_costs_strokes80 + total_costs_mi80 - hypertension_costs80)

ics_treatment_data <- ics_treatment_data[,c(1,3,2,4:9,12,13,17,10,11)]

sub_icb_data <- left_join(sub_icb_abs, undiagnosed_excess)
ccg_data <- left_join(ccg_agg, missed_21)

ics_imd <- ics_imd %>%
  rename(SICBL22CDH = CCG21CDH, 
         SICBL22CD = CCG21CD)

ics_hypertension_data <- merge(ccg_data, sub_icb_data, by.x = c("CCG21CDH", "CCG21CD"), 
                               by.y = c("sub_icb_loc_ods_code", "sub_icb_loc_ons_code")) %>%
  select(-c(CCG21NM:obs_over_exp_22, avg_u79_achievement_21_22:ccg_code, prev_diff_21, 
            excess_stroke_LB, excess_stroke_UB, excess_mi_LB, excses_mi_UB, excess_stroke80:excess_mi50, 
            sub_icb_pop.y, lsoa_pop_21_22, predicted_prev)) %>%
  rename(SICBL22CDH = CCG21CDH, 
         SICBL22CD = CCG21CD, 
         SICBL22NM = sub_icb_loc_name, 
         observed_hypertension_prevalence = obs_prev_21, 
         age_std_hypertension_prevalence = age_std_prev_21_22,
         sub_icb_population = sub_icb_pop.x,
         missed_diagnoses = missed_21, 
         number_undiagnosed = undiagnosed_hypertension,
         undiagnosed_rate_per_100000 = undiagnosed_per_100000) %>%
  mutate(missed_diagnoses = missed_diagnoses*(-1),
         undiagnosed_rate = round(undiagnosed_rate_per_100000/1000, 2),
         missed_strokes = floor(missed_diagnoses/67), 
         missed_mi = floor(missed_diagnoses/118)) %>%
  inner_join(ics_imd) %>%
  select(-CCG21NM)

ics_hypertension_data <- ics_hypertension_data[,c(2,7,1,4,3,5,8,16,9,10,15,6,17:20)]

msoa_hypertension_data <- merge(lsoa_hypertension_data, lsoa_msoa, by.x = 'LSOA11CD', by.y = 'lsoa11cd') %>%
  group_by(msoa11cd, msoa11nm) %>%
  summarise(observed_hypertension_prevalence = mean(observed_hypertension_prevalence), 
            age_std_hypertension_prevalence = mean(age_std_hypertension_prevalence), 
            msoa_pop = sum(lsoa_pop), 
            undiagnosed_rate = mean(undiagnosed_rate), 
            number_undiagnosed = sum(number_undiagnosed), 
            excess_stroke = sum(excess_stroke), 
            excess_mi = sum(excess_mi),
            imd_score = mean(imd_score),
            SICBL22CD = paste(unique(SICBL22CD)), 
            SICBL22NM = paste(unique(SICBL22NM))) %>%
  mutate(imd_decile = ntile(desc(imd_score),10))

msoa_hypertension_data <- msoa_hypertension_data[,c(1:10,13,11,12)]

msoa_treatment_data <- merge(lsoa_treatment_data, lsoa_msoa, by.x = 'LSOA11CD', by.y = 'lsoa11cd') %>%
  group_by(msoa11cd, msoa11nm) %>%
  summarise(observed_hypertension_prevalence = mean(observed_hypertension_prevalence), 
            under80_treatment_rate = mean(under80_treatment_rate), 
            over80_treatment_rate = mean(over80_treatment_rate),
            overall_treatment_rate = mean(overall_treatment_rate),
            msoa_pop = sum(lsoa_pop), 
            preventable_strokes = sum(preventable_strokes), 
            preventable_mi = sum(preventable_mi),
            SICBL22CD = paste(unique(SICBL22CD)), 
            SICBL22NM = paste(unique(SICBL22NM)))

# Exporting as CSV
write_csv(lsoa_hypertension_data, "lsoa_hypertension_data.csv")
write_csv(lsoa_treatment_data, "lsoa_treatment_data.csv") 

write_csv(msoa_hypertension_data, "msoa_hypertension_data.csv")
write_csv(msoa_treatment_data, "msoa_treatment_data.csv")

write_csv(ics_hypertension_data, "ics_hypertension_data.csv")
write_csv(ics_treatment_data, "ics_treatment_data.csv")
