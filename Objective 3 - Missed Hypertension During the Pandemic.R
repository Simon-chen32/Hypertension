# set working directory 
setwd("C:/Users/SIMC/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - 03 Secondary prevention/Analysis")

#### Installing Packages ####
library(tidyverse)
library(tmap)
library(sf)
library(janitor)
library(stringr)
library(ggplot2)

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

QOF_prev_21_22 <- QOF_prev_21_22 %>%
  mutate(prev_diff = obsprev-predicted_prev)

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
  mutate(undiagnosed = round(prev_diff*sub_icb_pop/100, digits = 0), 
         estimated = round(predicted_prev*(sub_icb_pop/100))) 

# Finding Absolute Values in Differences
missed_21 <- QOF_prev_pop %>%
  filter(year == 6) %>%
  select(., -c(year, covid)) %>%
  rename(obs_prev_21 = obsprev, 
         prev_diff_21 = prev_diff,
         missed_21 = undiagnosed)

top_decile_missed_ics <- missed_21 %>%
  mutate(decile = ntile(missed_21, 10)) %>%
  filter(decile == 1)

top_decile_missed_ics_shp <- merge(Eng_CCG, top_decile_missed_ics, by = c('CCG21CD', 'CCG21NM'))

tm_shape(Eng_CCG) + 
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
         undiagnosed_22 = undiagnosed)

missed_all <- merge(missed_21, missed_22, by = c("ccg_code", "CCG21CD", "CCG21NM", "sub_icb_pop"))

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

ggplot(abs_hyper_region, aes(x = NHSER21NM, y = missed_21, fill = CCG21NM)) + 
  geom_col() +
  labs(x = "Region", y = "Missed Diagnoses", title = "Missed Diagnoses by Region") +
  #  scale_fill_brewer() + 
  stat_summary(fun = sum, aes(label = format(..y.., digits = 5), group = NHSER21NM), geom = "text") +
  theme(legend.position = "none")

# Find Regional values 
regional_totals <- abs_hyper_region %>%
  group_by(NHSER21NM) %>%
  summarise(missed_diagnoses_total = round(sum(missed_21), 0), 
            region_pop = sum(sub_icb_pop))
