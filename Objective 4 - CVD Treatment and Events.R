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

# set working directory 
setwd("C:/Users/SIMC/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - 03 Secondary prevention/Analysis")

#### Objective 4 ####
# Loading in Shapefile
Eng_ICS <- st_read("~/Shapefiles/Sub-ICB Shapefiles/SICBL_JUL_2022_EN_BUC.shp")
# Rename the three ICS with "Isle" so there are no difficulties with merging
Eng_ICS$SICBL22NM <- gsub("NHS Cornwall and The Isles Of Scilly ICB - 11N", "NHS Cornwall and the Isles of Scilly ICB - 11N", Eng_ICS$SICBL22NM)
Eng_ICS$SICBL22NM <- gsub("NHS Hampshire and Isle Of Wight ICB - 10R", "NHS Hampshire and Isle of Wight ICB - 10R", Eng_ICS$SICBL22NM)
Eng_ICS$SICBL22NM <- gsub("NHS Hampshire and Isle Of Wight ICB - D9Y0V", "NHS Hampshire and Isle of Wight ICB - D9Y0V", Eng_ICS$SICBL22NM)

ENG_LSOA11 <- st_read("~/Shapefiles/LSOA (2011) Shapefiles/infuse_lsoa_lyr_2011.shp") %>% 
  filter(str_detect(geo_code, '^E'))


# Looking into Number of Uncontrolled cases of Hypertension 
# Load in LSOA level data from GP's Analysis
# Start by Calculating the weighted achievement and intervention rates at GP level 
lsoa_treatment_achievement <- read_csv("~/Hypertension/For Ed/GPs Data/treatment_achievement_lsoa.csv") %>%
  select(-c(u80deciles:alldeciles))

# LSOA to GP Analysis Data 
gp_to_lsoa_hypertension_prev <- read_csv("GP to LSOA Hypertension Data.csv")

# Lookup File
lsoa_ics <- read_csv("~/Lookup Files/LSOA_(2011)_to_Sub_ICB_Locations_to_Integrated_Care_Boards_(July_2022)_Lookup_in_England.csv") %>%
  select(-ObjectId)
lsoa_region <- read_csv("LSOA to Region Lookup File.csv")

# Extrapolating these to LSOA level using the same methods as before 
lsoa_treatment <- gp_to_lsoa_hypertension_prev %>%
  group_by(lsoa_code) %>%
  summarise(lsoa_pop = mean(lsoa_pop), 
            hypertension_prev = sum(prevalence_percent_21_22*gp_coverage)) %>%
  inner_join(lsoa_treatment_achievement) %>%
  mutate(untreated_perc_under80 = 100 - treated_under80_percent_lsoa, 
         untreated_perc_over80 = 100 - treated_over80_percent_lsoa, 
         untreated_80perc = 80 - all_treated_percent_lsoa,
         untreated_100perc = 100 - all_treated_percent_lsoa, 
         untreated_pop80 = round(untreated_80perc*(lsoa_pop*(hypertension_prev/100))/100),
         untreated_pop = round(untreated_100perc*(lsoa_pop*hypertension_prev/100)/100), 
         preventable_strokes = floor(untreated_pop80/67), 
         preventable_mi = floor(untreated_pop80/118))

lsoa_untreated_quintile <- lsoa_treatment %>%
  mutate(uncontrolled_quintile = ntile(untreated_80perc, 5)) %>%
  mutate(across(uncontrolled_quintile, as_factor))%>%
  group_by(uncontrolled_quintile) %>%
  summarise(population = sum(lsoa_pop),
            tot_untreated = sum(untreated_pop80)) %>%
  mutate(preventable_strokes = floor(tot_untreated/67), 
         preventable_mi = floor(tot_untreated/118), 
         total_costs_stroke_per_yr = (preventable_strokes)*(50850.72/5), 
         total_costs_mi_per_yr = (preventable_mi)*2052.12,
         hypertension_costs = (tot_untreated)*161.67, 
         costs_saved = round(total_costs_stroke_per_yr+total_costs_mi_per_yr-hypertension_costs, 2))

######################## Undiagnosed Hypertension ##########################
# Loading Undiagnosed Hypertension Data
lsoa_undiagnosed_hypertension <- read_csv("LSOA Undiagnosed Hypertension.csv")
ics_undiagnosed_hypertension <- read_csv("ICS Undiagnosed Hypertension.csv")
regional_undiagnosed <- read_csv("Undiagnosed Hypertension by Region.csv")

# Finding the Impact on Undiagnosed Hypertension 
cvd_events_from_undiagnosed <- ics_undiagnosed_hypertension %>%
  mutate(excess_stroke = round(undiagnosed_hypertension/67),
         excess_stroke_LB = round(undiagnosed_hypertension/84),
         excess_stroke_UB = round(undiagnosed_hypertension/57), 
         excess_mi = round(undiagnosed_hypertension/118),
         excess_mi_LB = round(undiagnosed_hypertension/171), 
         excses_mi_UB = round(undiagnosed_hypertension/94), 
         undiagnosed_per_100000 = round(undiagnosed_hypertension/(population/100000), digits = 2), 
         excess_stroke80 = round(undiagnosed_hypertension*0.8/67), 
         excess_stroke80_LB = round(undiagnosed_hypertension*0.8/84), 
         excess_stroke80_UB = round(undiagnosed_hypertension*0.8/57), 
         excess_mi80 = round(undiagnosed_hypertension*0.8/118), 
         excess_mi80_LB = round(undiagnosed_hypertension*0.8/171), 
         excess_mi80_UB = round(undiagnosed_hypertension*0.8/94), 
         excess_stroke50 = round(undiagnosed_hypertension*0.5/67), 
         excess_mi50 = round(undiagnosed_hypertension*0.5/118), 
         total_costs_stroke_per_yr = (excess_stroke)*(50850.72/5), 
         total_costs_mi_per_yr = (excess_mi)*2052.12,
         hypertension_costs = (undiagnosed_hypertension)*161.67, 
         costs_saved = total_costs_stroke_per_yr+total_costs_mi_per_yr-hypertension_costs) %>%
  drop_na()

regional_undiagnosed_cvd_event <- regional_undiagnosed %>%
  mutate(excess_stroke = floor(tot_undiagnosed/67), 
         excess_stroke_LB = floor(tot_undiagnosed/84), 
         excess_stroke_UB = floor(tot_undiagnosed/57), 
         excess_mi = floor(tot_undiagnosed/118), 
         excess_mi_LB = floor(tot_undiagnosed/171), 
         excess_mi_UB = floor(tot_undiagnosed/94))

regional_undiagnosed_cvd_event$NHSER22NM <- factor(regional_undiagnosed_cvd_event$NHSER22NM, 
                                                levels = rev(sort(regional_undiagnosed_cvd_event$NHSER22NM)))

# Plotting Regional Distribution of Undiagnosed Hypertension
undiagnosed_cvd_events <- regional_undiagnosed_cvd_event %>%
  pivot_longer(cols = c("excess_stroke", "excess_mi"), 
               names_to = "CVD",
               values_to = "excess", 
               names_prefix = "excess_") %>%
  select(-c(excess_stroke_LB:excess_mi_UB))

undiagnosed_LB <- regional_undiagnosed_cvd_event %>%
  pivot_longer(cols = c("excess_stroke_LB", "excess_mi_LB"), 
               names_to = "CVD_LB",
               names_prefix = "excess_",
               values_to = "LB") %>%
  select(-c(excess_stroke:excess_mi_UB))

undiagnosed_UB <- regional_undiagnosed_cvd_event %>%
  pivot_longer(cols = c("excess_stroke_UB", "excess_mi_UB"), 
               names_to = "CVD_UB",
               names_prefix = "excess_",
               values_to = "UB") %>%
  select(-c(excess_stroke:excess_mi_LB))

regional_undiagnosed_cvd_event_long <- cbind(undiagnosed_cvd_events, undiagnosed_LB, undiagnosed_UB) %>%
  select(-c(5,9:15,17:23)) 

ggplot(regional_undiagnosed_cvd_event_long, aes(x = NHSER22NM, y = excess, 
                                    fill = CVD)) +
  scale_fill_manual('CVD Type', values = c("#e93f6f", "#00a3c7"), 
                    labels = c("MI", "Stroke")) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = LB, ymax = UB), width = .2, position = position_dodge(1)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() + 
  labs(title = "CVD Events by Region", x = "Region", 
       y = "Number of Events") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black")) 
###

# Decile Analysis 
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

undiagnosed_hypertension_decile <- lsoa_undiagnosed_hypertension %>%
  dplyr::mutate(ntile = ntile(undiagnosed, 5)) %>%
  mutate(across(ntile, as_factor)) %>%
  dplyr::group_by(ntile) %>%
  summarise(undiagnosed = sum(undiagnosed), 
            population = sum(lsoa_pop)) %>%
  mutate(excess_stroke = floor(undiagnosed/67),
         stroke_LB = floor(undiagnosed/84), 
         stroke_UB = floor(undiagnosed/57), 
         excess_mi = floor(undiagnosed/118), 
         mi_LB = floor(undiagnosed/171), 
         mi_UB = floor(undiagnosed/94),
         total_costs_stroke_per_yr = round((excess_stroke)*(50850.72/5)), 
         total_costs_mi_per_yr = round((excess_mi)*2052.12),
         hypertension_costs = round((undiagnosed)*161.67), 
         costs_saved = total_costs_stroke_per_yr + total_costs_mi_per_yr-hypertension_costs) 

# Plotting of Results 
ggplot(decile_cumulative, aes(x = obs_decile, fill = obs_decile)) +
  scale_fill_manual(values = c("#fcdadb", "#f5c2c3", "#efaaaa", "#e89192", "#e2797a", "#db6161", "#d54949", "#ce3031", "#c81818", "#c10000")) + 
  geom_col(aes(y = hypertension_pop/1000)) +
  geom_point(aes(y = undiagnosed/1000), size = 3) +
  geom_path(aes(y = undiagnosed/1000, group = 1)) +
  labs(title = "Hypertension Population Distribution", x = "Hypertension Prevalence Decile", 
       y = "Hypertension Population (in 1000s)") +
  geom_text(aes(1, 750, label = 'Undiagnosed Population', vjust = 3, hjust = -0.25), size = 6) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Undiagnosed Population (in 1000s)"), 
                     expand = c(0,0)) +
  theme(axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.position = "none") 

###################### Missed Diagnoses due to COVID ##########################
# Loading in Missed due to COVID Data
ics_missed_hypertension <- read_csv("Missed Hypertension Cases by ICS.csv")
regional_missed_hypertension <- read_csv("Missed Hypertension Cases by Region.csv")

# Finding the impact of missed diagnosis due to COVID
missed_hypertension_cvd_events <- ics_missed_hypertension %>%
  mutate(missed_diagnoses = case_when(missed_21<0 ~ missed_21*-1, T~0),
         excess_stroke = ceiling(missed_diagnoses/67),
         excess_stroke_LB = ceiling(missed_diagnoses/84),
         excess_stroke_UB = ceiling(missed_diagnoses/57), 
         excess_mi = ceiling(missed_diagnoses/118),
         excess_mi_LB = ceiling(missed_diagnoses/171), 
         excess_mi_UB = ceiling(missed_diagnoses/94),
         total_costs_stroke_per_yr = (excess_stroke)*(50850.72/5), 
         total_costs_mi_per_yr = (excess_mi)*2052.12,
         hypertension_costs = (missed_diagnoses)*161.67, 
         costs_saved = total_costs_stroke_per_yr+total_costs_mi_per_yr-hypertension_costs,
         missed_diagnoses_per_100000 = round(missed_diagnoses/(population/100000), digits = 2)) %>%
  #  select(-c(obs_prev_21:age_std_prev_22)) %>%
  select(-c(missed_21, obs_prev_22:estimated_hypertension_pop_22))

# Plotting Regional Bar Chart
regional_missed <- regional_missed_hypertension %>%
  mutate(missed_diagnoses_total = missed_diagnoses_total*-1, 
         stroke = floor(missed_diagnoses_total/67),
         stroke_LB = floor(missed_diagnoses_total/84),
         stroke_UB = floor(missed_diagnoses_total/57), 
         mi = floor(missed_diagnoses_total/118),
         mi_LB = floor(missed_diagnoses_total/171), 
         mi_UB = floor(missed_diagnoses_total/94), 
         missed_diagnoses_per_100000 = round(missed_diagnoses_total/(region_pop/100000), digits = 2)) 

regional_missed$NHSER22NM <- factor(regional_missed$NHSER22NM, 
                                           levels = rev(sort(regional_missed$NHSER22NM)))

regional_missed_long <- regional_missed %>%
  pivot_longer(cols = c("stroke", "mi"), 
               names_to = "CVD",
               values_to = "excess") %>%
  select(-c(stroke_LB:mi_UB))

LB <- regional_missed %>%
  pivot_longer(cols = c("stroke_LB", "mi_LB"), 
               names_to = "CVD_LB",
               values_to = "LB") %>%
  select(-c(stroke:mi_UB))

UB <- regional_missed %>%
  pivot_longer(cols = c("stroke_UB", "mi_UB"), 
               names_to = "CVD_UB", 
               values_to = "UB") %>%
  select(-c(stroke:mi_LB))

region_missed_hypertension_long <- cbind(regional_missed_long, LB, UB) %>%
  select(-c(7:10, 13:16))

ggplot(region_missed_hypertension_long, aes(x = NHSER22NM, y = excess, 
                               fill = CVD)) +
  scale_fill_manual('CVD Type', values = c("#e93f6f", "#00a3c7"), 
                    labels = c("MI", "Stroke")) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = LB, ymax = UB), width = .2, position = position_dodge(1)) +
  coord_flip() + 
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "CVD Events by Region", x = "Region", 
       y = "Number of Events") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"))
###

# Decile Analysis
missed_cvd_events_quintile <- missed_hypertension_cvd_events %>%
  mutate(quintile = ntile((missed_diagnoses), 5)) %>%
  mutate(across(quintile, as_factor)) %>%
  group_by(quintile) %>%
  summarise(missed_diagnoses = sum(missed_diagnoses),
            missed_diagnoses_per_100000 = sum(missed_diagnoses_per_100000),
            population = sum(population), 
            excess_stroke = sum(excess_stroke), 
            excess_mi = sum(excess_mi),
            stroke_LB = sum(excess_stroke_LB), 
            stroke_UB = sum(excess_stroke_UB),
            mi_LB = sum(excess_mi_LB), 
            mi_UB = sum(excess_mi_UB))

######################### Deprivation Analysis ###############################
# Load in IMD Data
lsoa_imd <- read_csv("Cleaned LSOA IMD.csv")

# LSOA Analysis
lsoa_untreated_imd <- merge(lsoa_treatment, lsoa_imd, by.x = "lsoa_code", by.y = "lsoa_code_2011") %>%
  mutate(across(imd_decile, as_factor)) %>%
  group_by(imd_decile) %>%
  summarise(population = sum(lsoa_pop), 
            achievement_rate = sum((lsoa_pop/population)*all_treated_percent_lsoa), 
            tot_untreated = sum(untreated_pop80)) %>%
  mutate(preventable_strokes = floor(tot_untreated/67), 
         preventable_mi = floor(tot_untreated/118),
         total_costs_stroke_per_yr = (preventable_strokes)*(50850.72/5), 
         total_costs_mi_per_yr = (preventable_mi)*2052.12,
         hypertension_costs = (tot_untreated)*161.67, 
         costs_saved = round(total_costs_stroke_per_yr+total_costs_mi_per_yr-hypertension_costs, 2))

lsoa_undiagnosed_imd <- merge(lsoa_undiagnosed_hypertension, lsoa_imd, by.x = 'lsoa_code', by.y = 'lsoa_code_2011') %>%
  mutate(imd_decile = ntile(imd_score, 10)) %>%
  mutate(across(imd_decile, as_factor)) %>%
  group_by(imd_decile) %>%
  summarise(undiagnosed = sum(undiagnosed), 
            population = sum(lsoa_pop),
            hypertension_pop = sum(hypertension_cases)) %>%
  mutate(excess_stroke = floor(undiagnosed/67),
         excess_mi = floor(undiagnosed/118), 
         undiagnosed_per_100000 = round(undiagnosed/(population/100000), digits = 2))

# Calculating Weighted IMD by ICS
ics_imd <- merge(lsoa_imd, lsoa_ics, by.x = "lsoa_code_2011", by.y = "LSOA11CD") %>%
  group_by(SICBL22CD, SICBL22NM, SICBL22CDH) %>%
  summarise(imd_score = mean(imd_score)) %>%
  ungroup() %>%
  mutate(imd_decile = ntile(desc(imd_score), 10)) %>%
  mutate(across(imd_decile, as_factor))
# Exporting
write_csv(ics_imd, "ics_imd.csv")

missed_cvd_by_imd <- merge(ics_imd, missed_hypertension_cvd_events, by = c("SICBL22CD", "SICBL22NM", "SICBL22CDH"))

missed_cvd_by_imd_decile <- missed_cvd_by_imd %>%
  group_by(imd_decile) %>%
  summarise(missed_diagnoses = sum(missed_diagnoses), 
            costs_saved = sum(costs_saved))

################################### Plotting ##################################
# Undiagnosed Hypertension
ggplot(lsoa_undiagnosed_imd, aes(x = imd_decile, fill = imd_decile)) +
  scale_fill_manual(values = c("#fcdadb", "#f5c2c3", "#efaaaa", "#e89192", "#e2797a", "#db6161", "#d54949", "#ce3031", "#c81818", "#c10000")) + 
  geom_col(aes(y = hypertension_pop/1000)) +
  geom_point(aes(y = undiagnosed/1000), size = 3) +
  geom_path(aes(y = undiagnosed/1000, group = 1)) +
  labs(title = "Hypertension Population Distribution", x = "IMD Decile (10 is Most Deprived)", 
       y = "Hypertension Population (in 1000s)") +
  geom_text(aes(4.5, 725, label = 'Undiagnosed Population', vjust = -1.5, hjust = 0.25), size = 5) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Undiagnosed Population (in 1000s)"), 
                     expand = c(0,0)) +
  theme(axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.position = "none") 

# IMD 
ggplot(lsoa_untreated_imd, aes(x = imd_decile, fill = imd_decile)) +
  scale_fill_manual(values = c("#fcdadb", "#f5c2c3", "#efaaaa", "#e89192", "#e2797a", "#db6161", "#d54949", "#ce3031", "#c81818", "#c10000")) + 
  geom_col(aes(y = tot_untreated/1000)) +
  labs(x = "IMD Decile (10 is Most Deprived)", 
       y = "Untreated Population (in 1000s)") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 20),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.position = "none") 

# Missed
ggplot(missed_cvd_by_imd_decile, aes(x = imd_decile, fill = imd_decile)) +
  scale_fill_manual(values = c("#fcdadb", "#f5c2c3", "#efaaaa", "#e89192", "#e2797a", "#db6161", "#d54949", "#ce3031", "#c81818", "#c10000")) + 
  geom_col(aes(y = missed_diagnoses)) +
  labs(x = "IMD Decile (10 is Most Deprived)", 
       y = "Missed Hypertension Diagnoses") +
  theme(legend.position =  "none", 
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24))

##################### Pareto Charts - CVD Events by ICS ########################
# Stroke
undiagnosed_strokes <- cvd_events_from_undiagnosed[order(cvd_events_from_undiagnosed$undiagnosed_hypertension, 
                                                      decreasing = T), ]

undiagnosed_strokes$sub_icb_loc_name <- factor(undiagnosed_strokes$sub_icb_loc_name, 
                                                     levels = undiagnosed_strokes$sub_icb_loc_name)

undiagnosed_strokes$cumulative <- cumsum(undiagnosed_strokes$excess_stroke) 

undiagnosed_strokes$cumulative <- 100 * undiagnosed_strokes$cumulative/tail(undiagnosed_strokes$cumulative, n = 1)

scaleRight <- tail(undiagnosed_strokes$cumulative, n = 1)/head(undiagnosed_strokes$excess_stroke, n = 1)

ggplot(undiagnosed_strokes, aes(x = sub_icb_loc_name)) +
  geom_bar(aes(y = excess_stroke), fill = "#00a3c7", stat = "identity") + 
  geom_path(aes(y = cumulative/scaleRight, group = 1), color = "black", lwd = 2.5) + 
  geom_point(aes(y = cumulative/scaleRight, group = 1), color = 'black', size = 4) + 
  scale_y_continuous(expand = c(0,0), sec.axis = sec_axis(~.*scaleRight, name = "Cumulative (%)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = 'Preventable Strokes by ICS', y = 'Prevented Cases of Stroke') + 
  theme(panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 18))

# MI
undiagnosed_mi <- cvd_events_from_undiagnosed[order(cvd_events_from_undiagnosed$undiagnosed_hypertension, 
                                                  decreasing = T), ]

undiagnosed_mi$sub_icb_loc_name <- factor(undiagnosed_mi$sub_icb_loc_name, 
                                                 levels = undiagnosed_mi$sub_icb_loc_name)

undiagnosed_mi$cumulative <- cumsum(undiagnosed_mi$excess_mi) 

undiagnosed_mi$cumulative <- 100 * undiagnosed_mi$cumulative/tail(undiagnosed_mi$cumulative, n = 1)

scaleRight_MI <- tail(undiagnosed_mi$cumulative, n = 1)/head(undiagnosed_mi$excess_mi, n = 1)

ggplot(undiagnosed_mi, aes(x = sub_icb_loc_name)) +
  geom_bar(aes(y = excess_mi), fill = "#e93f6f", stat = "identity") + 
  geom_path(aes(y = cumulative/scaleRight_MI, group = 1), color = "black", lwd = 2.5) + 
  geom_point(aes(y = cumulative/scaleRight_MI, group = 1), color = 'black', size = 5) + 
  scale_y_continuous(expand = c(0,0), sec.axis = sec_axis(~.*scaleRight_MI, name = "Cumulative (%)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = 'Preventable MI by ICS', y = 'Prevented Cases of MI') + 
  theme(panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 18))

######################## Pareto Charts - Costs ############################
# Costs by Treatment Quintile
preventable_events_costs <- lsoa_untreated_quintile[order(lsoa_untreated_quintile$uncontrolled_quintile, 
                                                             decreasing = T), ]

preventable_events_costs$uncontrolled_quintile <- factor(preventable_events_costs$uncontrolled_quintile, 
                                                         levels = preventable_events_costs$uncontrolled_quintile)
# Creating Cumulative Sum of Costs Saved
preventable_events_costs$cumulative <- cumsum(preventable_events_costs$costs_saved) 
# Converting Cumulative Sum to a Percentage
preventable_events_costs$cumulative <- 100 * preventable_events_costs$cumulative/tail(preventable_events_costs$cumulative, n = 1)
# Adjusting Right Hand Scale
scaleRight_costs <- tail(preventable_events_costs$cumulative, n = 1)/head(preventable_events_costs$costs_saved, n = 1)

ggplot(preventable_events_costs, aes(x = uncontrolled_quintile)) +
  geom_bar(aes(y = costs_saved/1000000), fill = "#00a3c7", stat = "identity") + 
  geom_path(aes(y = cumulative/scaleRight_costs/1000000, group = 1), color = "black", lwd = 2.5) + 
  geom_point(aes(y = cumulative/scaleRight_costs/1000000, group = 1), color = 'black', size = 5) + 
  scale_y_continuous(sec.axis = sec_axis(~.*scaleRight_costs*1000000, name = "Cumulative (%)")) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  labs(x = 'Uncontrolled Hypertension Quintile (5 is Most Uncontrolled)', y = 'Costs (in Millions £)') + 
  theme(panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24))

# Costs by IMD Decile
imd_costs <- lsoa_untreated_imd[order(lsoa_untreated_imd$imd_decile, decreasing = T), ]

imd_costs$imd_decile <- factor(imd_costs$imd_decile,levels = imd_costs$imd_decile)
# Creating Cumulative Sum of Costs Saved
imd_costs$cumulative <- cumsum(imd_costs$costs_saved) 
# Converting Cumulative Sum to a Percentage
imd_costs$cumulative <- 100 * imd_costs$cumulative/tail(imd_costs$cumulative, n = 1)
# Adjusting Right Hand Scale
scaleRight_IMDcosts <- tail(imd_costs$cumulative, n = 1)/head(imd_costs$costs_saved, n = 1)

ggplot(imd_costs, aes(x = imd_decile)) +
  geom_bar(aes(y = costs_saved/1000), fill = "#002f5f", stat = "identity") + 
  geom_path(aes(y = cumulative/scaleRight_IMDcosts/1000, group = 1), color = "black", lwd = 3) + 
  geom_point(aes(y = cumulative/scaleRight_IMDcosts/1000, group = 1), color = 'black', size = 5) + 
  scale_y_continuous(sec.axis = sec_axis(~.*scaleRight_IMDcosts*1000, name = "Cumulative (%)")) +
  theme(axis.text.x = element_text(vjust = 0.5),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24)) +
  labs(title = 'Costs of Strokes Resulting from Uncontrolled Hypertension', 
       x = 'IMD Decile (10 is Most Deprived)', y = 'Costs (in Thousands £)') 

# Region
regional_treatment_prev <- merge(lsoa_treatment, lsoa_region, by.x = "lsoa_code", by.y = "LSOA11CD") %>%
  group_by(NHSER22NM,NHSER22CD) %>%
  summarise(achievement_rate = mean(all_treated_percent_lsoa), 
            population = sum(lsoa_pop), 
            untreated_population = sum(untreated_pop80)) %>%
  mutate(prevented_strokes = floor(untreated_population/67),
         prevented_mi = floor(untreated_population/118), 
         total_costs_stroke_per_yr = (prevented_strokes)*(50850.72/5), 
         total_costs_mi_per_yr = (prevented_mi)*2052.12,
         hypertension_costs = (untreated_population)*161.67, 
         costs_saved = round(total_costs_stroke_per_yr+total_costs_mi_per_yr-hypertension_costs, 2), 
         ambulance_stroke = floor(prevented_strokes*0.755), 
         hospital_days_stroke = floor(prevented_strokes*22.2), 
         admissions_total = floor(prevented_strokes+prevented_mi))

regional_costs <- regional_treatment_prev[order(regional_treatment_prev$costs_saved,decreasing = T), ]

regional_costs$NHSER21NM <- factor(regional_costs$NHSER21NM, levels = regional_costs$NHSER21NM)

# Costs by ICS 
ics_total_costs <- ics_treatment_data[order(ics_treatment_data$costs_saved, decreasing = T), ]

ics_total_costs$SICBL22NM <- factor(ics_total_costs$SICBL22NM, 
                                    levels = ics_total_costs$SICBL22NM)
# Creating Cumulative Sum of Costs Saved
ics_total_costs$cumulative <- cumsum(ics_total_costs$costs_saved) 
# Converting Cumulative Sum to a Percentage
ics_total_costs$cumulative <- 100 * ics_total_costs$cumulative/tail(ics_total_costs$cumulative, n = 1)
# Adjusting Right Hand Scale
scaleRight_Totalcosts <- tail(ics_total_costs$cumulative, n = 1)/head(ics_total_costs$costs_saved, n = 1)

ggplot(ics_total_costs, aes(x = SICBL22NM)) +
  geom_bar(aes(y = costs_saved/1000), fill = "#00a3c7", stat = "identity") + 
  geom_path(aes(y = cumulative/scaleRight_Totalcosts/1000, group = 1), color = "red") + 
  geom_point(aes(y = cumulative/scaleRight_Totalcosts/1000, group = 1), color = 'black') + 
  scale_y_continuous(sec.axis = sec_axis(~.*scaleRight_Totalcosts*1000, name = "Cumulative (%)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = 'Costs Resulting from Uncontrolled Hypertension by ICS', 
       x = 'ICS', y = 'Costs (in Thousands £)') + 
  theme(panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"))

ics_total_costs_top_20perc <- ics_total_costs %>%
  filter(cumulative <= 48)

top20perc_shp <- merge(Eng_ICS, ics_total_costs_top_20perc, by.x = c('CCG21CD', 'CCG21NM'), 
                       by.y = c("SICBL22CD", "SICBL22NM"))

tm_shape(Eng_ICS) + 
  tm_borders(col = "black") +
  tm_shape(top20perc_shp) + 
  tm_polygons(col = "#00a3c7") + 
  tm_compass(position = c("left", "top")) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(main.title = "Location of Top 20% of Uncontrolled Hypertension", legend.outside = T)


################# Waterfall Plots - Excess Strokes #####################
### National Plot
waterfall(values = undiagnosed_hypertension_decile$excess_stroke, 
          labels = undiagnosed_hypertension_decile$ntile, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, 
          fill_colours = c("#dcfffd", "#c3f0fa", "#ade1f6", "#95d1f2", "#8bbee0"),
          rect_text_size = 2) +
  theme(axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(x = "Undiagnosed Hypertension Quintile", y = "Excess Strokes") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total")) +
  scale_y_continuous(expand = c(0,0))

### Waterfall for Missed Diagnoses only 
waterfall(values = missed_cvd_events_quintile$excess_stroke, 
          labels = missed_cvd_events_quintile$quintile, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, 
          fill_colours = c("#dcfffd", "#c3f0fa", "#ade1f6", "#95d1f2", "#8bbee0"), rect_text_size = 2) +
  scale_y_continuous(expand = c(0,0), labels = function(y) format(y, scientific = FALSE)) +
  theme(axis.text = element_text(size = 16), 
        axis.title = element_text(size = 20),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(x = "Missed Hypertension Diagnoses Quintile", y = "Excess Strokes") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

### Regional Plots
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

######################### Waterfall Plots - Excess MI ##########################
### National
waterfall(values = undiagnosed_hypertension_decile$excess_mi, 
          labels = undiagnosed_hypertension_decile$ntile, calc_total = T,
          total_rect_color = "#840034",
          fill_by_sign = FALSE, 
          fill_colours = c("#fde5e2", "#fbcdca", "#f9b6b5", "#f6a0a0", "#b9787f"), 
          rect_text_size = 2) +
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 20),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(x = "Undiagnosed Hypertension Quintile", y = "Excess MI") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total")) + 
  scale_y_continuous(expand = c(0,0))

# For Missed Diagnoses Only
waterfall(values = missed_cvd_events_quintile$excess_mi, 
          labels = missed_cvd_events_quintile$quintile, calc_total = T,
          total_rect_color = "#840034",
          fill_by_sign = FALSE, 
          fill_colours = c("#fde5e2", "#fbcdca", "#f9b6b5", "#f6a0a0", "#b9787f"), 
          rect_text_size = 2) +
  scale_y_continuous(expand = c(0,0), labels = function(y) format(y, scientific = FALSE)) +
  theme(axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(title = "Excess MI in the England", 
       x = "Missed Hypertension Diagnoses Quintile", 
       y = "Excess MI") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

### Regional Plots
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

################ Waterfall Plots - Non-CVD Metrics #####################
# For Cost 
waterfall(values = round(undiagnosed_hypertension_decile$total_costs_stroke_per_yr/1000000, digits = 1), 
          labels = undiagnosed_hypertension_decile$ntile, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, 
          fill_colours = c("#dcfffd", "#c3f0fa", "#ade1f6", "#95d1f2", "#8bbee0"), 
          rect_text_size = 2) +
  theme_minimal() +
  labs(title = "Costs of Excess Strokes in England", 
       x = "Undiagnosed Hypertension Quintile", 
       y = "Total Costs of Excess Strokes (£M)") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total")) +
  scale_y_continuous(labels = function(y) format(y, scientific = FALSE))

# Ambulance by Region
regional_ambulance_and_hospitals <- regional_treatment_prev %>%
  arrange(desc(prevented_strokes)) 

waterfall(values = round(regional_ambulance_and_hospitals$ambulance_stroke, digits = 1), 
          labels = regional_ambulance_and_hospitals$NHSER22NM, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, 
          fill_colours = c("#006177", "#00829F", "#00a3c7", "#33B5D2", "#66C7DD", "#7FD0E2","#B2E3EE"), 
          rect_text_size = 2) +
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 20),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(x = "NHS Region", 
       y = "Prevented Ambulance Calls (from Strokes)") + 
  scale_y_continuous(expand = c(0,0))

# Hospital Days
waterfall(values = round(regional_ambulance_and_hospitals$hospital_days_stroke, digits = 1), 
          labels = regional_ambulance_and_hospitals$NHSER22NM, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, 
          fill_colours = c("#006177", "#00829F", "#00a3c7", "#33B5D2", "#66C7DD", "#7FD0E2", "#B2E3EE"), 
          rect_text_size = 2) +
  labs(x = "NHS Region", 
       y = "Hospital Days Saved Annually (from Strokes)") +
  scale_y_continuous(expand = c(0,0), labels = function(y) format(y, scientific = FALSE)) +
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 20),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black")) 

# Hospital Admissions
waterfall(values = round(regional_ambulance_and_hospitals$admissions_total, digits = 1), 
          labels = regional_ambulance_and_hospitals$NHSER22NM, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, 
          fill_colours = c("#006177", "#00829F", "#00a3c7", "#33B5D2", "#66C7DD", "#7FD0E2", "#B2E3EE"), 
          rect_text_size = 2) +
  theme_minimal() +
  labs(x = "NHS Region", 
       y = "Prevented Hospital Admissions (Stroke and MI)") +
  scale_y_continuous(labels = function(y) format(y, scientific = FALSE)) +
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 20),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black")) + 
  scale_y_continuous(expand = c(0,0))

#### Mapping ####
# mapping where the events are
top_quintile_undiagnosed_lsoa <- lsoa_undiagnosed_hypertension %>%
  filter(ntile == 5)

top_quin_undiagnosed_shp <- merge(ENG_LSOA11, top_quintile_undiagnosed, by.x = "geo_code", by.y = "lsoa_code") %>%
  select(-c(geo_label, geo_labelw, label))

tm_shape(Eng_CCG) + 
  tm_borders(col = "black") +
  tm_shape(top_quin_undiagnosed_shp) + 
  tm_fill(col = "undiagnosed", palette = "Reds", breaks = c(250, 300, 350, 400, 450, 500, Inf), 
          title = "No. Undiagnosed") +
  tm_compass(position = c("left", "top")) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(main.title = "Location of Top Quintile of Undiagnosed Hypertension LSOAs", legend.outside = T)

# Merge LSOA to CCG to Region to plot Regionally 
top_quin_shp_region <- merge(top_quin_undiagnosed_shp, lsoa_region, by.x = 'geo_code', by.y = 'LSOA11CD')

east_eng_top_quin <- subset(top_quin_shp_region, NHSER21NM == 'East of England')
london_top_quin <- subset(top_quin_shp_region, NHSER21NM=="London")
midlands_top_quin <- subset(top_quin_shp_region, NHSER21NM == "Midlands")
north_east_top_quin <- subset(top_quin_shp_region, NHSER21NM == 'North East and Yorkshire')
north_west_top_quin <- subset(top_quin_shp_region, NHSER21NM == 'North West')
south_east_top_quin <- subset(top_quin_shp_region, NHSER21NM == 'South East')
south_west_top_quin <- subset(top_quin_shp_region, NHSER21NM == 'South West')

tm_shape(east_eng_top_quin) + 
  tm_fill(col = "undiagnosed", palette = "Reds", breaks = c(250, 300, 350, 400, 450, 500, Inf), 
          title = "No. Undiagnosed") +
  tm_compass(position = c("left", "top")) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(main.title = "Top Quintile of Undiagnosed Hypertension LSOAs in East England", legend.outside = T) +
  tm_shape(east_eng_ccg) + 
  tm_borders(col = "black")

tm_shape(london_top_quin) + 
  tm_fill(col = "undiagnosed", palette = "Reds", breaks = c(250, 300, 350, 400, 450, 500, Inf), 
          title = "No. Undiagnosed") +
  tm_compass(position = c("left", "top")) + 
  tm_scale_bar(position = c("left", "bottom")) + 
  tm_layout(main.title = "Top Quintile of Undiagnosed Hypertension LSOAs in London", legend.outside = T) +
  tm_shape(london_ccg) + 
  tm_borders(col = "black")

tm_shape(midlands_top_quin) + 
  tm_fill(col = "undiagnosed", palette = "Reds", breaks = c(250, 300, 350, 400, 450, 500, Inf), 
          title = "No. Undiagnosed") +
  tm_compass(position = c("left", "top")) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(main.title = "Top Quintile of Undiagnosed Hypertension LSOAs in the Midlands", legend.outside = T) +
  tm_shape(midlands_ccg) + 
  tm_borders(col = "black")

tm_shape(north_east_top_quin) + 
  tm_fill(col = "undiagnosed", palette = "Reds", breaks = c(250, 300, 350, 400, 450, 500, Inf), 
          title = "No. Undiagnosed") +
  tm_compass(position = c("left", "top")) + 
  tm_scale_bar(position = c("left", "bottom")) + 
  tm_layout(main.title = "Top Quintile of Undiagnosed Hypertension LSOAs in NE England", legend.outside = T) +
  tm_shape(north_east_ccg) + 
  tm_borders(col = "black")

tm_shape(north_west_top_quin) + 
  tm_fill(col = "undiagnosed", palette = "Reds", breaks = c(250, 300, 350, 400, 450, 500, Inf), 
          title = "No. Undiagnosed") +
  tm_compass(position = c("right", "top")) + 
  tm_scale_bar(position = c("right", "bottom"), width = 0.2) + 
  tm_layout(main.title = "Top Quintile of Undiagnosed Hypertension LSOAs in NW England", legend.outside = T) +
  tm_shape(north_west_ccg) + 
  tm_borders(col = "black")

tm_shape(south_east_top_quin) + 
  tm_fill(col = "undiagnosed", palette = "Reds", breaks = c(250, 300, 350, 400, 450, 500, Inf), 
          title = "No. Undiagnosed") +
  tm_compass(position = c("right", "top")) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(main.title = "Top Quintile of Undiagnosed Hypertension LSOAs in SE England", legend.outside = T) +
  tm_shape(south_east_ccg) + 
  tm_borders(col = "black")

tm_shape(south_west_top_quin) + 
  tm_fill(col = "undiagnosed", palette = "Reds", breaks = c(250, 300, 350, 400, 450, 500, Inf), 
          title = "No. Undiagnosed") +
  tm_compass(position = c("left", "top")) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(main.title = "Top Quintile of Undiagnosed Hypertension LSOAs in SW England", legend.outside = T) +
  tm_shape(south_west_ccg) + 
  tm_borders(col = "black")

# Mapping Location of Top 25 ICS by Undiagnosed Hypertension/Stroke
top25_undiagnosed_hypertension_ics <- cvd_events_from_undiagnosed %>%
  arrange(undiagnosed_hypertension) %>%
  tail(25)

top25_undiagnosed_shp <- merge(Eng_ICS, top25_undiagnosed_hypertension_ics, 
                               by.x = c('SICBL22CD', 'SICBL22NM'), 
                               by.y = c('sub_icb_loc_ons_code', 'sub_icb_loc_name')) %>%
  select(-c(OBJECTID:SHAPE_Area))

tm_shape(Eng_ICS) + 
  tm_borders(col = "black") +
  tm_shape(top25_undiagnosed_shp) + 
  tm_fill(col = "#00a3c7", alpha = 0.6) + 
  tm_compass(position = c("right", "top")) + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_layout(main.title = "Top 25 ICS' by Undiagnosed Hypertension")

#### South-East England Analysis ####
se_uncontrolled <- merge(lsoa_uncontrolled_prev, lsoa_region, by.x = "lsoa_code", by.y = "LSOA11CD") %>%
  select(-FID) %>%
  filter(NHSER21CD == "E40000005") %>% # SE England ONS Code
  mutate(uncontrolled_quintile = ntile(needed_for_80_treat, 5)) %>%
  mutate(across(uncontrolled_quintile, as_factor)) %>%
  group_by(uncontrolled_quintile) %>%
  summarise(population = sum(lsoa_pop),
            tot_untreated = sum(untreated_80percent)) %>%
  mutate(preventable_strokes = floor(tot_untreated/67), 
         preventable_mi = floor(tot_untreated/118), 
         total_costs_stroke_per_yr = (preventable_strokes)*(50850.72/5), 
         total_costs_mi_per_yr = (preventable_mi)*2052.12,
         hypertension_costs = (tot_untreated)*161.67, 
         costs_saved = round(total_costs_stroke_per_yr+total_costs_mi_per_yr-hypertension_costs, 2))

se_uncontrolled_ungrouped <- merge(lsoa_uncontrolled_prev, lsoa_region, by.x = "lsoa_code", by.y = "LSOA11CD") %>%
  select(-FID) %>%
  filter(NHSER21CD == "E40000005") %>% # SE England ONS Code
  mutate(uncontrolled_quintile = ntile(untreated_perc, 10)) %>%
  mutate(across(uncontrolled_quintile, as_factor))

# SE England
se_costs <- se_uncontrolled[order(se_uncontrolled$costs_saved, decreasing = T), ]

se_costs$NHSER21NM <- factor(se_costs$uncontrolled_quintile, levels = se_costs$uncontrolled_quintile)

# Creating Cumulative Sum of Costs Saved
se_costs$cumulative <- cumsum(se_costs$costs_saved) 
# Converting Cumulative Sum to a Percentage
se_costs$cumulative <- 100 * se_costs$cumulative/tail(se_costs$cumulative, n = 1)
# Adjusting Right Hand Scale
scaleRight_SECosts <- tail(se_costs$cumulative, n = 1)/head(se_costs$costs_saved, n = 1)

ggplot(se_costs, aes(x = NHSER21NM)) +
  geom_bar(aes(y = costs_saved/1000000), fill = "#00a3c7", stat = "identity") + 
  geom_path(aes(y = cumulative/scaleRight_SECosts/1000000, group = 1), color = "black", lwd = 3) + 
  geom_point(aes(y = cumulative/scaleRight_SECosts/1000000, group = 1), color = 'black', size = 5) + 
  scale_y_continuous(sec.axis = sec_axis(~.*scaleRight_SECosts*1000000, name = "Cumulative (%)")) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  labs(x = 'Uncontrolled Hypertension Quintile (5 is Most Uncontrolled)', y = 'Costs (in Millions £)') + 
  theme(panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24))