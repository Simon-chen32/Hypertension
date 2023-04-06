# set working directory 
setwd("C:/Users/SIMC/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - 03 CVD Secondary prevention/Analysis")

#### Installing Packages ####
library(tidyverse)
library(tmap)
library(sf)
library(janitor)
library(stringr)
library(RColorBrewer)
library(ggQC)
library(extrafont)
library(waterfalls)
library(gt)
library(gtExtras)
library(gridExtra)

# loading palette 
lcp_blue_to_red <- c("#002f5f", "#345c80", "#627e9b",  "#8da8ad", "#c1d3d6", "#fcdadb", "#f595a3", "#e93f6f", "#821a4d")
lcp_red_to_blue_7col <- c("#821a4d", "#e93f6f", "#f8b7bd", "#dce6e7", "#c1cdd8", "#345c80", "#002f5f")

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

gp_to_lsoa_hypertension_prev$sub_icb_loc_name <- gsub("NHS Cornwall and The Isles Of Scilly ICB - 11N", "NHS Cornwall and the Isles of Scilly ICB - 11N", gp_to_lsoa_hypertension_prev$sub_icb_loc_name)
gp_to_lsoa_hypertension_prev$sub_icb_loc_name <- gsub("NHS Hampshire and Isle Of Wight ICB - 10R", "NHS Hampshire and Isle of Wight ICB - 10R", gp_to_lsoa_hypertension_prev$sub_icb_loc_name)
gp_to_lsoa_hypertension_prev$sub_icb_loc_name <- gsub("NHS Hampshire and Isle Of Wight ICB - D9Y0V", "NHS Hampshire and Isle of Wight ICB - D9Y0V", gp_to_lsoa_hypertension_prev$sub_icb_loc_name)

# Lookup File
lsoa_ics <- read_csv("~/Lookup Files/LSOA_(2011)_to_Sub_ICB_Locations_to_Integrated_Care_Boards_(July_2022)_Lookup_in_England.csv") %>%
  select(-ObjectId)
lsoa_region <- read_csv("LSOA to Region Lookup File.csv")
ics_region <- read_csv("~/Lookup Files/Sub_ICB_Locations_to_ICB_to_NHS_England_(Region)_(July_2022)_Lookup.csv")

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
         preventable_mi = floor(untreated_pop80/118)) %>%
  merge(., lsoa_ics, by.x = "lsoa_code", by.y = "LSOA11CD")

ics_treatment_data <- gp_to_lsoa_hypertension_prev %>%
  group_by(sub_icb_loc_ons_code, sub_icb_loc_name, sub_icb_loc_ods_code) %>%
  summarise(ics_pop = sum(number_of_patients), 
            hypertension_prev = sum(prevalence_percent_21_22*(number_of_patients/ics_pop)),
            treated_under80_percent = round(sum((number_of_patients/ics_pop)*under79_achievement_net_exceptions_21_22, na.rm = T), digits = 2), 
            treated_over80_percent = round(sum((number_of_patients/ics_pop)*over80_achievement_net_exceptions_21_22, na.rm = T), digits = 2), 
            under80_rate = sum((number_of_patients/ics_pop)*(under79_denominator_21_22/register_21_22)),
            all_treated_percent = round((under80_rate*treated_under80_percent + (1-under80_rate)*treated_over80_percent), digits = 2)) %>%
  mutate(untreated_perc_under80 = 100 - treated_under80_percent, 
         untreated_perc_over80 = 100 - treated_over80_percent, 
         untreated_80perc = 80 - all_treated_percent,
         untreated_100perc = 100 - all_treated_percent, 
         treated_pop = round(all_treated_percent*(ics_pop*(hypertension_prev/100))/100),
         untreated_pop80 = round(untreated_80perc*(ics_pop*(hypertension_prev/100))/100),
         untreated_pop = round(untreated_100perc*(ics_pop*hypertension_prev/100)/100), 
         preventable_strokes = floor(untreated_pop80/67), 
         preventable_mi = floor(untreated_pop80/118),
         total_costs_stroke_per_yr = (preventable_strokes)*(50850.72/5), 
         total_costs_mi_per_yr = (preventable_mi)*2052.12,
         hypertension_costs = (untreated_pop80)*161.67, 
         costs_saved = round(total_costs_stroke_per_yr+total_costs_mi_per_yr-hypertension_costs, 2)) %>%
  merge(., ics_region, by.x = c("sub_icb_loc_ons_code", "sub_icb_loc_name", "sub_icb_loc_ods_code"), 
        by.y = c("SICBL22CD", "SICBL22NM", "SICBL22CDH"))

lsoa_treatment_ranked <- lsoa_treatment %>%
  mutate(treated_pop = round(all_treated_percent_lsoa*(lsoa_pop*(hypertension_prev/100))/100)) %>%
  select(-c(ICB22CD:LAD22NM)) %>%
  group_by(SICBL22NM) %>%
  mutate(treatment_rank = rank(desc(all_treated_percent_lsoa), ties.method = "min"))

top25_lsoa_treatment <- lsoa_treatment %>%
  arrange(all_treated_percent_lsoa) %>%
  tail(25) %>%
  select(lsoa_code, lsoa_pop, all_treated_percent_lsoa, hypertension_prev, LSOA11NM, SICBL22NM)

bottom25_lsoa_treatment <- lsoa_treatment %>%
  arrange(all_treated_percent_lsoa) %>%
  head(25) %>%
  select(lsoa_code, lsoa_pop, all_treated_percent_lsoa, hypertension_prev, LSOA11NM, SICBL22NM)

write_csv(top25_lsoa_treatment, "Highest Treatment Rate LSOAs.csv")
write_csv(bottom25_lsoa_treatment, "Lowest Treatment Rate LSOAs.csv")

ics_treatment_ranked <- ics_treatment_data %>%
  ungroup() %>%
  mutate(treatment_rank = rank(desc(all_treated_percent), ties.method = "min"))

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

####################### Treatment by ICS Plots ################################
ics_treatment_rate_ranked <- ggplot(ics_treatment_data, aes(x = reorder(sub_icb_loc_name, -all_treated_percent))) +
  geom_bar(aes(y = all_treated_percent), fill = "#00a3c7", stat = "identity") + 
  geom_hline(yintercept = 80, color = "black", lwd = 2, linetype = 2) + 
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(y = 'Treatment Rate') + 
  coord_cartesian(ylim = c(50, 100)) +
  theme(panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 16))
ggsave("ICS Ordered by Treatment Rate.jpeg", plot = ics_treatment_rate_ranked, units = "px",
       width = 2048, height = 1152, dpi = 320, scale = 2)

ics_treatment_shp <- merge(Eng_ICS, ics_treatment_data, by.x = "SICBL22CD", 
                           by.y = "sub_icb_loc_ons_code") %>%
  select(-c(OBJECTID, BNG_E:sub_icb_loc_name, under80_rate, untreated_perc_under80, untreated_perc_over80))

ics_treatment_map <- tm_shape(ics_treatment_shp) + 
  tm_polygons(col = "untreated_80perc", title = "Needed for 80% Treatment",
              palette = c("#d4dc5c", "#ffeacd", "#fcdadb", "#f595a3", "#e93f6f", "#821a4d"), 
              labels = c("At Target", "Less than 5%", "5 to 10%", "10 to 15%", "15 to 20%", "More than 20%"),
              breaks = c(-Inf, 0, 5, 10, 15, 20, Inf)) + 
  tm_layout(frame = F, legend.outside = T, legend.outside.position = "top", legend.title.size = 1, 
            fontfamily = "serif", legend.title.fontface = "bold", legend.text.size = 0.6) + 
  tm_compass(position = c("right", "top")) + 
  tm_scale_bar(position = c("right", "bottom"))
ics_treatment_map
jpeg("Needed for 80 Treatment Map.jpeg", width = 2048, height = 1536, res = 320)
print(ics_treatment_map)
dev.off()

treatment_21 <- tm_shape(ics_treatment_shp) +
  tm_polygons(col = "all_treated_percent", palette = lcp_red_to_blue_7col, title = "Treatment Rate (2021)", 
              breaks = c(-Inf, 60, 65, 70, 75, 80, Inf)) + 
  tm_compass(position = c("right", "top")) + 
  tm_scale_bar() + 
  tm_layout(frame = FALSE, legend.title.fontface = "bold", fontfamily = "Times New Roman", 
            legend.title.size = 2, legend.text.size = 0.8) 
treatment_21

# treatment_comparison_plot <- tmap_arrange(treatment_2019_map, treatment_21)
# tmap_save(treatment_comparison_plot, "Hypertension Treatment Comparison Plot.jpeg", width = 2048, height = 1152, dpi = 320)  

top_10_highest_treatment <- ics_treatment_data %>%
  ungroup() %>%
  mutate(treatment_decile = ntile(all_treated_percent, 10)) %>%
  filter(treatment_decile == 10)

top_10_lowest_treatment <- ics_treatment_data %>%
  ungroup() %>%
  arrange(all_treated_percent) %>%
  head(10)

top10_highest_control <- top_10_highest_treatment %>%
  select(sub_icb_loc_name, all_treated_percent, NHSER22NM) %>%
  arrange(desc(all_treated_percent)) %>%
  gt() %>%
  fmt_percent(columns = all_treated_percent, decimals = 2, scale_values = F) %>%
  gt_theme_538() %>%
  cols_label(sub_icb_loc_name = "ICS", 
             all_treated_percent = "Hypertension Control", 
             NHSER22NM = "Region") %>%
  as_raw_html()

top10_lowest_control <- top_10_lowest_treatment %>%
  select(sub_icb_loc_name, all_treated_percent, NHSER22NM) %>%
  arrange(all_treated_percent) %>%
  gt() %>%
  fmt_percent(columns = all_treated_percent, decimals = 2, scale_values = F) %>%
  gt_theme_538() %>%
  cols_label(sub_icb_loc_name = "ICS", 
             all_treated_percent = "Hypertension Control", 
             NHSER22NM = "Region") %>%
  as_raw_html()

hyp_treatment_tables <- data.frame(top10 = top10_highest_control, 
                                   bottom10 = top10_lowest_control)

hyp_treatment_tables %>%
  gt() %>%
  fmt_markdown(columns = everything()) %>%
  cols_label(top10 = "10 Best Hypertension Control Rates", 
             bottom10 = "10 Worst Hypertension Control Rates") %>%
  gtsave("Hypertension Control Tables.png", vwidth = 1920, vheight = 1080)


write_csv(top_10_highest_treatment, "Top 10 Highest Treatment ICS.csv")
write_csv(top_10_lowest_treatment, "Top 10 Lowest Treatment ICS.csv")

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
         excess_mi_UB = round(undiagnosed_hypertension/94), 
         undiagnosed_per_100000 = round(undiagnosed_hypertension/(population/100000), digits = 2), 
         excess_stroke100 = round(undiagnosed_hypertension*1.25/67), 
         excess_stroke100_LB = round(undiagnosed_hypertension*1.25/84), 
         excess_stroke100_UB = round(undiagnosed_hypertension*1.25/57), 
         excess_mi100 = round(undiagnosed_hypertension*1.25/118), 
         excess_mi100_LB = round(undiagnosed_hypertension*1.25/171), 
         excess_mi100_UB = round(undiagnosed_hypertension*1.25/94), 
         excess_stroke50 = round(undiagnosed_hypertension*0.625/67), 
         excess_mi50 = round(undiagnosed_hypertension*0.625/118), 
         total_costs_stroke_per_yr = (excess_stroke)*(50850.72/5), 
         total_costs_mi_per_yr = (excess_mi)*2052.12,
         hypertension_costs = (undiagnosed_hypertension)*161.67, 
         costs_saved = total_costs_stroke_per_yr+total_costs_mi_per_yr-hypertension_costs) %>%
  drop_na()

top_decile_undiagnosed_table <- cvd_events_from_undiagnosed %>%
  ungroup() %>%
  mutate(decile = ntile(undiagnosed_hypertension, 10)) %>%
  filter(decile == 10) %>%
  select(sub_icb_loc_name, undiagnosed_hypertension, undiagnosed_rate, population, excess_stroke:excess_mi_UB) %>%
  arrange(desc(undiagnosed_rate)) %>%
  gt() %>% 
  fmt_number(columns = c(undiagnosed_hypertension, population), decimals = 1, suffixing = c("K", "M"), use_seps = T) %>%
  fmt_percent(columns = undiagnosed_rate, scale_values = F) %>%
  gt_plt_conf_int(column = excess_stroke,
                  ci_columns = c(excess_stroke_LB, excess_stroke_UB), 
                  palette = c("#00a3c7", "#c1d3d6", "#00a3c7", "black"), 
                  width = 90, text_size = 2.5,) %>%
  gt_plt_conf_int(column = excess_mi,
                  ci_columns = c(excess_mi_LB, excess_mi_UB), 
                  palette = c("#e93f6f", "#c1d3d6", "#e93f6f", "black"), 
                  width = 90, text_size = 2.5,) %>%
  cols_hide(c(excess_stroke_LB, excess_stroke_UB, excess_mi_LB, excess_mi_UB)) %>%
  tab_header(title = "CVD Events for Top 10 Worse ICS") %>%
  gt_theme_nytimes() %>%
  cols_label(sub_icb_loc_name = "ICS", 
             undiagnosed_hypertension = "Number Undiagnosed",
             undiagnosed_rate = "Undiagnosed Rate",
             population = "Population", 
             excess_stroke = "Preventable Strokes", 
             excess_mi = "Preventable MI")
top_decile_undiagnosed_table

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

undiagnosed_cvd_events_by_region <- ggplot(regional_undiagnosed_cvd_event_long, aes(x = NHSER22NM, y = excess, 
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
        axis.title = element_text(size = 16),
        axis.title.y = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black")) 
undiagnosed_cvd_events_by_region
jpeg("Undiagnosed Hypertension CVD Events by Region.jpeg", width = 1920, height= 1080, res = 300)
print(undiagnosed_cvd_events_by_region)
dev.off()

undiagnosed_region_cvd_events_table <- regional_undiagnosed_cvd_event %>%
  select(NHSER22NM, tot_undiagnosed, rate_per_100000, region_population, excess_stroke:excess_mi_UB) %>%
  arrange(NHSER22NM) %>%
  gt() %>% 
  fmt_number(columns = c(tot_undiagnosed, region_population, rate_per_100000), decimals = 1, suffixing = "K", use_seps = T) %>%
  gt_plt_conf_int(column = excess_stroke,
                  ci_columns = c(excess_stroke_LB, excess_stroke_UB), 
                  palette = c("#00a3c7", "#c1d3d6", "#00a3c7", "black"), 
                  width = 90, text_size = 2.5,) %>%
  gt_plt_conf_int(column = excess_mi,
                  ci_columns = c(excess_mi_LB, excess_mi_UB), 
                  palette = c("#e93f6f", "#c1d3d6", "#e93f6f", "black"), 
                  width = 90, text_size = 2.5,) %>%
  cols_hide(c(excess_stroke_LB, excess_stroke_UB, excess_mi_LB, excess_mi_UB)) %>%
  tab_header(title = "CVD Events by Region") %>%
  gt_theme_nytimes() %>%
  cols_label(NHSER22NM = "Region", 
             tot_undiagnosed = "Number Undiagnosed",
             rate_per_100000 = "Rate per 100,000",
             region_population = "Population", 
             excess_stroke = "Preventable Strokes", 
             excess_mi = "Preventable MI")

undiagnosed_region_cvd_events_table 

undiagnosed_region_cvd_events_table %>% gtsave('CVD Events from Undiagnosed Hypertension.png')
###

# Decile Analysis 
decile_by_hypertension_rate <- lsoa_undiagnosed_hypertension %>%
  mutate(hypertension_decile = ntile(hypertension_prev, 10)) %>%
  mutate(across(hypertension_decile, as_factor)) %>%
  group_by(hypertension_decile) %>%
  summarise(undiagnosed = sum(undiagnosed), 
            population = sum(lsoa_pop),
            average_prevalence = round(sum((lsoa_pop/population)*hypertension_prev),2),
            undiagnosed_rate = round(sum((lsoa_pop/population)*undiagnosed_prev),2),
            hypertension_population = sum(hypertension_cases)) %>%
  mutate(excess_stroke = floor(undiagnosed/67),
         excess_mi = floor(undiagnosed/118), 
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
ggplot(decile_by_hypertension_rate, aes(x = hypertension_decile, fill = hypertension_decile)) +
  scale_fill_manual(values = c("#fcdadb", "#f5c2c3", "#efaaaa", "#e89192", "#e2797a", "#db6161", "#d54949", "#ce3031", "#c81818", "#c10000")) + 
  geom_col(aes(y = hypertension_population/1000)) +
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
ics_missed_hypertension <- read_csv("Missed Diagnoses of Hypertension Ranked.csv")
regional_missed_hypertension <- read_csv("Missed Hypertension Cases by Region.csv") %>%
  mutate(missed_diagnoses_rate = round((missed_diagnoses/region_pop)*100, 2), 
         missed_diagnoses_per100000 = round((missed_diagnoses/region_pop)*100000, 2))

region_missed_hypertension_table <- regional_missed_hypertension %>%
  select(NHSER22NM, hypertension_prev, missed_diagnoses, missed_diagnoses_lb, missed_diagnoses_ub,
         missed_diagnoses_per100000) %>%
  mutate(missed_diagnoses = prettyNum(missed_diagnoses, big.mark = ',', scientific = F),
         missed_diagnoses_lb = prettyNum(missed_diagnoses_lb, big.mark = ',', scientific = F),
         missed_diagnoses_ub = prettyNum(missed_diagnoses_ub, big.mark = ',', scientific = F)) %>%
  unite("CI", missed_diagnoses_lb:missed_diagnoses_ub, sep = ", ", remove = T) %>%
  mutate(CI = paste0("(",CI, ")"))

# Finding the impact of missed diagnosis due to COVID
missed_hypertension_cvd_events <- ics_missed_hypertension %>%
  mutate(excess_stroke = ceiling(missed_diagnoses/67),
         excess_stroke_LB = ceiling(missed_diagnoses/84),
         excess_stroke_UB = ceiling(missed_diagnoses/57), 
         excess_mi = ceiling(missed_diagnoses/118),
         excess_mi_LB = ceiling(missed_diagnoses/171), 
         excess_mi_UB = ceiling(missed_diagnoses/94),
         total_costs_stroke_per_yr = (excess_stroke)*(50850.72/5), 
         total_costs_mi_per_yr = (excess_mi)*2052.12,
         hypertension_costs = (missed_diagnoses)*161.67, 
         costs_saved = total_costs_stroke_per_yr+total_costs_mi_per_yr-hypertension_costs,
         missed_diagnoses_per_100000 = round(missed_diagnoses/(population/100000), digits = 2)) 

top_decile_missed_table <- missed_hypertension_cvd_events %>%
  ungroup() %>%
  mutate(decile = ntile(missed_diagnoses_per_100000, 10)) %>%
  filter(decile == 10) %>%
  select(SICBL22NM, missed_diagnoses, missed_diagnoses_per_100000, population, excess_stroke:excess_mi_UB) %>%
  arrange(desc(missed_diagnoses_per_100000)) %>%
  mutate(stroke_ci = paste(excess_stroke_LB, excess_stroke_UB, sep = ", "), 
         mi_ci = paste(excess_mi_LB, excess_mi_UB, sep = ", ")) %>%
  gt() %>% 
  fmt_number(columns = population, decimals = 1, suffixing = c("K"), use_seps = T) %>%
  gt_merge_stack(col1 = excess_stroke,
                 col2 = stroke_ci) %>% 
  gt_merge_stack(col1 = excess_mi, col2 = mi_ci) %>%
  cols_hide(c(excess_stroke_LB, excess_stroke_UB, excess_mi_LB, excess_mi_UB)) %>%
  tab_header(title = "CVD Events for Top 10 Most Missed ICS") %>%
  gt_theme_nytimes() %>%
  cols_label(SICBL22NM = "ICS", 
             missed_diagnoses = "Number of Missed CVD Events",
             missed_diagnoses_per_100000 = "Missed per 100,000",
             population = "Population", 
             excess_stroke = "Missed Strokes", 
             excess_mi = "Missed MI")
top_decile_missed_table

# Plotting Regional Bar Chart
regional_missed <- regional_missed_hypertension %>%
  mutate(stroke = floor(missed_diagnoses/67),
         stroke_LB = floor(missed_diagnoses/84),
         stroke_UB = floor(missed_diagnoses/57), 
         mi = floor(missed_diagnoses/118),
         mi_LB = floor(missed_diagnoses/171), 
         mi_UB = floor(missed_diagnoses/94)) %>%
  select(-c(missed_diagnoses_lb, missed_diagnoses_ub, missed_diagnoses_rate))

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
  select(-c(8:13, 15:20))

missed_cvd_events_by_region <- ggplot(region_missed_hypertension_long, aes(x = NHSER22NM, y = excess, 
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
        axis.title = element_text(size = 16),
        axis.title.y = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"))

jpeg("Missed Hypertension Diagnoses CVD Events by Region.jpeg", width = 1920, height= 1080, res = 300)
print(missed_cvd_events_by_region)
dev.off()

###

# Decile Analysis
missed_cvd_events <- missed_hypertension_cvd_events %>%
  mutate(quintile = ntile((missed_diagnoses), 5)) %>%
  mutate(across(quintile, as_factor)) 

missed_cvd_events_quintile <- missed_cvd_events %>%
  group_by(quintile) %>%
  summarise(missed_diagnoses = sum(missed_diagnoses),
            missed_diagnoses_per_100000 = sum(missed_diagnoses_per_100000),
            population = sum(population), 
            excess_stroke = sum(excess_stroke), 
            excess_mi = sum(excess_mi),
            stroke_LB = sum(excess_stroke_LB), 
            stroke_UB = sum(excess_stroke_UB),
            mi_LB = sum(excess_mi_LB), 
            mi_UB = sum(excess_mi_UB)) %>%
  mutate(missed_prop = round(missed_diagnoses/sum(missed_diagnoses)*100,1))

missed_cvd_events_shp <- merge(Eng_ICS, missed_cvd_events, by = c("SICBL22CD", "SICBL22NM"))

missed_events_quintile_map <- tm_shape(missed_cvd_events_shp) + 
  tm_polygons(col = "quintile", title = "Sub-ICB Loc. by Missed Diagnoses Quintile", 
              palette = c("white", "#c9e6ee", "#95d1e3", "#5ebfd8", "#00a3c7"), 
              labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5")) + 
  tm_layout(frame = F, asp = 1, 
            fontfamily = "serif", 
            legend.title.fontface = "bold", legend.title.size = 1.25,
            legend.text.size = 0.6) + 
  tm_compass(position = c("right", "top")) + 
  tm_scale_bar(position = c("right", "bottom"))
missed_by_quintile_map <- tmap_grob(missed_events_quintile_map)

######################### Deprivation Analysis ###############################
# Load in IMD Data
lsoa_imd <- read_csv("Cleaned LSOA IMD.csv")

# LSOA Treatment Analysis
lsoa_untreated_imd <- merge(lsoa_treatment, lsoa_imd, by.x = "lsoa_code", by.y = "lsoa_code_2011") %>%
  mutate(across(imd_decile, as_factor)) %>%
  group_by(imd_decile) %>%
  summarise(population = sum(lsoa_pop), 
            achievement_rate = sum((lsoa_pop/population)*all_treated_percent_lsoa),
            hypertension_prev = sum((lsoa_pop/population)*hypertension_prev),
            tot_untreated = sum(untreated_pop80)) %>%
  mutate(preventable_strokes = floor(tot_untreated/67), 
         preventable_mi = floor(tot_untreated/118),
         total_costs_stroke_per_yr = (preventable_strokes)*(50850.72/5), 
         total_costs_mi_per_yr = (preventable_mi)*2052.12,
         hypertension_costs = (tot_untreated)*161.67, 
         costs_saved = round(total_costs_stroke_per_yr+total_costs_mi_per_yr-hypertension_costs, 2))

lsoa_undiagnosed_imd <- merge(lsoa_undiagnosed_hypertension, lsoa_imd, by.x = 'lsoa_code', by.y = 'lsoa_code_2011') %>%
  mutate(across(imd_decile, as_factor)) %>%
  group_by(imd_decile) %>%
  summarise(undiagnosed = sum(undiagnosed), 
            population = sum(lsoa_pop),
            hypertension_prev = sum((lsoa_pop/population)*hypertension_prev),
            hypertension_pop = sum(hypertension_cases)) %>%
  mutate(lifetime_cvd_event = floor(undiagnosed/35.96),
         lifetime_excess_stroke = floor(undiagnosed/141.4),
         lifetime_excess_mi = floor(undiagnosed/215.4), 
         treated_5yr_cvd_event = floor(undiagnosed/180.9),
         treated_5yr_stroke = floor(undiagnosed/1068),
         treated_5yr_mi = floor(undiagnosed/675.5),
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
# Treatment
untreated_by_imd <- ggplot(lsoa_untreated_imd, aes(x = imd_decile, fill = imd_decile)) +
  scale_fill_manual(values = c("#fcdadb", "#f5c2c3", "#efaaaa", "#e89192", "#e2797a", "#db6161", "#d54949", "#ce3031", "#c81818", "#c10000")) + 
  geom_col(aes(y = tot_untreated/1000)) +
  labs(x = "IMD Decile (1 is Most Deprived)", 
       y = "Uncontrolled Population (in 1000s)") +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 20),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.position = "none") 
ggsave("Relationship between IMD and Untreated Population.jpeg", plot = untreated_by_imd, 
       width = 1920, height = 1080, units = "px", dpi = 320, scale = 2)

untreated_rate_by_imd <- ggplot(lsoa_untreated_imd, aes(x = imd_decile, fill = imd_decile)) +
  scale_fill_manual(values = c("#fcdadb", "#f5c2c3", "#efaaaa", "#e89192", "#e2797a", "#db6161", "#d54949", "#ce3031", "#c81818", "#c10000")) + 
  geom_col(aes(y = achievement_rate)) +
  labs(x = "IMD Decile (1 is Most Deprived)", 
       y = "Hypertension Treatment Rate") +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,70,10), limits = c(0,70)) +
  theme(axis.text = element_text(family = "Times New Roman", size = 10), 
        axis.title = element_text(family = "Times New Roman", face = "bold", size = 10),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.position = "none") 
ggsave("Relationship between IMD and Uncontrolled Hypertension.jpeg", plot = untreated_rate_by_imd, 
       width = 1920, height = 1080, units = "px", dpi = 320, scale = 2)


# Undiagnosed Hypertension
ggplot(lsoa_undiagnosed_imd, aes(x = imd_decile, fill = imd_decile)) +
  scale_fill_manual(values = c("#fcdadb", "#f5c2c3", "#efaaaa", "#e89192", "#e2797a", "#db6161", "#d54949", "#ce3031", "#c81818", "#c10000")) + 
  geom_col(aes(y = hypertension_pop/1000)) +
  geom_point(aes(y = undiagnosed/1000), size = 3) +
  geom_path(aes(y = undiagnosed/1000, group = 1)) +
  labs(x = "IMD Decile (1 is Most Deprived)", 
       y = "Hypertension Population (in 1000s)") +
  geom_text(aes(4.5, 725, label = 'Undiagnosed Population', vjust = -1.5, hjust = 0.25), size = 5) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Undiagnosed Population (in 1000s)"), 
                     expand = c(0,0)) +
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 20),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.position = "none") 

hypertension_by_imd <- ggplot(lsoa_undiagnosed_imd, aes(x = imd_decile, fill = imd_decile)) +
  scale_fill_manual(values = c("#fcdadb", "#f5c2c3", "#efaaaa", "#e89192", "#e2797a", "#db6161", "#d54949", "#ce3031", "#c81818", "#c10000")) + 
  geom_col(aes(y = hypertension_pop/1000)) +
  labs(x = "IMD Decile (1 is Most Deprived)", 
       y = "Hypertension Population (in 1000s)") +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 20),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.position = "none") 
ggsave("Relationship between IMD and Hypertension.jpeg", plot = hypertension_by_imd, 
       width = 1920, height = 1080, units = "px", dpi = 320, scale = 2)

hyp_prev_by_imd <- ggplot(lsoa_undiagnosed_imd, aes(x = imd_decile, fill = imd_decile)) +
  scale_fill_manual(values = c("#fcdadb", "#f5c2c3", "#efaaaa", "#e89192", "#e2797a", "#db6161", "#d54949", "#ce3031", "#c81818", "#c10000")) + 
  geom_col(aes(y = hypertension_prev)) +
  labs(x = "IMD Decile (1 is Most Deprived)", 
       y = "Age Standardised Hypertension Prevalence") +
  scale_y_continuous(expand = c(0,0), breaks = c(0,3,6,9,12,15), limits = c(0,15)) +
  theme(axis.text = element_text(family = "Times New Roman", size = 10), 
        axis.title = element_text(family = "Times New Roman", face = "bold", size = 10),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.position = "none") 
ggsave("Relationship between IMD and Hypertension Prevalence.jpeg", 
       plot = hyp_prev_by_imd, 
       width = 1920, height = 1080, units = "px", dpi = 320, scale = 2)


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
missed_diagnoses_imd <- ggplot(missed_cvd_by_imd_decile, aes(x = imd_decile, fill = imd_decile)) +
  scale_fill_manual(values = c("#fcdadb", "#f5c2c3", "#efaaaa", "#e89192", "#e2797a", "#db6161", "#d54949", "#ce3031", "#c81818", "#c10000")) + 
  geom_col(aes(y = missed_diagnoses)) +
  labs(x = "IMD Decile (10 is Most Deprived)", 
       y = "Missed Hypertension Diagnoses") +
  theme(legend.position =  "none", 
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(family = "Times New Roman", size = 10), 
        axis.title = element_text(family = "Times New Roman", face = "bold", size = 10))
ggsave("Relationship between IMD and Missed Diagnoses.jpeg", plot = missed_diagnoses_imd, 
       width = 1920, height = 1080, units = "px", dpi = 320, scale = 2)

ggplot(missed_cvd_by_imd_decile, aes(x = imd_decile, y = missed_diagnoses, group = 1)) +
  geom_line(lwd = 3, col = '#00a3c7') +
  geom_point(size = 5, col = "#002f5f") +
  geom_text(aes(label = missed_diagnoses), hjust = -0.5) +
  labs(x = 'IMD Decile (10 is Most Deprived)', y = 'Missed Diagnoses') + 
  theme(panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 18))

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
  labs(y = 'Prevented Cases of Stroke') + 
  theme(panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 10), 
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

# Costs by ICS 
ics_total_costs <- ics_treatment_data[order(ics_treatment_data$costs_saved, decreasing = T), ]

ics_total_costs$sub_icb_loc_name <- factor(ics_total_costs$sub_icb_loc_name, 
                                    levels = ics_total_costs$sub_icb_loc_name)
# Creating Cumulative Sum of Costs Saved
ics_total_costs$cumulative <- cumsum(ics_total_costs$costs_saved) 
# Converting Cumulative Sum to a Percentage
ics_total_costs$cumulative <- 100 * ics_total_costs$cumulative/tail(ics_total_costs$cumulative, n = 1)
# Adjusting Right Hand Scale
scaleRight_Totalcosts <- tail(ics_total_costs$cumulative, n = 1)/head(ics_total_costs$costs_saved, n = 1)

ggplot(ics_total_costs, aes(x = sub_icb_loc_name)) +
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

################# Waterfall Plots - Excess Strokes #####################
### National Plot
undiagnosed_strokes <- waterfall(values = undiagnosed_hypertension_decile$excess_stroke, 
          labels = undiagnosed_hypertension_decile$ntile, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, 
          fill_colours = c("#dcfffd", "#c3f0fa", "#ade1f6", "#95d1f2", "#8bbee0"),
          rect_text_size = 1.5) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(x = "Undiagnosed Hypertension Quintile", y = "Excess Strokes") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total")) +
  scale_y_continuous(expand = c(0,0))
ggsave("Undiagnosed Strokes by Quintile.jpeg", undiagnosed_strokes, width = 1920, height = 1080, dpi = 320, units = "px")


### Waterfall for Missed Diagnoses only 
missed_waterfall <- waterfall(values = missed_cvd_events_quintile$missed_prop, 
          labels = missed_cvd_events_quintile$quintile, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, theme_text_family = "serif",
          fill_colours = c("white", "#c9e6ee", "#95d1e3", "#5ebfd8", "#00a3c7"), rect_text_size = 1) +
  scale_y_continuous(expand = c(0,0), labels = function(y) format(y, scientific = FALSE)) +
  theme(axis.text = element_text(family = "serif", size = 10), 
        axis.title = element_text(family = "serif", face = "bold", size = 10),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(x = "LSOA Quintiles by Number of Missed Diagnoses", y = "% of Total Missed Hypertension Diagnoses") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

library(ggpubr)
g <- ggarrange(missed_by_quintile_map, missed_waterfall, ncol = 2,  widths = c(5, 4))
g

grid.arrange(missed_by_quintile_map, missed_waterfall, ncol = 2)

ggsave("Missed Hypertension Infographic.pdf", g, width = 2048, height = 864, units = "px", dpi = 320, 
       scale = 2)


missed_strokes <- waterfall(values = missed_cvd_events_quintile$excess_stroke, 
          labels = missed_cvd_events_quintile$quintile, calc_total = T,
          total_rect_color = "#002f5f",
          fill_by_sign = FALSE, 
          fill_colours = c("white", "#c9e6ee", "#95d1e3", "#5ebfd8", "#00a3c7"), rect_text_size = 1.5) +
  scale_y_continuous(expand = c(0,0), labels = function(y) format(y, scientific = FALSE)) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(x = "Missed Diagnoses by LSOA Quintile", y = "Excess Strokes") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

ggsave("Missed Strokes by Quintile.jpeg", plot = missed_strokes, width = 1920, height = 1080, units = "px", 
       dpi = 320)

######################### Waterfall Plots - Excess MI ##########################
### National
undiagnosed_mi <- waterfall(values = undiagnosed_hypertension_decile$excess_mi, 
          labels = undiagnosed_hypertension_decile$ntile, calc_total = T,
          total_rect_color = "#840034",
          fill_by_sign = FALSE, 
          fill_colours = c("#fde5e2", "#fbcdca", "#f9b6b5", "#f6a0a0", "#b9787f"), 
          rect_text_size = 1.5) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(x = "Undiagnosed Hypertension Quintile", y = "Excess MI") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total")) + 
  scale_y_continuous(expand = c(0,0))

ggsave("Undiagnosed MI by Quintile.jpeg", plot = undiagnosed_mi, width = 1920, height = 1080, units = "px", 
       dpi = 320)

# For Missed Diagnoses Only
missed_mi <- waterfall(values = missed_cvd_events_quintile$excess_mi, 
          labels = missed_cvd_events_quintile$quintile, calc_total = T,
          total_rect_color = "#840034",
          fill_by_sign = FALSE, 
          fill_colours = c("#fde5e2", "#fbcdca", "#f9b6b5", "#f6a0a0", "#b9787f"), 
          rect_text_size = 1.5) +
  scale_y_continuous(expand = c(0,0), labels = function(y) format(y, scientific = FALSE)) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14),
        panel.background = element_blank(), 
        panel.grid.major =  element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(x = "Missed Hypertension Diagnoses Quintile", 
       y = "Excess MI") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", 
                              "Quintile 5", "Total"))

ggsave("Missed MI by Quintile.jpeg", plot= missed_mi, width = 1920, height = 1080, units = "px", dpi = 320)

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

# export
write_csv(lsoa_treatment_ranked, "LSOA Treatment Indicators Ranked.csv")
write_csv(ics_treatment_ranked, "ICS Treatment Indicators Ranked.csv")
