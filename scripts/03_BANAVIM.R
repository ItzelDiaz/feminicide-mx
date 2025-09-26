############################################################
# 03_BANAVIM.R
# Feminicide and Female Life Expectancy in Mexico
# Author: Itzel Díaz-Juárez; Víctor M. Garcia-Guerrero
# 
# Description:
# Descriptive data from BANAVIM
#
# Methodological basis:
# - Selection of external causes of death (Grushka & Kohan, 2020; Kohan, 2018)
# - Integration with Demographic Conciliation (INEGI, CONAPO)
# - Cause-deleted life tables (Chiang, 1968)
# - Multiple decrement life tables (Preston et al., 2001)
############################################################


# ----------------------------------------------------------
# 0. Setup
# ----------------------------------------------------------

rm(list=ls())   # Clear the environment (remove all objects from memory)
setwd("C:/Users/ditze/OneDrive - El Colegio de México A.C/ARTÍCULOS/1_Descriptive/FINALES")  # Set working directory

# Load pacman if not installed, then load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, foreign, ggplot2, readr, janitor, dplyr, haven, car, expss, janitor, tidyr, tidyverse, survey, lubridate, R.utils, minpack.lm, gridExtra, Hmisc, wesanderson, openxlsx)

#-----1) Load the dataset------
load("fem_bnvmcc.RData")   # Loads the BANAVIM feminicide dataset


fem_bnvmcc <- fem_bnvmcc %>%
  rename(
    state_h = estado_h,                       # State where the homicide occurred
    victim_age = edad_v,                      # Victim's age
    classified_place = lugar_clasificado,     # Classified place (where body was found)
    event_space = espacio_hecho,              # Type of space (public/private)
    relation_to = relacion_a,                 # Relation to perpetrator
    year_occur = year_ocur                    # Year of occurrence
  )

#-----Function to group ages------
group_age <- function(df, age_var) {
  df <- df %>%
    mutate(g_age = case_when(
      {{ age_var }} >= 0 & {{ age_var }} < 1 ~ 0,
      {{ age_var }} >= 1 & {{ age_var }} < 5 ~ 1,
      {{ age_var }} >= 5 & {{ age_var }} < 10 ~ 5,
      {{ age_var }} >= 10 & {{ age_var }} < 15 ~ 10,
      {{ age_var }} >= 15 & {{ age_var }} < 20 ~ 15,
      {{ age_var }} >= 20 & {{ age_var }} < 25 ~ 20,
      {{ age_var }} >= 25 & {{ age_var }} < 30 ~ 25, 
      {{ age_var }} >= 30 & {{ age_var }} < 35 ~ 30,
      {{ age_var }} >= 35 & {{ age_var }} < 40 ~ 35,
      {{ age_var }} >= 40 & {{ age_var }} < 45 ~ 40,
      {{ age_var }} >= 45 & {{ age_var }} < 50 ~ 45,
      {{ age_var }} >= 50 & {{ age_var }} < 55 ~ 50,
      {{ age_var }} >= 55 & {{ age_var }} < 60 ~ 55,
      {{ age_var }} >= 60 & {{ age_var }} < 65 ~ 60,
      {{ age_var }} >= 65 & {{ age_var }} < 70 ~ 65,
      {{ age_var }} >= 70 & {{ age_var }} < 75 ~ 70,
      {{ age_var }} >= 75 & {{ age_var }} < 80 ~ 75,
      {{ age_var }} >= 80 & {{ age_var }} < 85 ~ 80,
      {{ age_var }} >= 85 ~ 85
    ), .after = {{ age_var }})
  
  df$g_age <- as.integer(df$g_age)  # Ensure grouped ages are integers
  return(df)
}

# ------ 2)Database preparation----------------------
names(fem_bnvmcc)   

# Select relevant variables, order by state and age, filter by age 15–64, 
# and convert to appropriate data types
feminicida1 <- fem_bnvmcc %>% 
  arrange(state_h, victim_age) %>% 
  select(-c(3, 7, 23,28,30:32)) %>%   # Drop unnecessary variables
  filter(victim_age %in% c(15:64)) %>% 
  type_convert()

# Apply age grouping function
feminicida1 <- group_age(feminicida1, victim_age)

tab_feminicida1 <- lapply(feminicida1, function(x) table(x, useNA = "ifany"))
names(feminicida1)
tab_fembnvmcc <- lapply(fem_bnvmcc, function(x) table(x, useNA = "ifany"))

# Cross-tabulations between age groups, location, and victim-offender relationship
table(feminicida1$g_age, feminicida1$classified_place, useNA = "ifany") 
table(feminicida1$event_space, feminicida1$classified_place, useNA = "ifany") 
table(feminicida1$event_space, feminicida1$relation_to, useNA = "ifany") 

# Feminicides in public spaces
# Count cases by age group and classified place, then compute proportions
place_age <- feminicida1 %>% 
  filter(event_space == "espacio publico") %>% 
  select(g_age, classified_place) %>% 
  group_by(g_age, classified_place) %>%  
  count() %>%
  group_by(g_age) %>%
  mutate(proportion = n/sum(n) * 100) 

# Places where the body was found, by age group
place_age_plot <- ggplot(place_age, aes(x = factor(g_age), y = proportion, fill = classified_place)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = sprintf("%.1f%%", proportion)), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white", fontface = "bold", show.legend = FALSE) +  
  labs(title = "Places where the body was found by age group",
       subtitle = "Feminicides in public spaces",
       x = "Age group",
       y = "Proportion",
       fill = "Place") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("alcantarilla/canal/río" = "#9E7BB5",
                               "baldío/barranca/sembradío/basurero" = "#CAB2D6",
                               "bar/restaurante/hotel" = "#7E5189",
                               "calle/carretera/vía pública" = "#8C6BB1",
                               "hospital/fosa/panteón" = "#5E3B65",
                               "otro" = "#D5C0D9",
                               "NA" = "#BDBDBD"),
                    labels = c("alcantarilla/canal/río" = "Sewer/canal/river",
                               "baldío/barranca/sembradío/basurero" = "Wasteland/ravine/field/dump",
                               "bar/restaurante/hotel" = "Bar/restaurant/hotel",
                               "calle/carretera/vía pública" = "Street/highway/public road",
                               "hospital/fosa/panteón" = "Hospital/graveyard/cemetery",
                               "otro" = "Other",
                               "NA" = "Missing")) +  
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),           
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        panel.grid.major.x = element_blank()) 

place_age_plot  

#------3)Feminicides by year and age group------
# Aggregate the number of feminicides by year of occurrence and age group
feminicides_year <- feminicida1 %>%
  group_by(year_occur, g_age) %>%
  summarise(total_fem = n()) %>%
  ungroup()

# Stacked area chart: feminicides by age and year (2010–2020)
cum_fem_bnvm <- ggplot(feminicides_year, aes(x = g_age, y = total_fem, fill = factor(year_occur, levels = rev(unique(year_occur))))) +
  geom_area(position = "stack", alpha = 0.8, size = 0.7) + 
  geom_vline(xintercept = 20, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 40, linetype = "dashed", color = "gray40") +  
  scale_fill_viridis_d(option = "magma", direction = 1, begin = 0.1, end = 0.9, guide = guide_legend(reverse = TRUE)) +
  labs(
    title = "Feminicides by age group and year of occurrence (2010-2020)",
    x = "Age group",
    y = "Number of feminicides",
    fill = "Year"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
    axis.text.x = element_text(size = 10),  
    axis.text.y = element_text(size = 10),  
    legend.box = "horizontal",
    legend.box.spacing = unit(0.1, "cm")
  ) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 85, by = 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 130, by = 25), expand = c(0, 0)) 

cum_fem_bnvm  

# ggsave("cum_fem_bnvm.jpg", cum_fem_bnvm, width = 24, height = 18, units = "cm", dpi = 300)
