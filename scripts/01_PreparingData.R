############################################################
# 01_prepare_data.R
# Feminicide and Female Life Expectancy in Mexico
# Author: Itzel Díaz-Juárez; Víctor M. Garcia-Guerrero
# 
# Description:
# Prepares mortality data and conciliation to estimate feminicide proportions and build cause-deleted life tables.
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
rm(list=ls())
setwd("")

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, dplyr, tidyr, readr, openxlsx, ggplot2, haven,
  car, expss,  R.utils, gridExtra, wesanderson, janitor, tidyverse, here
)


##----1) Loading data ---------
#Loading conciliación demográfica to apply proportions (deaths from 2000 to 2019)
load("conciliacion2020.RData")
# Keep only the relevant objects
defs.edos <- get("defs.edos")
pob.edos.mid20 <- get("pob.edos.mid20")
# Remove all other objects from the environment to avoid clutter
rm(list = setdiff(ls(), c("defs.edos", "pob.edos.mid20")))

#Loading vital statistics of female deaths
load("deathsVS_00_19F.RData")


#Functions
#Function to group five-year ages
group_age <- function(df, age_var) {
  df <- df %>%
    mutate(g_age = case_when(
      {{ age_var }} >= 0 & {{ age_var }} < 1 ~ "0",
      {{ age_var }} >= 1 & {{ age_var }} < 5 ~ "1 a 4",
      {{ age_var }} >= 5 & {{ age_var }} < 10 ~ "5 a 9",
      {{ age_var }} >= 10 & {{ age_var }} < 15 ~ "10 a 14",
      {{ age_var }} >= 15 & {{ age_var }} < 20 ~ "15 a 19",
      {{ age_var }} >= 20 & {{ age_var }} < 25 ~ "20 a 24",
      {{ age_var }} >= 25 & {{ age_var }} < 30 ~ "25 a 29",
      {{ age_var }} >= 30 & {{ age_var }} < 35 ~ "30 a 34",
      {{ age_var }} >= 35 & {{ age_var }} < 40 ~ "35 a 39",
      {{ age_var }} >= 40 & {{ age_var }} < 45 ~ "40 a 44",
      {{ age_var }} >= 45 & {{ age_var }} < 50 ~ "45 a 49",
      {{ age_var }} >= 50 & {{ age_var }} < 55 ~ "50 a 54",
      {{ age_var }} >= 55 & {{ age_var }} < 60 ~ "55 a 59",
      {{ age_var }} >= 60 & {{ age_var }} < 65 ~ "60 a 64",
      {{ age_var }} >= 65 & {{ age_var }} < 70 ~ "65 a 69",
      {{ age_var }} >= 70 & {{ age_var }} < 75 ~ "70 a 74",
      {{ age_var }} >= 75 & {{ age_var }} < 80 ~ "75 a 79",
      {{ age_var }} >= 80 & {{ age_var }} < 85 ~ "80 a 84",
      {{ age_var }} >= 85 ~ "85 y más"
    ), .after = {{ age_var }})
  
  df$g_age <- factor(df$g_age, levels = c("0", "1 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24", "25 a 29", "30 a 34", "35 a 39", "40 a 44", "45 a 49", "50 a 54", "55 a 59", "60 a 64", "65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 y más"))
  
  return(df)
}
##-Recodifying ages

deathsVS_00_19F <- deathsVS_00_19F %>% 
  mutate(age = case_when(
    between(EDAD, 1001, 3098) ~ 0,
    EDAD == 4001 ~ 1,
    between(EDAD, 4002, 4109) ~ EDAD - 4000,
    EDAD %in% 4110:4120 ~ 110
  )) %>% 
  mutate(ENT_RESID = as.numeric(ENT_RESID))

##------2) Preparing databases for Life table with deleted cause----------
##------2.1 Selecting causes-----
causes_cie <- as.character(sort(unique(deathsVS_00_19F$CAUSA_DEF)))

##Selected agressions (homicides)
agressions_f <- causes_cie[causes_cie >= "X850" & causes_cie <= "Y099"]
#
#It is decided not to take into account for approximation: X96 Aggression with explosive material 
agressions_f <- agressions_f[!grepl("X96[0-9]$", agressions_f)]

#Accident or undefined cause that occurred at household
accident_indet <- c("W100", "W200", "W320", "W330", "W340", "W500", "X100", "Y220", "Y230", "Y240", "Y260", "Y280")

#Possible agression at not specified location (vector of causes)
agression_nslocv <- causes_cie[causes_cie %in% c("W109", "W209", "W329", "W339", "W349", "W509", "X109", "Y229", "Y239", "Y249", "Y269", "Y289", "Y871")]


#Deaths from vital statistics with grouped causes
deathsVS_00_19F <- deathsVS_00_19F %>% 
  mutate(summ_causes = case_when(
    (CAUSA_DEF %in% c(accident_indet, agressions_f)) | (CAUSA_DEF %in% agression_nslocv & SITIO_OCUR==3) ~ "feminicidio",
    #TRUE ~ CAUSA_DEF  #Toma el mismo valor que CAUSA_DEF si no cumple ninguna de las condiciones anteriores
    TRUE ~ "otras causas" 
  )) 

table(deathsVS_00_19F$summ_causes, useNA = "ifany")


#Applying function to group ages
deathsVS_00_19F_ <- group_age(deathsVS_00_19F, age)
table(deathsVS_00_19F_$age, deathsVS_00_19F_$g_age, useNA = "ifany")

#----2.2) Calculate proportion of deaths by cause for each state, age and year--------
prop_cause_year_staF <- deathsVS_00_19F_ %>%
  filter(ENT_RESID %in% c(1:32)) %>% 
  group_by(ENT_RESID, ANIO_OCUR, g_age) %>%
  arrange(ENT_RESID, ANIO_OCUR, g_age) %>%
  mutate(total_deaths = n()) %>%
  group_by(summ_causes, add = TRUE) %>%
  summarise(proportion = n() / first(total_deaths)) %>%
  ungroup() %>%
  group_split(ENT_RESID)

prop_cause_year_staF <- prop_cause_year_staF %>%
  bind_rows() %>%  # combine all tibbles in the list below
  mutate(ENT_RESID = as.numeric(ENT_RESID))


#----2.3) Preparing conciliation data--------
#
##DATA CONCILIATION FEMALES: deaths by group of causes
deaths_stconc00_20F <- defs.edos %>%  
  filter(year %in% c(2000:2019) & sex=="Mujeres") %>% 
  mutate(ent_num = match(edo, unique(edo)), .after = "sex") %>% 
  type.convert() 

#Applying function to group ages (deaths from 2000 to 2019)
deaths_stconc00_20F <- group_age(deaths_stconc00_20F, age) %>% 
  group_by(g_age, edo, ent_num,year) %>% 
  summarise(sumdeaths = sum(defs)) %>% 
  ungroup()


#Applying proportions computed with vital statistics 
deaths_conc_propF <- deaths_stconc00_20F %>% 
  full_join(prop_cause_year_staF, by = c("ent_num" = "ENT_RESID", "year" = "ANIO_OCUR", "g_age" = "g_age")) %>% 
  mutate(deaths_summ = ifelse(is.na(proportion), 0, round(sumdeaths * proportion,0)))

#save(deaths_conc_propF, file = "deaths_conc_propF.RData")

#----2.4) Databases population at mid-year & total deaths from conciliacion --------

#Population at mid-year from 2000 to 2019
pop_mid00_20F <- pob.edos.mid20 %>% 
  filter(sex == "Mujeres" & year >=2000) %>% 
  type.convert()

pop_mid00_20F <- group_age(pop_mid00_20F, age) %>% #Grouping ages
  group_by(edo, year, g_age) %>% 
  summarise(pop_q = sum(pob)) %>% 
  ungroup()


#save(pop_mid00_20F, file = "pop_mid00_20F.RData")

#Database with total deaths from conciliacion
deaths_stconc00_20F



##-----2.5) Databases for lifetables----
#
#Creating database of deaths with population at mid-year
base_lt <- deaths_stconc00_20F %>% 
  full_join(pop_mid00_20F, by = c("edo" = "edo", "year" = "year", "g_age" = "g_age")) %>% 
  mutate(nmx = sumdeaths/pop_q)  %>% #Computing nmx
  group_by(edo, year, g_age) %>% 
  arrange(edo, year, g_age) %>% 
  ungroup() %>% 
  select(g_age, edo, ent_num, year, sumdeaths, pop_q, nmx) %>% 
  filter(!is.na(sumdeaths)) %>% 
  as_tibble()

#Saving database with general nmx
#save(base_lt, file = "base_lt.RData")

#----2.6) Total central death rates for every state, age and year-----
#mx_st is a list of tibbles for every state with the central death rates from 2000 to 2020 (all causes)
mx_st <- base_lt %>% 
  select(g_age, edo, year,nmx) %>% 
  arrange(edo,g_age,year) %>% 
  group_by(edo, g_age) %>% 
  pivot_wider(names_from = year, values_from = nmx) %>% 
  ungroup() %>% 
  type.convert() %>% 
  split(.$edo) 

mx_st <- mx_st %>%
  map(~ mutate(.x, across(`2000`:`2019`, ~ unlist(.))))

#----2.7) Computing Ri due to feminicide------

Ri_fem <- deaths_conc_propF %>% 
  group_by(edo, ent_num, year, g_age) %>% 
  filter(summ_causes == "feminicidio" & sumdeaths>0) %>% 
  mutate(Ri = (sumdeaths-deaths_summ)/sumdeaths) %>% 
  arrange(edo, g_age, year, Ri) %>% 
  # slice_max(Ri) %>% 
  ungroup() %>% 
  select(g_age, edo, year, Ri)


combs <- expand_grid(
  g_age = c("0", "1 a 4", "5 a 9", "10 a 14",  "15 a 19", "20 a 24", "25 a 29", "30 a 34", "35 a 39", "40 a 44", "45 a 49", "50 a 54", "55 a 59", "60 a 64", "65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 y más" ),
  edo = unique(Ri_fem$edo),
  year = 2000:2020
)

#Joining Ri and tibble with combinations
#It works because Ri do not have feminicides in each combination
Ri_fem <- combs %>%
  left_join(Ri_fem, by = c("g_age", "edo", "year")) %>%
  replace(is.na(.), 1) # NA == 1 cause it means that there are 0 feminicides: Ri= (total deaths - 0)/total deaths = 1
Ri_fem$g_age <- factor(Ri_fem$g_age, levels = c("0", "1 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24", "25 a 29", "30 a 34", "35 a 39", "40 a 44", "45 a 49", "50 a 54", "55 a 59", "60 a 64", "65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 y más"))



#Creating a list in which each element is a tibble for every state (Ri)
Ri_fem <- Ri_fem %>% 
  arrange(edo,g_age,year) %>% 
  group_by(edo, g_age) %>% 
  pivot_wider(names_from = year, values_from = Ri) %>% 
  ungroup() %>% 
  type.convert() %>% 
  split(.$edo) 


save(Ri_fem, file = "Ri_fem.RData")

#-----*Test: feminicides in reproductive ages*--------
#Feminicide database to verify totals and proportions in reproductive ages
feminicidiodb <- base_lt %>% 
  select(g_age, edo, ent_num, year,pop_q) %>% 
  arrange(edo,g_age,year) %>% 
  left_join(deaths_conc_propF, by = c("g_age", "edo", "ent_num", "year")) %>% 
  filter(summ_causes == "feminicidio" & sumdeaths>0) %>%
  select(g_age, edo, ent_num, year, pop_q, deaths_summ) %>% 
  mutate(nmxfem = deaths_summ/pop_q) %>% 
  select(-ent_num) %>% 
  right_join(combs, by = c("g_age", "edo", "year")) %>%
  replace(is.na(.), 0) %>% 
  arrange(edo,year, g_age,) 

#Reproductive ages vector
repr_ages <- c("15 a 19", "20 a 24", "25 a 29", "30 a 34", "35 a 39", "40 a 44", "45 a 49")

#Sum of feminicides of all ages
sumfems_allages <- sum(feminicidiodb$deaths_summ) 
sumfems_reproduct <- sum(feminicidiodb$deaths_summ[feminicidiodb$g_age %in% repr_ages]) 

#For each state
feminicidiodb <- feminicidiodb %>% 
  group_by(edo, year) %>% 
  mutate(prop_rep = sum(deaths_summ[g_age %in% repr_ages])/sum(deaths_summ)) %>% 
  ungroup()

#Cases with less than 80% of feminicides in reproductive ages
prop_repr <- feminicidiodb %>% 
  group_by(edo, year) %>% 
  select(edo, year, prop_rep) %>% 
  slice_head()
#Of the 672 cases being analyzed (32 states and 21 years),  489 are cases in which less than 80% of the victims were of reproductive age; 9 cases have all femicides in reproductive age.

#-----* Computing mx due to feminicide ------
#Joining with population
mxfem <- base_lt %>% 
  select(g_age, edo, ent_num, year, pop_q) %>% 
  arrange(edo,g_age,year) %>% 
  left_join(deaths_conc_propF, by = c("g_age", "edo", "ent_num", "year")) %>% 
  filter(summ_causes == "feminicidio" & sumdeaths>0) %>%
  select(g_age, edo, ent_num, year, pop_q, deaths_summ) %>% 
  mutate(nmxfem = deaths_summ/pop_q) %>% 
  select(-c(ent_num, pop_q, deaths_summ))
mxfem$g_age <- factor(mxfem$g_age, levels = c("0", "1 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24", "25 a 29", "30 a 34", "35 a 39", "40 a 44", "45 a 49", "50 a 54", "55 a 59", "60 a 64", "65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 y más"))
combs$g_age <- factor(combs$g_age, levels = c("0", "1 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24", "25 a 29", "30 a 34", "35 a 39", "40 a 44", "45 a 49", "50 a 54", "55 a 59", "60 a 64", "65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 y más"))



mxfem <- combs %>%
  left_join(mxfem, by = c("g_age", "edo", "year")) %>%
  replace(is.na(.), 0.000001) %>% 
  arrange(edo,g_age,year) %>% 
  group_by(edo, g_age) %>% 
  pivot_wider(names_from = year, values_from = nmxfem) %>% 
  ungroup() %>% 
  type.convert() %>% 
  split(.$edo) 


#-----3) Saving databases for use in cause-deleted lifetables---- 

save(deathsVS_00_19F_, "deathsVS_00_19F_.RData")
save(mx_st, file = "mx_st.RData")
save(mxfem, file = "mxfem_st.RData")
save(Ri_fem, file ="Ri_fem.RData")

# ----------------------------------------------------------
# End of script
# ----------------------------------------------------------
