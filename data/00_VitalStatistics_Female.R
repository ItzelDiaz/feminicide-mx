############################################################
# 00_VitalStatistics_Female
# Feminicide and Female Life Expectancy in Mexico
# Author: Itzel Díaz-Juárez; Víctor M. Garcia-Guerrero
# 
# Description:
# Selection if variables in vital statistics
#
############################################################

# ----------------------------------------------------------
# 0. Setup
# ----------------------------------------------------------
rm(list=ls())
setwd("C:/Users/ditze/OneDrive - El Colegio de México A.C/ARTÍCULOS/1_Descriptive/FINALES")

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, dplyr, tidyr, readr, openxlsx, ggplot2, haven,
  car, expss,  R.utils, gridExtra, wesanderson, janitor, tidyverse, here
)


##----1) Loading data ---------
#Loading vital statistics of female deaths
load("deathsVS_00_20F.RData")

names(deathsVS_00_20F)
table(deathsVS_00_20F$ANIO_REGIS)


#----Filter of database----
deathsVS_00_19F <- deathsVS_00_20F %>% 
  filter(ANIO_OCUR < 2020) %>% 
  select(c(1:16,19:41))

save(deathsVS_00_19F, file = "deathsVS_00_19F.RData")
