# Feminicide and Female Survival in Mexico

This repository contains the R code used in the paper *“Feminicide as a Determinant of Mexican Female Life Expectancy in the 21st Century”*.  
The study develops an **operational definition of feminicide** based on selected external causes of death and evaluates its demographic impact on women’s life expectancy in Mexico between 2000 and 2019.

## Repository structure

- **`scripts/`**  
  Contains the R scripts for data preparation, demographic analysis, and complementary integration with external sources:
  - `01_PreparingData.R`: Loads and cleans datasets (vital statistics and demographic conciliation). Prepares databases for inputs in cause-deleted lifetables.  
  - `02_Cause-deleted LT.R`: Applies proportions, builds cause-deleted and multiple decrement life tables.  
  - `03_BANAVIM.R`: Descriptive analysis of BANAVIM (data not included in this repository).  

- **`data/`**  
  Contains datasets used in the project.
   
  - `conciliacion2020`: CONAPO Demographic Conciliation.
  - `deathsVS_00_19F`: INEGI Vital Statistics, Cleaned for selection of external causes.  
  - ⚠️ **Note**: BANAVIM (National Database on Cases of Violence against Women) is not included due to access restrictions.  

## Data availability

- **Vital Statistics (INEGI)**: Female deaths by age, year, and cause of death (2000–2019).  
- **Demographic Conciliation (CONAPO)**: Population and deaths by state, age, sex, and year.  
