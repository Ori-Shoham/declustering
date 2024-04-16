# *********************************************
# File description ----
# *********************************************
# Download MEPS raw data
# Input: None
# Output: Saves MEPS data for years 2008-2022 in  raw_data folder, includes the following types of files:
#  - Full Year Consolidated (main data)
#  - Conditions
#  - Event based:
#     - Prescription Medicines
#     - Dental
#     - Inpatient
#     - ER
#     - Outpatient
#     - Office Based
#     - Home Health
#  - Medicine linking file
#  - Conditions linking file

# Author: Ori
# Date: 09/04/2024

# *********************************************
# Setup ----
# *********************************************

rm(list = ls())
library(tidyverse) 
library(data.table)
library(haven)
# devtools::install_github("e-mitchell/meps_r_pkg/MEPS")
library(MEPS)
library(glue)

# *********************************************
# Data Download ----
# *********************************************

save_data <- function(year, dat_type){
  temp <- read_MEPS(year = year, type = dat_type)
  dat_type <- case_when(dat_type == "FYC" ~ "FullYearConsolidated",
                        dat_type == "PMED" ~ "PrescriptionsMedicines",
                        dat_type == "Office_based" ~ "OfficeBased",
                        dat_type == "Home_Health" ~ "HomeHealth",
                        dat_type == "CLNK" ~ "ConditionsLinkFile",
                        dat_type == "RXLK" ~ "MedicinesLinkFile",
                        T ~ dat_type)
  fwrite(temp, glue("raw_data/{dat_type}{year}.csv"))
  rm(temp)
  gc()
}

combs <- expand_grid(year = 2008:2022, dat_type = c("FYC", "Conditions", "PMED", "Dental", "Inpatient",
                                                    "ER", "Outpatient", "Office_based", "Home_Health",
                                                    "CLNK", "RXLK"))

walk2(combs$year, combs$dat_type, save_data, .progress = T)
