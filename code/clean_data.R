# *********************************************
# File description ----
# *********************************************
# Create basic dataset 
# Begin with the full year consolidated data - merge across years and panels
# Input: TBD
# Output: TBD

# Author: Ori
# Date: 16/04/2024

# *********************************************
# Setup ----
# *********************************************

rm(list = ls())
library(tidyverse) 
library(data.table)
library(haven)
library(glue)

# *********************************************
# TBD ----
# *********************************************

# helper table - each panels years
panel_years <- data.table(year = rep(2008:2019, each = 2),
                          panel_year = c(2,1),
                          PANEL = c(12, rep(13:23, each = 2), 24))


get_panel_year <- function(panel, .year){
  panel_years %>% 
    filter(PANEL == panel, year == .year) %>% 
    pull(panel_year)
}

# a function to read a FYC for a certain year, add year and panel_year variables and
# save a separate data frame in the working environment for each panel
read_FYC <- function(.year){
  dat <- fread(glue("raw_data/FullYearConsolidated{.year}.csv"))
  dat <- dat %>% 
    mutate(year = .year) %>% 
    left_join(panel_years)
  panels <- unique(dat$PANEL)
  walk(panels, \(x) assign(glue("FYC{.year}_pan{x}_y{get_panel_year(x, .year)}"), dat %>% filter(PANEL == x), envir = .GlobalEnv))
}

# Read all Full Year Consolidated data (2008-2019, panels 13-23)
walk(2008:2019, read_FYC, .progress = T)
rm(list = c("FYC2008_pan12_y2", "FYC2019_pan24_y1"))
gc()

# function to rename all variables in term of rounds and years
rename_vars <- function(dat_name){
  dat <- get(dat_name)
  nums <- str_split_1(dat_name, "[^[:digit:]]+") %>% 
    as.numeric()
  year <- nums[2]
  panel <- nums[3]
  panel_year <- nums[4]
  year_short <- str_sub(year, 3,4)
  if(panel_year == 1){
    dat <- dat %>% 
      rename_with(
        ~str_replace_all(.x, c("(?<![:digit:])31"= "_r1", "42" = "_r2", "53" = "_r3")))
  }else{
    dat <- dat %>% 
      rename_with(
        ~str_replace_all(.x, c("(?<![:digit:])31"= "_r3", "42" = "_r4", "53" = "_r5")))
  }
  
  dat <- dat %>% 
    rename_with(~str_replace_all(.x, year_short, glue("_y{panel_year}"))) %>% 
    rename_with(~str_replace_all(.x, "1231", glue("1231_y{panel_year}")))
  assign(dat_name, dat, envir = .GlobalEnv)
}

walk(ls(pattern = "^FYC?"), rename_vars)
gc()
