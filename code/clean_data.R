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

  panels <- unique(dat$PANEL)
  walk(panels, \(x) assign(glue("FYC{.year}_pan{x}_y{get_panel_year(x, .year)}"), dat %>% filter(PANEL == x), envir = .GlobalEnv))
}

# Read all Full Year Consolidated data (2008-2019, panels 13-23)
walk(2008:2019, read_FYC, .progress = T)
rm(list = c("FYC2008_pan12_y2", "FYC2019_pan24_y1"))
gc()

# function to rename all variables in term of rounds and years
rename_timed_vars <- function(dat_name){
  dat <- get(dat_name)
  nums <- str_split_1(dat_name, "[^[:digit:]]+") %>% 
    as.numeric()
  year <- nums[2]
  panel <- nums[3]
  panel_year <- nums[4]
  year_short <- str_sub(year, 3,4)
  # define round variables
  if(panel_year == 1){
    dat <- dat %>% 
      rename_with(
        ~str_replace_all(.x, c("(?<![:digit:])31"= "_r1", "42" = "_r2", "53" = "_r3")))
  }else{
    dat <- dat %>% 
      rename_with(
        ~str_replace_all(.x, c("(?<![:digit:])31"= "_r3", "42" = "_r4", "53" = "_r5")))
  }
  # add year suffix to yearly or end of year variables
  dat <- dat %>% 
    rename_with(~str_replace_all(.x, glue("{year_short}(?!31)"), glue("_y{panel_year}"))) %>% 
    rename_with(~str_replace_all(.x, "1231", glue("1231_y{panel_year}")))
  # add year suffix to other years (e.g. for health status variables)
  year_before <- str_sub(year - 1, 3,4)
  year_after  <- str_sub(year + 1, 3,4)
  dat <- dat %>% 
    rename_with(~str_replace_all(.x, glue("{year_before}(?!31)"), glue("_y{panel_year-1}"))) %>% 
    rename_with(~str_replace_all(.x, glue("{year_after}(?!31)"), glue("_y{panel_year+1}"))) 
  assign(dat_name, dat, envir = .GlobalEnv)
}

walk(ls(pattern = "^FYC?"), rename_timed_vars)
gc()

## Rename first years ----

#    *********************************************
#### Panel 13 - base data ----
#    *********************************************

pan_13 <- FYC2008_pan13_y1 %>% 
  # survey administrative and id variables
  rename_with(~glue("surv_ad_{.x}"), c(DUID:FAMRFPYR, REFPRS_r1:RURSLT_r3, CPSFAMID:FCRP1231_y1, MOPID_r1X:DAPID_r3X)) %>% 
  # demographic variables
  rename_with(~glue("dem_{.x}"), c(REGION_r1:MSA_y1, AGE_r1X:AGELAST, DOBMM:RFREL_y1X)) %>% 
  # Income and Tax Filing Variables
  rename_with(~glue("inc_{.x}"), c(SSIDIS_y1:OTHIMP_y1)) %>% 
  # PERSON-LEVEL CONDITION VARIABLES
  rename_with(~glue("cond_{.x}"), c(RTHLTH_r1:ADHDAGED)) %>% 
  # Health Status Variables
  rename_with(~glue("hstat_{.x}"), c(IADLHP_r1:DSPRX_r3)) %>% 
  # DISABILITY DAYS VARIABLES
  rename_with(~glue("dd_{.x}"), c(DDNWRK_r1:OTHNDD_r3)) %>% 
  # ACCESS TO CARE VARIABLES
  rename_with(~glue("acc_{.x}"), c(ACCELI_r2:PMDLPR_r2)) %>% 
  # EMPLOYMENT VARIABLES
  rename_with(~glue("emp_{.x}"), c(EMPST_r1:YNOINS_r3)) %>% 
  # HEALTH INSURANCE VARIABLES
  rename_with(~glue("ins_{.x}"), c(TRIJA_y1X:RTPLNT_r2)) %>% 
  # Expenditure and charging variables
  rename_with(~glue("exp_{.x}"), c(TOTTCH_y1:TOTOSR_y1, OBVTCH_y1:OBVOSR_y1,
                                   OBDTCH_y1:OBDOSR_y1, OBOTCH_y1:OBOOSR_y1,
                                   OBCTCH_y1:OBCOSR_y1, OBNTCH_y1:OBNOSR_y1,
                                   OBETCH_y1:OBEOSR_y1, OBATCH_y1:OBAOSR_y1,
                                   OBTTCH_y1:OBTOSR_y1, OPTTCH_y1:OPTOSR_y1,
                                   OPFTCH_y1:OPFOSR_y1, OPDEXP_y1:OPDOSR_y1,
                                   OPVTCH_y1:OPVOSR_y1, OPSEXP_y1:OPSOSR_y1,
                                   OPOTCH_y1:OPOOSR_y1, OPPEXP_y1:OPPOSR_y1,
                                   ERTTCH_y1:ERTOSR_y1, ERFTCH_y1:ERFOSR_y1,
                                   ERDEXP_y1:ERDOSR_y1, IPTEXP_y1:IPTOSR_y1,
                                   IPFEXP_y1:IPFOSR_y1, IPDEXP_y1:IPDOSR_y1,
                                   ZIFTCH_y1:ZIFOSR_y1, ZIDEXP_y1:ZIDOSR_y1,
                                   DVTTCH_y1:DVTOSR_y1, DVGTCH_y1:DVGOSR_y1,
                                   DVOTCH_y1:DVOOSR_y1, HHATCH_y1:HHAOSR_y1,
                                   HHNTCH_y1:HHNOSR_y1, HHNTCH_y1:HHNOSR_y1,
                                   RXEXP_y1:RXOSR_y1  , AMCTCH_y1:AMCOSR_y1,
                                   AMNTCH_y1:AMNOSR_y1, AMETCH_y1:AMEOSR_y1,
                                   AMATCH_y1:AMAOSR_y1, AMTTCH_y1:AMTOSR_y1,
                                   VISEXP_y1:VISOSR_y1, OTHTCH_y1:OTHOSR_y1,
                                   TOTPTR_y1:TOTOTH_y1,)) %>% 
  # Utilization variables
  rename_with(~glue("util_{.x}"), c(OBTOTV_y1, OBDRV_y1, OBOTHV_y1, OBCHIR_y1,
                                    OBNURS_y1, OBOPTO_y1,  OBASST_y1, OBTHER_y1,
                                    OPTOTV_y1, OPDRV_y1, OPOTHV_y1, ERTOT_y1, IPZERO_y1,
                                    IPDIS_y1, IPNGTD_y1, DVTOT_y1, DVGEN_y1,
                                    DVORTH_y1, HHTOTD_y1, HHAGD_y1, HHINDD_y1, HHINFD_y1,
                                    RXTOT_y1, AMCHIR_y1, AMNURS_y1, AMOPTO_y1,
                                    AMASST_y1, AMTHER_y1, AMTOTC_y1, AMDRC_y1,
                                    OBVPTR_y1:RXOTH_y1)) %>% 
  # Weights variables
  rename_with(~glue("w_{.x}"), c(PERWT_y1F:VARPSU))
  
# verify that all variables where renamed
colnames(pan_13)[str_detect(colnames(pan_13), "^[:upper:]")]
  

# function to rename a var based on categorization of this var in previous panels
rename_one_var_other_panels <- function(var_name, orig_list, new_list){
  for(i in 1:length(orig_list)){
    if(var_name %in% colnames(orig_list[[i]])){
      return(colnames(new_list[[i]])[var_name == colnames(orig_list[[i]])])
    }
  }
    return(var_name)
}  
# function to rename all vars based on categorizations in previous panels

rename_other_panels <- function(var_names, orig_list, new_list){
  map_chr(var_names,\(x) rename_one_var_other_panels(x,orig_list, new_list) )
}

### pan_14 naming ----
pan_14 <- FYC2009_pan14_y1 %>% 
  rename_with(\(x) rename_other_panels(x,list(FYC2008_pan13_y1), list(pan_13)), everything())

# Check which variables did not get modified
colnames(pan_14)[which(str_detect(colnames(pan_14), "^[:upper:]"))]

# manual modification of left vars
pan_14 <- pan_14 %>% 
  rename_with(~glue("hstat_{.x}"),
              c("BSTST_r3", "BSTSRE_r3", "CLNTST_r3",
                "CLNTRE_r3", "SGMTST_r3", "SGMTRE_r3")) %>% 
  rename_with(~glue("acc_{.x}"), c("NOHINS_r2")) %>%
  rename_with(~glue("ins_{.x}"),
              c("MCRPB_r1", "MCRPB_r2", "MCRPB_y1",
                "PMEDPP_r1", "PMEDPP_r2", "PMEDPP_r3"))


### pan_15 naming ----
pan_15 <- FYC2010_pan15_y1 %>% 
  rename_with(\(x) rename_other_panels(x,list(FYC2008_pan13_y1, FYC2009_pan14_y1), list(pan_13, pan_14)), everything())
# Check which variables did not get modified
colnames(pan_15)[which(str_detect(colnames(pan_15), "^[:upper:]"))]

# manual modification of left vars

pan_15 <- pan_15 %>% 
  rename_with(~glue("cond_{.x}"),
              c("CAESOPH", "ESPHAGED", "ESPHREMS"))

### pan_16 naming ----
pan_16 <- FYC2011_pan16_y1 %>% 
  rename_with(
    \(x) rename_other_panels(x,
                             list(FYC2008_pan13_y1, FYC2009_pan14_y1, FYC2010_pan15_y1),
                             list(pan_13, pan_14, pan_15)), everything())
# Check which variables did not get modified
colnames(pan_16)[which(str_detect(colnames(pan_16), "^[:upper:]"))]
# manual modification of left vars
# To be continued

## Rename second years ----

pan_13_y2 <- FYC2009_pan13_y2 %>% 
  rename_with(\(x) glue("surv_ad_{x}"), 1:4) %>% 
  rename_with(\(x) glue("y_exp_{x}"), which(str_detect(colnames(pan_14), "exp"))) %>% 
  rename_with(\(x) glue("y_util_{x}"), which(str_detect(colnames(pan_14), "util"))) %>% 
  select(c(1:4, which(str_detect(colnames(pan_14), "exp|util"))))
  
pan_14_y2 <- FYC2010_pan14_y2 %>% 
  rename_with(\(x) glue("surv_ad_{x}"), 1:4) %>% 
  rename_with(\(x) glue("y_exp_{x}"), which(str_detect(colnames(pan_15), "exp"))) %>% 
  rename_with(\(x) glue("y_util_{x}"), which(str_detect(colnames(pan_15), "util"))) %>% 
  select(c(1:4, which(str_detect(colnames(pan_15), "exp|util"))))
  
pan_15_y2 <- FYC2011_pan15_y2 %>% 
  rename_with(\(x) glue("surv_ad_{x}"), 1:4) %>% 
  rename_with(\(x) glue("y_exp_{x}"), which(str_detect(colnames(pan_16), "exp"))) %>% 
  rename_with(\(x) glue("y_util_{x}"), which(str_detect(colnames(pan_16), "util"))) %>% 
  select(c(1:4, which(str_detect(colnames(pan_16), "exp|util"))))

## Merge data ----

# merge each panel - keep only observations for which there is data in both years
pan_13 <- pan_13 %>% 
  inner_join(pan_13_y2)
pan_14 <- pan_14 %>% 
  inner_join(pan_14_y2)
pan_15 <- pan_15 %>% 
  inner_join(pan_15_y2)

# merge panels - variables that are not present for one of the panels get NA
base_samp_2008_2010 <- bind_rows(pan_13, pan_14, pan_15)

write.csv(base_samp_2008_2010, "datasets/base_samp_2008_2010.csv", row.names = F)




##### Keep adding data later























numeric_vars <- c("RUSIZE","FAMS","FCSZ1231", "AGE", "DOBYY")
dummy_vars <- c("FCRP1231", "FAMRFPYR", "FMRS1231", "MSA")
factor_vars <- c("RUCLAS", "REGION", "INTVLANG", "DOBM", "SEX", "RACE", "HISP")

rename_vars <- function(dat_name){
  dat <- get(dat_name)
  nums <- str_split_1(dat_name, "[^[:digit:]]+") %>% 
    as.numeric()
  year <- nums[2]
  panel <- nums[3]
  panel_year <- nums[4]
  year_short <- str_sub(year, 3,4)
  dat <- dat %>% 
    # main ids - add p_id_ prefix
    rename_with(~glue("p_id_{.x}"),c(DUID, PID, DUPERSID, PANEL)) %>% 
    # family, residential and other ids (other then person and DU) - add prefix g_id_
    rename_with(~glue("fr_id_{.x}"),  contains(c("FAMID","HIEUIDX","RULETR", "REFPRS", "RESP", "PROXY"))) %>% 
    # family, residential and other group potential explanatory variables (other then person and DU) -
    # add prefix fr_
    rename_with(~glue("fr_{.x}"), contains(c("RUCLAS","FCRP1231","FMRS1231","RUSIZE","FAMS","FCSZ1231", "FAMRFPYR"))) %>% 
    # demographic variables - add dem_ prefix
    rename_with(~glue("dem_{.x}"), contains(c("REGION", "MSA", "INTVLANG", "AGE", "DOB", "SEX","RACE", "HISP"))) %>% 
    # survey administration - add surv_ad_ prefix
    rename_with(~glue("dem_{.x}"), contains(c("BEGRF", "ENDRF", "KEY", "INSCOP", "INSC1", "ELGRND", "PSTAT", "RURSLT"))) %>% 
    
    
  assign(dat_name, dat, envir = .GlobalEnv)
}


walk(ls(pattern = "^FYC?"), rename_vars)
gc()
