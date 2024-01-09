# Count the Ns for the regression tables in the paper

# WRITTEN IN R VERSION: R version 4.0.3 (2020-10-10)

library(tidyverse)
library(patchwork)

# Set the working directory
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)
setwd('..') 
setwd('..') 

options(stringsAsFactors = FALSE)
options(scipen = 999)

# Load in the public data file
bd <- read_rds("data/public_dataset.rds")

# each "person" is a row - so can filter out all the rows that are missing demographics
bd_count <- bd %>%
  filter(!(is.na(female))) %>%
  filter(!(is.na(child))) %>%
  filter(!(is.na(wcba))) %>%
  filter(!(is.na(race_cat))) %>%
  filter(!(is.na(PIR_cat)))

# Are there any observations with non-missing concentration data but missing weights?
# No because we cleaned this up as we were making the public data
# BUT, we do need to clean up if anyone is missing creatinine or cotinine

bd_count <- bd_count %>%
  mutate(counter_nonsmoke_blood=1) %>%
  mutate(counter_nonsmoke_urine = case_when(
    counter_nonsmoke_blood==1 & is.na(ln_UCR)==F ~ 1,
    TRUE ~ 0)) %>%
  mutate(counter_smoke_blood = case_when(
    counter_nonsmoke_blood==1 & is.na(ln_COT)==F ~ 1,
    TRUE ~ 0)) %>%
  mutate(counter_smoke_urine = case_when(
    counter_smoke_blood==1 & is.na(ln_UCR)==F ~ 1,
    TRUE ~ 0))

counts_nonsmoke_urine <- bd_count %>%
  filter(counter_nonsmoke_urine==1) %>%
  select(SEQN,  
         ln_MZP, ln_MBP, ln_MEP, ln_MIB, ln_MHH, ln_CNP, ln_COP,
         ln_BPH, ln_BP3, ln_TRS, ln_14D, ln_DCB, ln_MPB, ln_PPB, ln_BPS) %>%
  summarise_all(~sum(!is.na(.))) %>%
  select(-SEQN) %>%
  pivot_longer(cols = c(ln_MZP, ln_MBP, ln_MEP, ln_MIB, ln_MHH, ln_CNP, ln_COP,
                        ln_BPH, ln_BP3, ln_TRS, ln_14D, ln_DCB, ln_MPB, ln_PPB, ln_BPS)) %>%
  rename(chemical=name,
         num_obs=value)

counts_nonsmoke_blood <- bd_count %>%
  filter(counter_nonsmoke_blood==1) %>%
  select(SEQN,
         ln_VCF,
         ln_MPAH, ln_PFDE, ln_PFHS, ln_PFNA, ln_PFOA, ln_PFOS,
         ln_BPB, ln_THG) %>%
  summarise_all(~sum(!is.na(.))) %>%
  select(-SEQN) %>%
  pivot_longer(cols=c(ln_VCF,
                      ln_MPAH, ln_PFDE, ln_PFHS, ln_PFNA, ln_PFOA, ln_PFOS,
                      ln_BPB, ln_THG)) %>%
  rename(chemical=name,
         num_obs=value)

counts_smoke_urine <- bd_count %>%
  filter(counter_smoke_urine==1) %>%
  select(SEQN,
         ln_2MH, ln_34M, ln_AAM, ln_ATC, ln_BMA, ln_CYM, ln_DHB, ln_HP2, ln_MAD, ln_MB3,
         ln_P01, ln_P02, ln_P03, ln_P04, ln_P06, ln_P10, ln_P05, ln_P07, ln_P17, ln_P19,
         ln_UCD) %>%
  summarise_all(~sum(!is.na(.))) %>%
  select(-SEQN) %>%
  pivot_longer(cols=c(ln_2MH, ln_34M, ln_AAM, ln_ATC, ln_BMA, ln_CYM, ln_DHB, ln_HP2, ln_MAD, ln_MB3,
                      ln_P01, ln_P02, ln_P03, ln_P04, ln_P06, ln_P10, ln_P05, ln_P07, ln_P17, ln_P19,
                      ln_UCD)) %>%
  rename(chemical=name,
         num_obs=value)

counts_smoke_blood <- bd_count %>%
  filter(counter_smoke_blood==1) %>%
  select(SEQN, ln_VTO) %>%
  summarise_all(~sum(!is.na(.))) %>%
  select(-SEQN) %>%
  pivot_longer(cols=c(ln_VTO)) %>%
  rename(chemical=name,
         num_obs=value)

counts <- rbind(counts_smoke_blood,
             counts_smoke_urine,
             counts_nonsmoke_blood,
             counts_nonsmoke_urine)

#save as a R dataset
#write_rds(counts, "data/obs_counts_for_tables.rds")

