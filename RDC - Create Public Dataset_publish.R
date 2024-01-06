# This file creates the NHANES public data set that we will provide to our RDC analyst
# PUBLIC DATA GENERATED ON 11/19/20
# Note: I am calculating the weights within each chemical class section
# WRITTEN IN R VERSION: R version 3.6.3 (2020-02-29)

library(tidyverse)
library(survey)
library(RNHANES)

# Set the working directory
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)
setwd('..') 

options(stringsAsFactors = FALSE)
options(scipen = 999)
 
# Start with phthalates - we are interested in the following phthalates metabolites from NHANES:
# URXMBP (99-00 through 15-16)
# URXMEP (99-00 through 15-16)
# URXMZP (99-00 through 15-16)
# URXMHH (01-02 through 15-16)
# URXMIB (01-02 through 15-16)
# URXCNP (05-06 through 15-16)
# URXCOP (05-06 through 15-16)

# Load in each cycle of phthalates data (without demographics):
phthalates9900 <- nhanes_load_data("PHPYPA", "1999-2000", demographics = FALSE) %>%
  select(SEQN, cycle, WTSPH4YR, WTSPH2YR, URXMBP, URXMEP, URXMZP) %>%
  filter(WTSPH4YR!=0)

phthalates0102 <- nhanes_load_data("PHPYPA_B", "2001-2002", demographics = FALSE) %>%
  select(SEQN, cycle, WTSPH4YR, WTSPH2YR, URXMBP, URXMEP, URXMZP, URXMHH, URXMIB) %>%
  filter(WTSPH4YR!=0)

phthalates0304 <- nhanes_load_data("L24PH_C", "2003-2004", demographics = FALSE) %>%
  select(SEQN, cycle, WTSB2YR, URXMBP, URXMEP, URXMHH, URXMIB, URXMZP) %>%
  filter(WTSB2YR!=0)

phthalates0506 <- nhanes_load_data("PHTHTE_D", "2005-2006", demographics = FALSE) %>%
  select(SEQN, cycle, WTSB2YR, URXCNP, URXCOP, URXMBP, URXMEP, URXMHH, URXMIB, URXMZP) %>%
  filter(WTSB2YR!=0)

phthalates0708 <- nhanes_load_data("PHTHTE_E", "2007-2008", demographics = FALSE) %>%
  select(SEQN, cycle, WTSB2YR, URXCNP, URXCOP, URXMBP, URXMEP, URXMHH, URXMIB, URXMZP) %>%
  filter(WTSB2YR!=0)

phthalates0910 <- nhanes_load_data("PHTHTE_F", "2009-2010", demographics = FALSE) %>%
  select(SEQN, cycle, WTSB2YR, URXCNP, URXCOP, URXMBP, URXMEP, URXMHH, URXMIB, URXMZP) %>%
  filter(WTSB2YR!=0)

phthalates1112 <- nhanes_load_data("PHTHTE_G", "2011-2012", demographics = FALSE) %>%
  select(SEQN, cycle, WTSA2YR, URXCNP, URXCOP, URXMBP, URXMEP, URXMHH, URXMIB, URXMZP) %>%
  # note: contains 43 records with WTSA2YR and all phthalate data missing - just going to delete these here
  filter(is.na(WTSA2YR)==F) %>%
  filter(WTSA2YR!=0)

phthalates1314 <- nhanes_load_data("PHTHTE_H", "2013-2014", demographics = FALSE) %>%
  select(SEQN, cycle, WTSB2YR, URXCNP, URXCOP, URXMBP, URXMEP, URXMHH, URXMIB, URXMZP) %>%
  filter(WTSB2YR!=0)

phthalates1516 <- nhanes_load_data("PHTHTE_I", "2015-2016", demographics = FALSE) %>%
  select(SEQN, cycle, WTSB2YR, URXCNP, URXCOP, URXMBP, URXMEP, URXMHH, URXMIB, URXMZP) %>%
  filter(WTSB2YR!=0)

# Combine the cycles of data together
phthalates <- bind_rows(phthalates9900,
                        phthalates0102,
                        phthalates0304,
                        phthalates0506,
                        phthalates0708,
                        phthalates0910,
                        phthalates1112,
                        phthalates1314,
                        phthalates1516)

# Calculate the weights
phthalates <- phthalates %>%
    mutate(weight18_phthalates= case_when(
    !is.na(WTSPH4YR) ~ WTSPH4YR*2/9,
    !is.na(WTSA2YR) ~ WTSA2YR/9,
    !is.na(WTSB2YR) ~ WTSB2YR/9 # there are no C weights for phthalates
  )) %>%
  mutate(weight16_phthalates = case_when(
    cycle=="1999-2000" ~ NA_real_, 
    !is.na(WTSPH2YR) ~ WTSPH2YR/8,
    !is.na(WTSA2YR) ~ WTSA2YR/8,
    !is.na(WTSB2YR) ~ WTSB2YR/8
  )) %>%
  mutate(weight12_phthalates = case_when(
    cycle=="1999-2000" | cycle=="2001-2002" | cycle=="2003-2004" ~ NA_real_,
    !is.na(WTSA2YR) ~ WTSA2YR/6,
    !is.na(WTSB2YR) ~ WTSB2YR/6
  ))

phthalates <- phthalates %>%
  select(-WTSPH4YR, -WTSPH2YR, -WTSB2YR, -WTSA2YR)

# Phenols
# Data from 2003-2016; separate pesticide files for 2003-2012
# Note that for a given cycle, it's the same set of SEQN for both the phenols and the
# pesticide files (setdiff on the SEQN yields 0 observations and also it's the same weights)
# Note: have to filter out the 0 weight observations AFTER merging phenols and pesticides files for a given cycle to avoid getting NA cycles

phenols0304 <- nhanes_load_data("L24EPH_C", "2003-2004", demographics = FALSE) %>%
  select(SEQN,cycle, WTSC2YR, URXBPH, URXBP3, URDTRS) %>% 
  rename(URXTRS=URDTRS) #correct triclosan variable name

pesticides0304 <- nhanes_load_data("L24PP_C", "2003-2004", demographics = FALSE) %>%
  select(SEQN, URX14D, URXDCB)

phenols0304 <- phenols0304 %>%
  full_join(pesticides0304,by="SEQN") %>%
  filter(WTSC2YR!=0)

phenols0506 <- nhanes_load_data("EPH_D", "2005-2006", demographics = FALSE) %>%
  select(SEQN,cycle, WTSB2YR, URXBPH, URXBP3, URXTRS, URXMPB, URXPPB)

pesticides0506 <- nhanes_load_data("PP_D", "2005-2006", demographics = FALSE) %>%
  select(SEQN, URX14D, URXDCB)

phenols0506 <- phenols0506 %>%
  full_join(pesticides0506, by="SEQN") %>% 
  filter(WTSB2YR!=0)

phenols0708 <- nhanes_load_data("EPH_E", "2007-2008", demographics = FALSE) %>%
  select(SEQN,cycle, WTSB2YR, URXBPH, URXBP3, URXTRS, URXMPB, URXPPB)

pesticides0708 <- nhanes_load_data("PP_E", "2007-2008", demographics = FALSE) %>%
  select(SEQN, URX14D, URXDCB)

phenols0708 <- phenols0708 %>%
  full_join(pesticides0708, by="SEQN") %>% 
  filter(WTSB2YR!=0) 

phenols0910 <- nhanes_load_data("EPH_F", "2009-2010", demographics = FALSE) %>%
  select(SEQN,cycle, WTSB2YR, URXBPH, URXBP3, URXTRS, URXMPB, URXPPB)

pesticides0910 <- nhanes_load_data("PP_F", "2009-2010", demographics = FALSE) %>%
  select(SEQN, URX14D, URXDCB)

phenols0910 <- phenols0910 %>%
  full_join(pesticides0910, by="SEQN") %>% 
  filter(WTSB2YR!=0)

phenols1112 <- nhanes_load_data("EPH_G", "2011-2012", demographics = FALSE) %>%
  select(SEQN,cycle, WTSA2YR, URXBPH, URXBP3, URXTRS, URXMPB, URXPPB)

pesticides1112 <- nhanes_load_data("PP_G", "2011-2012", demographics = FALSE) %>%
  select(SEQN, URX14D, URXDCB)

phenols1112 <- phenols1112 %>%
  full_join(pesticides1112, by="SEQN") %>% 
  filter(WTSA2YR!=0)

phenols1314 <- nhanes_load_data("EPHPP_H", "2013-2014", demographics = FALSE) %>%
  select(SEQN,cycle, WTSB2YR, URXBPH, URXBP3, URXTRS, URXMPB, URXPPB, URXBPS, URX14D, URXDCB) %>% 
  filter(WTSB2YR!=0)

phenols1516 <- nhanes_load_data("EPHPP_I", "2015-2016", demographics = FALSE) %>%
  select(SEQN,cycle, WTSB2YR, URXBPH, URXBP3, URXTRS, URXMPB, URXPPB, URXBPS, URX14D, URXDCB) %>% 
  filter(WTSB2YR!=0)

# Combine the cycles of data together
phenols <- bind_rows(phenols0304,
                     phenols0506,
                     phenols0708,
                     phenols0910,
                     phenols1112,
                     phenols1314,
                     phenols1516)

# Make weights - need 14, 12 and 4 year weights
phenols <- phenols %>%
  mutate(weight14_phenols = case_when(
    !is.na(WTSA2YR) ~ WTSA2YR/7,
    !is.na(WTSB2YR) ~ WTSB2YR/7,
    !is.na(WTSC2YR) ~ WTSC2YR/7
    )) %>%
  mutate(weight12_phenols = case_when(
    cycle=="2003-2004" ~ NA_real_,
    !is.na(WTSA2YR) ~ WTSA2YR/6,
    !is.na(WTSB2YR) ~ WTSB2YR/6,
  )) %>%
  mutate(weight4_phenols = case_when(
    cycle == "2003-2004" | cycle == "2005-2006" | cycle == "2007-2008" | cycle == "2009-2010" | cycle == "2011-2012" ~ NA_real_,
    !is.na(WTSB2YR) ~ WTSB2YR/2
  ))

phenols <- phenols %>%
  select(-WTSA2YR, -WTSB2YR, -WTSC2YR)

# VOCs in urine
#URX2MH	2005-2006, 2011-2016
#URX34M	2005-2006, 2011-2016
#URXAAM	2005-2006, 2011-2016
#URXATC	2005-2006, 2011-2016
#URXBMA	2005-2006, 2011-2016
#URXCYM	2005-2006, 2011-2016
#URXDHB	2005-2006, 2011-2016
#URXHP2	2005-2006, 2011-2016
#URXMAD	2005-2006, 2011-2016
#URXMB3	2005-2006, 2011-2016

urineVOC0506 <- nhanes_load_data("SSUVOC_D", "2005-2006", demographics = FALSE) %>%
  select(SEQN, cycle, WTSVOC2Y, URX2MH, URX34M, URXAAM, URXATC, URXBMA, URXCYM, URXDHB, URXHP2, URXMAD, URXMB3) %>% 
  filter(WTSVOC2Y!=0)

urineVOC1112 <- nhanes_load_data("UVOC_G", "2011-2012", demographics = FALSE) %>%
  select(SEQN, cycle, WTSA2YR, URX2MH, URX34M, URXAAM, URXATC, URXBMA, URXCYM, URXDHB, URXHP2, URXMAD, URXMB3) %>% 
  filter(WTSA2YR!=0)

urineVOC1314 <- nhanes_load_data("UVOC_H", "2013-2014", demographics = FALSE) %>%
  select(SEQN, cycle, WTSA2YR, URX2MH, URX34M, URXAAM, URXATC, URXBMA, URXCYM, URXDHB, URXHP2, URXMAD, URXMB3) %>% 
  filter(WTSA2YR!=0)

urineVOC1516 <- nhanes_load_data("UVOC_I", "2015-2016", demographics = FALSE) %>%
  select(SEQN, cycle, WTSA2YR, URX2MH, URX34M, URXAAM, URXATC, URXBMA, URXCYM, URXDHB, URXHP2, URXMAD, URXMB3) %>% 
  filter(WTSA2YR!=0)

# Combine the cycles of data together
urineVOC <- bind_rows(urineVOC0506,
                      urineVOC1112,
                      urineVOC1314,
                      urineVOC1516)

# Make 8-year weights
urineVOC <- urineVOC %>%
  mutate(weight8_urineVOC = case_when(
    !is.na(WTSA2YR) ~ WTSA2YR/4,
    !is.na(WTSVOC2Y) ~ WTSVOC2Y/4))

urineVOC <- urineVOC %>%
  select(-WTSA2YR, -WTSVOC2Y)

# Blood VOCs
#LBXVTO 1999-2010, 2013-2016 (toluene missing in 2011-2012 cycle)
#LBXVCF 1999-2006, 2007-2012 (with trihalomethanes), 2013-2016 (unit change)

bloodVOC9900 <- nhanes_load_data("LAB04", "1999-2000", demographics = FALSE) %>%
  select(SEQN, cycle, WTSVOC2Y, WTSVOC4Y, LBXVTO, LBXVCF) %>% 
  filter(WTSVOC4Y!=0)

bloodVOC0102 <- nhanes_load_data("L04VOC_B", "2001-2002", demographics = FALSE) %>%
  select(SEQN, cycle, WTSVOC2Y, WTSVOC4Y, LBXVTO, LBXVCF) %>% 
  filter(WTSVOC4Y!=0)

bloodVOC0304 <- nhanes_load_data("L04VOC_C", "2003-2004", demographics = FALSE) %>%
  select(SEQN, cycle, WTSVOC2Y, LBXVTO, LBXVCF) %>% 
  filter(WTSVOC2Y!=0)

bloodVOC0506 <- nhanes_load_data("VOCWB_D", "2005-2006", demographics = FALSE) %>%
  select(SEQN, cycle, WTSVOC2Y, LBXVTO, LBXVCF) %>% 
  filter(WTSVOC2Y!=0)

# In 2007-2008 through 2011-2012 cycles, chloroform is in separate trihalomethanes file; same SEQNs
bloodVOC0708 <- nhanes_load_data("VOCWB_E", "2007-2008", demographics = FALSE) %>%
  select(SEQN, cycle, WTSVOC2Y, LBXVTO)

bloodVOC0708_c <- nhanes_load_data("VOCMWB_E", "2007-2008", demographics = FALSE) %>%
  select(SEQN, LBXVCF)

#checking both the same SEQNs before joining
stopifnot(length(intersect(bloodVOC0708$SEQN, bloodVOC0708_c$SEQN))==nrow(bloodVOC0708))

bloodVOC0708 <- bloodVOC0708 %>%
  left_join(bloodVOC0708_c, by = "SEQN") %>% 
  filter(WTSVOC2Y!=0) 

bloodVOC0910 <- nhanes_load_data("VOCWB_F", "2009-2010", demographics = FALSE) %>%
  select(SEQN, cycle, WTSVOC2Y, LBXVTO)

bloodVOC0910_c <- nhanes_load_data("VOCMWB_F", "2009-2010", demographics = FALSE) %>%
  select(SEQN, LBXVCF)

bloodVOC0910 <- bloodVOC0910 %>%
  left_join(bloodVOC0910_c, by = "SEQN") %>% 
  filter(WTSVOC2Y!=0)

# no toluene data in 2011-2012
bloodVOC1112 <- nhanes_load_data("VOCMWB_g", "2011-2012", demographics = FALSE) %>%
  select(SEQN, cycle, WTSVOC2Y, LBXVCF) %>% 
  filter(WTSVOC2Y!=0)

#NOTE: for 2013-2014 and 2015-2016, chloroform data is in ng/mL, but in earlier cycles it is in picograms/mL;
# going to convert 2013-2014 and 2015-2016 data so that it's all in picograms/mL; this also matches what's done in Exposure Report
bloodVOC1314 <- nhanes_load_data("VOCWB_H", "2013-2014", demographics = FALSE) %>%
  select(SEQN, cycle, WTSVOC2Y, LBXVTO, LBXVCF) %>% 
  filter(WTSVOC2Y!=0) %>%
  mutate(LBXVCF=LBXVCF*1000) #multiply by 1000 to get from nanograms to picograms

bloodVOC1516 <- nhanes_load_data("VOCWB_I", "2015-2016", demographics = FALSE) %>%
  select(SEQN, cycle, WTSVOC2Y, LBXVTO, LBXVCF) %>%
  filter(WTSVOC2Y!=0) %>%
  mutate(LBXVCF=LBXVCF*1000) #multiply by 1000 to get from nanograms to picograms

# Combine the cycles of data together
bloodVOC <- bind_rows(bloodVOC9900,
                      bloodVOC0102,
                      bloodVOC0304,
                      bloodVOC0506,
                      bloodVOC0708,
                      bloodVOC0910,
                      bloodVOC1112,
                      bloodVOC1314,
                      bloodVOC1516)

# Calculate the weights - need 18-year and 16-year weights (because no toluene in 11-12)
bloodVOC <- bloodVOC %>%
  mutate(weight18_bloodVOC= case_when(
    !is.na(WTSVOC4Y) ~ WTSVOC4Y*2/9,
    !is.na(WTSVOC2Y) ~ WTSVOC2Y/9
  )) %>%
  mutate(weight16_bloodVOC = case_when(
    cycle=="2011-2012" ~ NA_real_, 
    !is.na(WTSVOC4Y) ~ WTSVOC4Y*2/8,
    !is.na(WTSVOC2Y) ~ WTSVOC2Y/8
  ))

bloodVOC <- bloodVOC %>%
  select(-WTSVOC4Y, -WTSVOC2Y)

#PAHs
# URXP01 (data for 01-02 through 13-14)
# URXP02 (data for 01-02 through 13-14)
# URXP03 (data for 01-02 through 13-14)
# URXP04 (data for 01-02 through 13-14)
# URXP05 (data for 01-02 through 11-12)
# URXP06 (data for 01-02 through 13-14)
# URXP07 (data for 01-02 through 11-12)
# URXP10 (data for 01-02 through 13-14)
# URXP17 (data for 03-04 through 11-12)
# URXP19 (data for 03-04, 05-06, 11-12)

PAH0102 <- nhanes_load_data("PHPYPA_B", "2001-2002", demographics = FALSE) %>%
  select(SEQN, cycle, WTSPH4YR, WTSPH2YR, URXP01, URXP02, URXP03, URXP04, URXP05, URXP06, URXP07, URXP10) %>%
  filter(WTSPH2YR!=0) %>%
  mutate(URXP01=URXP01/1000, # convert P01 and P02 from ng/L to micrograms/L to match Exposure Report, the rest are reported in ng/L so don't need to convert
         URXP02=URXP02/1000)
  
PAH0304 <- nhanes_load_data("L31PAH_C", "2003-2004", demographics = FALSE) %>%
  select(SEQN, cycle, WTSB2YR, URXP01, URXP02, URXP03, URXP04, URXP05, URXP06, URXP07, URXP10, URXP17, URXP19) %>%
  filter(WTSB2YR!=0) %>%
  mutate(URXP01=URXP01/1000, # convert P01 and P02 from ng/L to micrograms/L to match Exposure Report, the rest are reported in ng/L so don't need to convert
         URXP02=URXP02/1000)

PAH0506 <- nhanes_load_data("PAH_D", "2005-2006", demographics = FALSE) %>%
  select(SEQN, cycle, WTSB2YR, URXP01, URXP02, URXP03, URXP04, URXP05, URXP06, URXP07, URXP10, URXP17, URXP19) %>%
  filter(WTSB2YR!=0) %>%
  mutate(URXP01=URXP01/1000, # convert P01 and P02 from ng/L to micrograms/L to match Exposure Report, the rest are reported in ng/L so don't need to convert
         URXP02=URXP02/1000)

PAH0708 <- nhanes_load_data("PAH_E", "2007-2008", demographics = FALSE) %>%
  select(SEQN, cycle, WTSB2YR, URXP01, URXP02, URXP03, URXP04, URXP05, URXP06, URXP07, URXP10, URXP17) %>%
  filter(WTSB2YR!=0) %>%
  mutate(URXP01=URXP01/1000, # convert P01 and P02 from ng/L to micrograms/L to match Exposure Report, the rest are reported in ng/L so don't need to convert
         URXP02=URXP02/1000)

PAH0910 <- nhanes_load_data("PAH_F", "2009-2010", demographics = FALSE) %>%
  select(SEQN, cycle, WTSB2YR, URXP01, URXP02, URXP03, URXP04, URXP05, URXP06, URXP07, URXP10, URXP17) %>%
  filter(WTSB2YR!=0) %>%
  mutate(URXP01=URXP01/1000, # convert P01 and P02 from ng/L to micrograms/L to match Exposure Report, the rest are reported in ng/L so don't need to convert
         URXP02=URXP02/1000)

PAH1112 <- nhanes_load_data("PAH_G", "2011-2012", demographics = FALSE) %>%
  select(SEQN, cycle, WTSA2YR, URXP01, URXP02, URXP03, URXP04, URXP05, URXP06, URXP07, URXP10, URXP17, URXP19) %>%
  filter(WTSA2YR!=0) %>%
  mutate(URXP01=URXP01/1000, # convert P01 and P02 from ng/L to micrograms/L to match Exposure Report, the rest are reported in ng/L so don't need to convert
         URXP02=URXP02/1000)

PAH1314 <- nhanes_load_data("PAH_H", "2013-2014", demographics = FALSE) %>%
  select(SEQN, cycle, WTSA2YR, URXP01, URXP02, URXP03, URXP04, URXP06, URXP10) %>%
  filter(WTSA2YR!=0) %>%
  mutate(URXP01=URXP01/1000, # convert P01 and P02 from ng/L to micrograms/L to match Exposure Report, the rest are reported in ng/L so don't need to convert
         URXP02=URXP02/1000)

# Combine the cycles of data together
PAH <- bind_rows(PAH0102,
                 PAH0304,
                 PAH0506,
                 PAH0708,
                 PAH0910,
                 PAH1112,
                 PAH1314)

# Calculate the weights - need 14-year, 12-year, 10-year, and 6-year weights
PAH <- PAH %>%
  mutate(weight14_PAH= case_when(
    !is.na(WTSPH2YR) ~ WTSPH2YR/7,
    !is.na(WTSA2YR) ~ WTSA2YR/7,
    !is.na(WTSB2YR) ~ WTSB2YR/7 
  )) %>%
  mutate(weight12_PAH= case_when(
    cycle=="2013-2014" ~ NA_real_,
    !is.na(WTSPH2YR) ~ WTSPH2YR/6,
    !is.na(WTSA2YR) ~ WTSA2YR/6,
    !is.na(WTSB2YR) ~ WTSB2YR/6 
  )) %>%
  mutate(weight10_PAH= case_when(
    cycle=="2001-2002" | cycle=="2013-2014" ~ NA_real_,
    !is.na(WTSPH2YR) ~ WTSPH2YR/5,
    !is.na(WTSA2YR) ~ WTSA2YR/5,
    !is.na(WTSB2YR) ~ WTSB2YR/5 
  )) %>%
  mutate(weight6_PAH= case_when(
    cycle=="2001-2002" | cycle=="2007-2008" | cycle=="2009-2010" | cycle=="2013-2014" ~ NA_real_,
    !is.na(WTSPH2YR) ~ WTSPH2YR/3,
    !is.na(WTSA2YR) ~ WTSA2YR/3,
    !is.na(WTSB2YR) ~ WTSB2YR/3 
  )) 

PAH <- PAH %>%
  select(-WTSPH4YR, -WTSPH2YR,-WTSB2YR, -WTSA2YR)

# PFAS
  # LBXMPAH (03-04 through 15-16)
  # LBXPFDE (03-04 through 15-16)
  # LBXPFHS (03-04 through 15-16)
  # LBXPFNA (03-04 through 15-16)
  # LBXPFOA (03-04 through 15-16)
  # LBXPFOS (03-04 through 15-16)

pfas0304 <- nhanes_load_data("L24PFC_C", "2003-2004", demographics = FALSE) %>%
  select(SEQN, cycle, WTSA2YR, LBXMPAH, LBXPFDE, LBXPFHS, LBXPFNA, LBXPFOA, LBXPFOS) %>%
  filter(WTSA2YR!=0) 

pfas0506 <- nhanes_load_data("PFC_D", "2005-2006", demographics = FALSE) %>%
  select(SEQN, cycle, WTSA2YR, LBXMPAH, LBXPFDE, LBXPFHS, LBXPFNA, LBXPFOA, LBXPFOS) %>%
  filter(WTSA2YR!=0) 

pfas0708 <- nhanes_load_data("PFC_E", "2007-2008", demographics = FALSE) %>%
  select(SEQN, cycle, WTSC2YR, LBXMPAH, LBXPFDE, LBXPFHS, LBXPFNA, LBXPFOA, LBXPFOS) %>%
  filter(WTSC2YR!=0) 

pfas0910 <- nhanes_load_data("PFC_F", "2009-2010", demographics = FALSE) %>%
  select(SEQN, cycle, WTSC2YR, LBXMPAH, LBXPFDE, LBXPFHS, LBXPFNA, LBXPFOA, LBXPFOS) %>%
  filter(WTSC2YR!=0) 

pfas1112 <- nhanes_load_data("PFC_G", "2011-2012", demographics = FALSE) %>%
  select(SEQN, cycle, WTSA2YR, LBXMPAH, LBXPFDE, LBXPFHS, LBXPFNA, LBXPFOA, LBXPFOS) %>%
  filter(WTSA2YR!=0) 

pfas1314 <- nhanes_load_data("PFAS_H", "2013-2014", demographics = FALSE) %>%
  select(SEQN, cycle, WTSB2YR, LBXMPAH, LBXPFDE, LBXPFHS, LBXPFNA)
# no LBXPFOA, LBXPFOS - to get these, have to sum the isomers from the supplemental

pfas1314s <- nhanes_load_data("SSPFAS_H", "2013-2014", demographics = FALSE) %>%
  select(SEQN, SSNPFOA, SSBPFOA, SSNPFOS, SSMPFOS )

pfas1314 <- pfas1314 %>%
  left_join(pfas1314s, by="SEQN") %>%
  mutate(LBXPFOA=SSNPFOA+SSBPFOA,
         LBXPFOS=SSNPFOS+SSMPFOS) %>%
  select(-SSNPFOA, -SSBPFOA, -SSNPFOS, -SSMPFOS) %>%
  filter(WTSB2YR!=0) 

pfas1516 <- nhanes_load_data("PFAS_I", "2015-2016", demographics = FALSE) %>%
  select(SEQN, cycle, WTSB2YR, LBXMPAH, LBXPFDE, LBXPFHS, LBXPFNA, LBXNFOA, LBXBFOA, LBXNFOS, LBXMFOS)  %>%
  filter(WTSB2YR!=0) %>%
  mutate(LBXPFOA=LBXNFOA+LBXBFOA,
         LBXPFOS=LBXNFOS+LBXMFOS) %>%
  select(-LBXNFOA, -LBXBFOA, -LBXNFOS, -LBXMFOS)

pfas <- bind_rows(pfas0304,
                  pfas0506,
                  pfas0708,
                  pfas0910,
                  pfas1112,
                  pfas1314,
                  pfas1516)

# Calculate the weights - only need 14-year weights
pfas <- pfas %>%
  mutate(weight14_pfas= case_when(
    !is.na(WTSC2YR) ~ WTSC2YR/7,
    !is.na(WTSA2YR) ~ WTSA2YR/7,
    !is.na(WTSB2YR) ~ WTSB2YR/7 
  ))

pfas <- pfas %>%
  select(-WTSC2YR,-WTSB2YR, -WTSA2YR)

# Metals
# Lead and mercury (in blood); note that 1999-2012 uses MEC weights
lead9900 <- nhanes_load_data("LAB06", "1999-2000", demographics = TRUE) %>%
  select(SEQN, cycle, WTMEC4YR, WTMEC2YR, LBXBPB, LBXTHG) %>%
  filter(WTMEC4YR!=0) 

lead0102 <- nhanes_load_data("L06_B", "2001-2002", demographics = TRUE) %>%
  select(SEQN, cycle, WTMEC4YR, WTMEC2YR, LBXBPB, LBXTHG) %>%
  filter(WTMEC4YR!=0)

lead0304 <- nhanes_load_data("L06BMT_C", "2003-2004", demographics = TRUE) %>%
  select(SEQN, cycle, WTMEC2YR, LBXBPB, LBXTHG) %>%
  filter(WTMEC2YR!=0)

lead0506 <- nhanes_load_data("PBCD_D", "2005-2006", demographics = TRUE) %>%
  select(SEQN, cycle, WTMEC2YR, LBXBPB, LBXTHG) %>%
  filter(WTMEC2YR!=0)

lead0708 <- nhanes_load_data("PBCD_E", "2007-2008", demographics = TRUE) %>%
  select(SEQN, cycle, WTMEC2YR, LBXBPB, LBXTHG) %>%
  filter(WTMEC2YR!=0)

lead0910 <- nhanes_load_data("PBCD_F", "2009-2010", demographics = TRUE) %>%
  select(SEQN, cycle, WTMEC2YR, LBXBPB, LBXTHG) %>%
  filter(WTMEC2YR!=0)

lead1112 <- nhanes_load_data("PBCD_G", "2011-2012", demographics = TRUE) %>%
  select(SEQN, cycle, WTMEC2YR, LBXBPB, LBXTHG) %>%
  filter(WTMEC2YR!=0)

lead1314 <- nhanes_load_data("PBCD_H", "2013-2014", demographics = FALSE) %>%
  select(SEQN, cycle, WTSH2YR, LBXBPB, LBXTHG) %>%
  filter(WTSH2YR!=0)

lead1516 <- nhanes_load_data("PBCD_I", "2015-2016", demographics = FALSE) %>%
  select(SEQN, cycle, WTSH2YR, LBXBPB, LBXTHG) %>%
  filter(WTSH2YR!=0)

lead_mercury <- bind_rows(lead9900,
                  lead0102,
                  lead0304,
                  lead0506,
                  lead0708,
                  lead0910,
                  lead1112,
                  lead1314,
                  lead1516)

# Calculate the weights
lead_mercury <- lead_mercury %>%
  mutate(weight18_lead_mercury= case_when(
    !is.na(WTMEC4YR) ~ WTMEC4YR*2/9,
    !is.na(WTMEC2YR) ~ WTMEC2YR/9,
    !is.na(WTSH2YR) ~ WTSH2YR/9
  ))

lead_mercury <- lead_mercury %>%
  select(-WTMEC4YR, -WTMEC2YR, -WTSH2YR)

#Cadmium
# From 2003-2004 lab notes: When comparing urine cadmium across two-year cycles please note that even though these 
# two variables have different names the data is comparable. Variable URXUCD is used in 1999-2000 and 2003-2004 and 
# variable URDUCD was used in 2001-2002. Variable URDUCD was derived to correct for molybdenum interference 
# (Reference to 2001-2002 lab 6 heavy metal documentation). Beginning in 2003-2004 the urinary cadmium data is 
# corrected at the testing laboratory for molybdenum interference.
# Note that URDUCD can take on 0 values so need to adjust before we take logs.
cad9900 <- nhanes_load_data("LAB06HM", "1999-2000", demographics = FALSE) %>%
  rename(URXUCD=URDUCD) %>%
  select(SEQN, cycle, WTSHM2YR, WTSHM4YR, URXUCD) %>%
  filter(WTSHM4YR!=0)

cad0102 <- nhanes_load_data("L06HM_B", "2001-2002", demographics = FALSE) %>%
  rename(URXUCD=URDUCD) %>%
  select(SEQN, cycle, WTSHM2YR, WTSHM4YR, URXUCD) %>%
  filter(WTSHM4YR!=0)
  
cad0304 <- nhanes_load_data("L06HM_C", "2003-2004", demographics = FALSE) %>%
  select(SEQN, cycle, WTSA2YR, URXUCD) %>%
  filter(WTSA2YR!=0)

cad0506 <- nhanes_load_data("UHM_D", "2005-2006", demographics = FALSE) %>%
  select(SEQN, cycle, WTSA2YR, URXUCD) %>%
  filter(WTSA2YR!=0)

cad0708 <- nhanes_load_data("UHM_E", "2007-2008", demographics = FALSE) %>%
  select(SEQN, cycle, WTSA2YR, URXUCD) %>%
  filter(WTSA2YR!=0)

cad0910 <- nhanes_load_data("UHM_F", "2009-2010", demographics = FALSE) %>%
  select(SEQN, cycle, WTSA2YR, URXUCD) %>%
  filter(WTSA2YR!=0)

cad1112 <- nhanes_load_data("UHM_G", "2011-2012", demographics = FALSE) %>%
  select(SEQN, cycle, WTSA2YR, URXUCD) %>%
  filter(WTSA2YR!=0)

cad1314 <- nhanes_load_data("UM_H", "2013-2014", demographics = FALSE) %>%
  select(SEQN, cycle, WTSA2YR, URXUCD) %>%
  filter(WTSA2YR!=0)

cad1516 <- nhanes_load_data("UM_I", "2015-2016", demographics = FALSE) %>%
  select(SEQN, cycle, WTSA2YR, URXUCD) %>%
  filter(WTSA2YR!=0)

cadmium <- bind_rows(cad9900,
                     cad0102,
                     cad0304,
                     cad0506,
                     cad0708,
                     cad0910,
                     cad1112,
                     cad1314,
                     cad1516)


cadmium <- cadmium %>%
  mutate(weight18_cadmium= case_when(
    !is.na(WTSHM4YR) ~ WTSHM4YR*2/9,
    !is.na(WTSHM2YR) ~ WTSHM2YR/9,
    !is.na(WTSA2YR) ~ WTSA2YR/9
  ))

cadmium <- cadmium %>%
  select(-WTSHM4YR, -WTSHM2YR, -WTSA2YR)

# Load in cotinine
cot9900 <- nhanes_load_data("LAB06", "1999-2000", demographics = FALSE) %>%
  select(SEQN, cycle, LBXCOT)

cot0102 <- nhanes_load_data("L06_B", "2001-2002", demographics = FALSE) %>%
  select(SEQN, cycle, LBXCOT)

cot0304 <- nhanes_load_data("L06COT_C", "2003-2004", demographics = FALSE) %>%
  select(SEQN, cycle, LBXCOT)

cot0506 <- nhanes_load_data("COT_D", "2005-2006", demographics = FALSE) %>%
  select(SEQN, cycle, LBXCOT)

cot0708 <- nhanes_load_data("COTNAL_E", "2007-2008", demographics = FALSE) %>%
  select(SEQN, cycle, LBXCOT)

cot0910 <- nhanes_load_data("COTNAL_F", "2009-2010", demographics = FALSE) %>%
  select(SEQN, cycle, LBXCOT)

cot1112 <- nhanes_load_data("COTNAL_G", "2011-2012", demographics = FALSE) %>%
  select(SEQN, cycle, LBXCOT)

cot1314 <- nhanes_load_data("COT_H", "2013-2014", demographics = FALSE) %>%
  select(SEQN, cycle, LBXCOT)

cot1516 <- nhanes_load_data("COT_I", "2015-2016", demographics = FALSE) %>%
  select(SEQN, cycle, LBXCOT)


cotinine <- bind_rows(cot9900,
                      cot0102,
                      cot0304,
                      cot0506,
                      cot0708,
                      cot0910,
                      cot1112,
                      cot1314,
                      cot1516)

# Load in creatinine
creat9900 <- nhanes_load_data("LAB16", "1999-2000", demographics = FALSE) %>%
  select(SEQN, cycle, URXUCR) 

creat0102 <- nhanes_load_data("L16_B", "2001-2002", demographics = FALSE) %>%
  select(SEQN, cycle, URXUCR) 

creat0304 <- nhanes_load_data("L16_C", "2003-2004", demographics = FALSE) %>%
  select(SEQN, cycle, URXUCR) 

creat0506 <- nhanes_load_data("ALB_CR_D", "2005-2006", demographics = FALSE) %>%
  select(SEQN, cycle, URXUCR) 

creat0708 <- nhanes_load_data("ALB_CR_E", "2007-2008", demographics = FALSE) %>%
  select(SEQN, cycle, URXUCR) 

creat0910 <- nhanes_load_data("ALB_CR_F", "2009-2010", demographics = FALSE) %>%
  select(SEQN, cycle, URXUCR) 

creat1112 <- nhanes_load_data("ALB_CR_G", "2011-2012", demographics = FALSE) %>%
  select(SEQN, cycle, URXUCR) 

creat1314 <- nhanes_load_data("ALB_CR_H", "2013-2014", demographics = FALSE) %>%
  select(SEQN, cycle, URXUCR) 

creat1516 <- nhanes_load_data("ALB_CR_I", "2015-2016", demographics = FALSE) %>%
  select(SEQN, cycle, URXUCR) 

creatinine <- bind_rows(creat9900,
                        creat0102,
                        creat0304,
                        creat0506,
                        creat0708,
                        creat0910,
                        creat1112,
                        creat1314,
                        creat1516)

# Merge everything together
bigdata <- phthalates %>%
  full_join(phenols, by=c("SEQN", "cycle")) %>%
  full_join(urineVOC, by=c("SEQN", "cycle")) %>%
  full_join(bloodVOC, by=c("SEQN", "cycle")) %>%
  full_join(PAH, by=c("SEQN", "cycle")) %>%
  full_join(pfas, by=c("SEQN", "cycle")) %>%
  full_join(lead_mercury, by=c("SEQN", "cycle")) %>%
  full_join(cadmium, by=c("SEQN", "cycle")) %>%
  left_join(cotinine, by=c("SEQN", "cycle")) %>%
  left_join(creatinine, by=c("SEQN", "cycle"))

# Load in the demographics
# Include the MEC weights
dem9900 <- nhanes_load_data("DEMO", "1999-2000") %>%
  mutate(cycle="1999-2000") %>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR, RIDRETH1, INDFMPIR, WTMEC4YR) 

dem0102 <- nhanes_load_data("DEMO_B", "2001-2002") %>%
  mutate(cycle="2001-2002") %>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR, RIDRETH1, INDFMPIR, WTMEC4YR) 

dem0304 <- nhanes_load_data("DEMO_C", "2003-2004") %>%
  mutate(cycle="2003-2004") %>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR, RIDRETH1, INDFMPIR, WTMEC2YR) 

dem0506 <- nhanes_load_data("DEMO_D", "2005-2006") %>%
  mutate(cycle="2005-2006") %>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR, RIDRETH1, INDFMPIR, WTMEC2YR) 

dem0708 <- nhanes_load_data("DEMO_E", "2007-2008") %>%
  mutate(cycle="2007-2008") %>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR, RIDRETH1, INDFMPIR, WTMEC2YR) 

dem0910 <- nhanes_load_data("DEMO_F", "2009-2010") %>%
  mutate(cycle="2009-2010") %>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR, RIDRETH1, INDFMPIR, WTMEC2YR) 

dem1112 <- nhanes_load_data("DEMO_G", "2011-2012") %>%
  mutate(cycle="2011-2012") %>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR, RIDRETH1, INDFMPIR, WTMEC2YR) 

dem1314 <- nhanes_load_data("DEMO_H", "2013-2014") %>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR, RIDRETH1, INDFMPIR, WTMEC2YR) 

dem1516 <- nhanes_load_data("DEMO_I", "2015-2016") %>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR, RIDRETH1, INDFMPIR, WTMEC2YR) 

dems <- bind_rows(dem9900,
                  dem0102,
                  dem0304,
                  dem0506,
                  dem0708,
                  dem0910,
                  dem1112,
                  dem1314,
                  dem1516)

dems <- dems %>%
  mutate(weight18_MEC = case_when(
    !is.na(WTMEC4YR) ~ WTMEC4YR*2/9,
    !is.na(WTMEC2YR) ~ WTMEC2YR/9
  ))

dems <- dems %>%
  mutate(female = case_when(RIAGENDR==2 ~ 1, 
                            RIAGENDR==1 ~ 0,
                            TRUE ~ NA_real_)) %>%
  mutate(child = case_when(RIDAGEYR<=11 & is.na(RIDAGEYR)==F ~ 1,
                           RIDAGEYR>11 & is.na(RIDAGEYR)==F ~ 0,
                           TRUE ~ NA_real_)) %>%
  mutate(wcba = case_when(is.na(RIAGENDR)==T ~ NA_real_,
                          is.na(RIDAGEYR)==T ~ NA_real_,
                          RIAGENDR==2 & 18<=RIDAGEYR & RIDAGEYR<=50 ~ 1,
                          TRUE ~ 0)) %>%
  mutate(race_cat = factor(case_when(
    RIDRETH1 == 1 ~ "Hispanic",
    RIDRETH1 == 2 ~ "Hispanic",
    RIDRETH1 == 3 ~ "Non-Hispanic White",
    RIDRETH1 == 4 ~ "Non-Hispanic Black",
    RIDRETH1 == 5 ~ "Other"),
    levels=c("Non-Hispanic White", "Hispanic", "Non-Hispanic Black", "Other")
  )) %>%
  mutate(PIR_cat = factor(case_when(
    INDFMPIR<1 ~ "low",
    INDFMPIR>=1 & INDFMPIR<=3 ~ "middle",
    INDFMPIR>3 ~ "high"),
    levels=c("middle","low","high")
  ))

dems <- dems %>%
  select(SEQN, cycle, female, child, wcba, race_cat, PIR_cat, weight18_MEC)

# Merge demographics with the chemical data
bigdata <- bigdata %>%
  full_join(dems, by=c("SEQN", "cycle")) 

# Create the ln concentrations
bigdata <- bigdata %>%
  mutate(ln_MZP=log(URXMZP),
         ln_MBP=log(URXMBP),
         ln_MEP=log(URXMEP),
         ln_MIB=log(URXMIB),
         ln_MHH=log(URXMHH),
         ln_CNP=log(URXCNP),
         ln_COP=log(URXCOP),
         ln_BPH = log(URXBPH),
         ln_BP3 = log(URXBP3),
         ln_TRS = log(URXTRS),
         ln_14D = log(URX14D),
         ln_DCB = log(URXDCB),
         ln_MPB = log(URXMPB),
         ln_PPB = log(URXPPB),
         ln_BPS = log(URXBPS),
         ln_2MH = log(URX2MH),
         ln_34M = log(URX34M),
         ln_AAM = log(URXAAM),
         ln_ATC = log(URXATC),
         ln_BMA = log(URXBMA),
         ln_CYM = log(URXCYM),
         ln_DHB = log(URXDHB),
         ln_HP2 = log(URXHP2),
         ln_MAD = log(URXMAD),
         ln_MB3 = log(URXMB3),
         ln_VTO = log(LBXVTO),
         ln_VCF = log(LBXVCF),
         ln_P01 = log(URXP01),
         ln_P02 = log(URXP02),
         ln_P03 = log(URXP03),
         ln_P04 = log(URXP04),
         ln_P05 = log(URXP05),
         ln_P06 = log(URXP06),
         ln_P07 = log(URXP07),
         ln_P10 = log(URXP10),
         ln_P17 = log(URXP17),
         ln_P19 = log(URXP19),
         ln_MPAH = log(LBXMPAH),
         ln_PFDE = log(LBXPFDE),
         ln_PFHS = log(LBXPFHS),
         ln_PFNA = log(LBXPFNA),
         ln_PFOA = log(LBXPFOA),
         ln_PFOS = log(LBXPFOS),
         ln_BPB = log(LBXBPB),
         ln_THG = log(LBXTHG),
         ln_UCD = case_when(
           URXUCD==0 ~ log(0.00001),
           TRUE ~ log(URXUCD)),
         ln_COT = log(LBXCOT),
         ln_UCR = log(URXUCR)
         )

# Create creatinine-adjusted versions of the analytes measured in urine
bigdata <- bigdata %>%
  mutate(MZP_cr=100*URXMZP/URXUCR,
         MBP_cr=100*URXMBP/URXUCR,
         MEP_cr=100*URXMEP/URXUCR,
         MIB_cr=100*URXMIB/URXUCR,
         MHH_cr=100*URXMHH/URXUCR,
         CNP_cr=100*URXCNP/URXUCR,
         COP_cr=100*URXCOP/URXUCR,
         BPH_cr=100*URXBPH/URXUCR,
         BP3_cr=100*URXBP3/URXUCR,
         TRS_cr=100*URXTRS/URXUCR,
         X14Dcr=100*URX14D/URXUCR,
         DCB_cr=100*URXDCB/URXUCR,
         MPB_cr=100*URXMPB/URXUCR,
         PPB_cr=100*URXPPB/URXUCR,
         BPS_cr=100*URXBPS/URXUCR,
         X2MHcr=100*URX2MH/URXUCR,
         X34Mcr=100*URX34M/URXUCR,
         AAM_cr=100*URXAAM/URXUCR,
         ATC_cr=100*URXATC/URXUCR,
         BMA_cr=100*URXBMA/URXUCR,
         CYM_cr=100*URXCYM/URXUCR,
         DHB_cr=100*URXDHB/URXUCR,
         HP2_cr=100*URXHP2/URXUCR,
         MAD_cr=100*URXMAD/URXUCR,
         MB3_cr=100*URXMB3/URXUCR,
         P01_cr=100*URXP01/URXUCR,
         P02_cr=100*URXP02/URXUCR,
         P03_cr=100*URXP03/URXUCR,
         P04_cr=100*URXP04/URXUCR,
         P05_cr=100*URXP05/URXUCR,
         P06_cr=100*URXP06/URXUCR,
         P07_cr=100*URXP07/URXUCR,
         P10_cr=100*URXP10/URXUCR,
         P17_cr=100*URXP17/URXUCR,
         P19_cr=100*URXP19/URXUCR,
         UCD_cr=100*URXUCD/URXUCR
         )

# Create lns of the creatinine-adjusted concentrations
bigdata <- bigdata %>%
  mutate(ln_MZP_cr=log(MZP_cr),
         ln_MBP_cr=log(MBP_cr),
         ln_MEP_cr=log(MEP_cr),
         ln_MIB_cr=log(MIB_cr),
         ln_MHH_cr=log(MHH_cr),
         ln_CNP_cr=log(CNP_cr),
         ln_COP_cr=log(COP_cr),
         ln_BPH_cr=log(BPH_cr),
         ln_BP3_cr=log(BP3_cr),
         ln_TRS_cr=log(TRS_cr),
         ln_14D_cr=log(X14Dcr),
         ln_DCB_cr=log(DCB_cr),
         ln_MPB_cr=log(MPB_cr),
         ln_PPB_cr=log(PPB_cr),
         ln_BPS_cr=log(BPS_cr),
         ln_2MH_cr=log(X2MHcr),
         ln_34M_cr=log(X34Mcr),
         ln_AAM_cr=log(AAM_cr),
         ln_ATC_cr=log(ATC_cr),
         ln_BMA_cr=log(BMA_cr),
         ln_CYM_cr=log(CYM_cr),
         ln_DHB_cr=log(DHB_cr),
         ln_HP2_cr=log(HP2_cr),
         ln_MAD_cr=log(MAD_cr),
         ln_MB3_cr=log(MB3_cr),
         ln_P01_cr=log(P01_cr),
         ln_P02_cr=log(P02_cr),
         ln_P03_cr=log(P03_cr),
         ln_P04_cr=log(P04_cr),
         ln_P05_cr=log(P05_cr),
         ln_P06_cr=log(P06_cr),
         ln_P07_cr=log(P07_cr),
         ln_P10_cr=log(P10_cr),
         ln_P17_cr=log(P17_cr),
         ln_P19_cr=log(P19_cr),
         ln_UCD_cr = case_when(
           UCD_cr==0 ~ log(0.00001),
           TRUE ~ log(UCD_cr))
         )

# Make yes/no variables for whether a concentration is above the national 95th percentile level for its corresponding cycle
# Cut-offs calculated from public data, by cycle
# First merge in the cut-offs
cut95 <- read_rds("data/cut95.rds")

bigdata <- bigdata %>%
  left_join(cut95, by="cycle")

bigdata <- bigdata %>%
  mutate(MZP_high = case_when(
    is.na(URXMZP) ~ NA_real_,  
    URXMZP>q95_URXMZP ~ 1,
    TRUE ~ 0)) %>%
  mutate(MBP_high = case_when(
    is.na(URXMBP) ~ NA_real_,  
    URXMBP>q95_URXMBP ~ 1,
    TRUE ~ 0)) %>%
  mutate(MEP_high = case_when(
    is.na(URXMEP) ~ NA_real_,  
    URXMEP>q95_URXMEP ~ 1,
    TRUE ~ 0)) %>%
  mutate(MIB_high = case_when(
    is.na(URXMIB) ~ NA_real_,  
    URXMIB>q95_URXMIB ~ 1,
    TRUE ~ 0)) %>%
  mutate(MHH_high = case_when(
    is.na(URXMHH) ~ NA_real_,  
    URXMHH>q95_URXMHH ~ 1,
    TRUE ~ 0)) %>%
  mutate(CNP_high = case_when(
    is.na(URXCNP) ~ NA_real_,  
    URXCNP>q95_URXCNP ~ 1,
    TRUE ~ 0)) %>%
  mutate(COP_high = case_when(
      is.na(URXCOP) ~ NA_real_,  
      URXCOP>q95_URXCOP ~ 1,
      TRUE ~ 0)) %>%
  mutate(BPH_high = case_when(
    is.na(URXBPH) ~ NA_real_,  
    URXBPH>q95_URXBPH ~ 1,
    TRUE ~ 0)) %>%
  mutate(BP3_high = case_when(
    is.na(URXBP3) ~ NA_real_,  
    URXBP3>q95_URXBP3 ~ 1,
    TRUE ~ 0)) %>%
  mutate(TRS_high = case_when(
    is.na(URXTRS) ~ NA_real_,  
    URXTRS>q95_URXTRS ~ 1,
    TRUE ~ 0)) %>%
  mutate(MPB_high = case_when(
      is.na(URXMPB) ~ NA_real_,  
      URXMPB>q95_URXMPB ~ 1,
      TRUE ~ 0)) %>%
  mutate(PPB_high = case_when(
    is.na(URXPPB) ~ NA_real_,  
    URXPPB>q95_URXPPB ~ 1,
    TRUE ~ 0)) %>%
  mutate(BPS_high = case_when(
    is.na(URXBPS) ~ NA_real_,  
    URXBPS>q95_URXBPS ~ 1,
    TRUE ~ 0)) %>%  
  mutate(X14D_high = case_when(
    is.na(URX14D) ~ NA_real_,  
    URX14D>q95_URX14D ~ 1,
    TRUE ~ 0)) %>%
  mutate(DCB_high = case_when(
    is.na(URXDCB) ~ NA_real_,  
    URXDCB>q95_URXDCB ~ 1,
    TRUE ~ 0)) %>%  
  mutate(X2MH_high = case_when(
      is.na(URX2MH) ~ NA_real_,  
      URX2MH>q95_URX2MH ~ 1,
      TRUE ~ 0)) %>%
  mutate(X34M_high = case_when(
    is.na(URX34M) ~ NA_real_,  
    URX34M>q95_URX34M ~ 1,
    TRUE ~ 0)) %>%
  mutate(AAM_high = case_when(
      is.na(URXAAM) ~ NA_real_,  
      URXAAM>q95_URXAAM ~ 1,
      TRUE ~ 0)) %>%
  mutate(ATC_high = case_when(
    is.na(URXATC) ~ NA_real_,  
    URXATC>q95_URXATC ~ 1,
    TRUE ~ 0)) %>%  
  mutate(BMA_high = case_when(
    is.na(URXBMA) ~ NA_real_,  
    URXBMA>q95_URXBMA ~ 1,
    TRUE ~ 0)) %>%    
  mutate(CYM_high = case_when(
      is.na(URXCYM) ~ NA_real_,  
      URXCYM>q95_URXCYM ~ 1,
      TRUE ~ 0)) %>%  
  mutate(DHB_high = case_when(
        is.na(URXDHB) ~ NA_real_,  
        URXDHB>q95_URXDHB ~ 1,
        TRUE ~ 0)) %>%
  mutate(HP2_high = case_when(
    is.na(URXHP2) ~ NA_real_,  
    URXHP2>q95_URXHP2 ~ 1,
    TRUE ~ 0)) %>%
  mutate(MAD_high = case_when(
    is.na(URXMAD) ~ NA_real_,  
    URXMAD>q95_URXMAD ~ 1,
    TRUE ~ 0)) %>%
  mutate(MB3_high = case_when(
    is.na(URXMB3) ~ NA_real_,  
    URXMB3>q95_URXMB3 ~ 1,
    TRUE ~ 0)) %>%
  mutate(VCF_high = case_when(
    is.na(LBXVCF) ~ NA_real_,  
    LBXVCF>q95_LBXVCF ~ 1,
    TRUE ~ 0)) %>%
  mutate(VTO_high = case_when(
    is.na(LBXVTO) ~ NA_real_,  
    LBXVTO>q95_LBXVTO ~ 1,
    TRUE ~ 0)) %>%
  mutate(P01_high = case_when(
    is.na(URXP01) ~ NA_real_,  
    URXP01>q95_URXP01 ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P02_high = case_when(
    is.na(URXP02) ~ NA_real_,  
    URXP02>q95_URXP02 ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P03_high = case_when(
    is.na(URXP03) ~ NA_real_,  
    URXP03>q95_URXP03 ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P04_high = case_when(
    is.na(URXP04) ~ NA_real_,  
    URXP04>q95_URXP04 ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P05_high = case_when(
    is.na(URXP05) ~ NA_real_,  
    URXP05>q95_URXP05 ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P06_high = case_when(
    is.na(URXP06) ~ NA_real_,  
    URXP06>q95_URXP06 ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P07_high = case_when(
    is.na(URXP07) ~ NA_real_,  
    URXP07>q95_URXP07 ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P10_high = case_when(
    is.na(URXP10) ~ NA_real_,  
    URXP10>q95_URXP10 ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P17_high = case_when(
    is.na(URXP17) ~ NA_real_,  
    URXP17>q95_URXP17 ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P19_high = case_when(
    is.na(URXP19) ~ NA_real_,  
    URXP19>q95_URXP19 ~ 1,
    TRUE ~ 0)) %>%  
  mutate(PFOA_high = case_when(
    is.na(LBXPFOA) ~ NA_real_,  
    LBXPFOA>q95_LBXPFOA ~ 1,
    TRUE ~ 0)) %>%  
  mutate(PFOS_high = case_when(
    is.na(LBXPFOS) ~ NA_real_,  
    LBXPFOS>q95_LBXPFOS ~ 1,
    TRUE ~ 0)) %>%  
  mutate(PFHS_high = case_when(
    is.na(LBXPFHS) ~ NA_real_,  
    LBXPFHS>q95_LBXPFHS ~ 1,
    TRUE ~ 0)) %>%  
  mutate(MPAH_high = case_when(
    is.na(LBXMPAH) ~ NA_real_,  
    LBXMPAH>q95_LBXMPAH ~ 1,
    TRUE ~ 0)) %>%  
  mutate(PFDE_high = case_when(
    is.na(LBXPFDE) ~ NA_real_,  
    LBXPFDE>q95_LBXPFDE ~ 1,
    TRUE ~ 0)) %>%  
  mutate(PFNA_high = case_when(
    is.na(LBXPFNA) ~ NA_real_,  
    LBXPFNA>q95_LBXPFNA ~ 1,
    TRUE ~ 0)) %>%  
  mutate(BPB_high = case_when(
    is.na(LBXBPB) ~ NA_real_,  
    LBXBPB>q95_LBXBPB ~ 1,
    TRUE ~ 0)) %>%  
  mutate(THG_high = case_when(
    is.na(LBXTHG) ~ NA_real_,  
    LBXTHG>q95_LBXTHG ~ 1,
    TRUE ~ 0)) %>%  
  mutate(UCD_high = case_when(
    is.na(URXUCD) ~ NA_real_,  
    URXUCD>q95_URXUCD ~ 1,
    TRUE ~ 0))  
  
# Make yes/no variables for whether a creatinine-adjusted concentration is above the national creatinine-adjusted 95th percentile level for its corresponding cycle
bigdata <- bigdata %>%
  mutate(MZP_high_cr = case_when(
    is.na(MZP_cr) ~ NA_real_,  
    MZP_cr>q95_MZP_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(MBP_high_cr = case_when(
    is.na(MBP_cr) ~ NA_real_,  
    MBP_cr>q95_MBP_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(MEP_high_cr = case_when(
    is.na(MEP_cr) ~ NA_real_,  
    MEP_cr>q95_MEP_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(MIB_high_cr = case_when(
    is.na(MIB_cr) ~ NA_real_,  
    MIB_cr>q95_MIB_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(MHH_high_cr = case_when(
    is.na(MHH_cr) ~ NA_real_,  
    MHH_cr>q95_MHH_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(CNP_high_cr = case_when(
    is.na(CNP_cr) ~ NA_real_,  
    CNP_cr>q95_CNP_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(COP_high_cr = case_when(
    is.na(COP_cr) ~ NA_real_,  
    COP_cr>q95_COP_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(BPH_high_cr = case_when(
    is.na(BPH_cr) ~ NA_real_,  
    BPH_cr>q95_BPH_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(BP3_high_cr = case_when(
    is.na(BP3_cr) ~ NA_real_,  
    BP3_cr>q95_BP3_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(TRS_high_cr = case_when(
    is.na(TRS_cr) ~ NA_real_,  
    TRS_cr>q95_TRS_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(MPB_high_cr = case_when(
    is.na(MPB_cr) ~ NA_real_,  
    MPB_cr>q95_MPB_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(PPB_high_cr = case_when(
    is.na(PPB_cr) ~ NA_real_,  
    PPB_cr>q95_PPB_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(BPS_high_cr = case_when(
    is.na(BPS_cr) ~ NA_real_,  
    BPS_cr>q95_BPS_cr ~ 1,
    TRUE ~ 0)) %>%  
  mutate(X14D_high_cr = case_when(
    is.na(X14Dcr) ~ NA_real_,  
    X14Dcr>q95_X14Dcr ~ 1,
    TRUE ~ 0)) %>%
  mutate(DCB_high_cr = case_when(
    is.na(DCB_cr) ~ NA_real_,  
    DCB_cr>q95_DCB_cr ~ 1,
    TRUE ~ 0)) %>%  
  mutate(X2MH_high_cr = case_when(
    is.na(X2MHcr) ~ NA_real_,  
    X2MHcr>q95_X2MHcr ~ 1,
    TRUE ~ 0)) %>%
  mutate(X34M_high_cr = case_when(
    is.na(X34Mcr) ~ NA_real_,  
    X34Mcr>q95_X34Mcr ~ 1,
    TRUE ~ 0)) %>%
  mutate(AAM_high_cr = case_when(
    is.na(AAM_cr) ~ NA_real_,  
    AAM_cr>q95_AAM_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(ATC_high_cr = case_when(
    is.na(ATC_cr) ~ NA_real_,  
    ATC_cr>q95_ATC_cr ~ 1,
    TRUE ~ 0)) %>%  
  mutate(BMA_high_cr = case_when(
    is.na(BMA_cr) ~ NA_real_,  
    BMA_cr>q95_BMA_cr ~ 1,
    TRUE ~ 0)) %>%    
  mutate(CYM_high_cr = case_when(
    is.na(CYM_cr) ~ NA_real_,  
    CYM_cr>q95_CYM_cr ~ 1,
    TRUE ~ 0)) %>%  
  mutate(DHB_high_cr = case_when(
    is.na(DHB_cr) ~ NA_real_,  
    DHB_cr>q95_DHB_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(HP2_high_cr = case_when(
    is.na(HP2_cr) ~ NA_real_,  
    HP2_cr>q95_HP2_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(MAD_high_cr = case_when(
    is.na(MAD_cr) ~ NA_real_,  
    MAD_cr>q95_MAD_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(MB3_high_cr = case_when(
    is.na(MB3_cr) ~ NA_real_,  
    MB3_cr>q95_MB3_cr ~ 1,
    TRUE ~ 0)) %>%
  mutate(P01_high_cr = case_when(
    is.na(P01_cr) ~ NA_real_,  
    P01_cr>q95_P01_cr ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P02_high_cr = case_when(
    is.na(P02_cr) ~ NA_real_,  
    P02_cr>q95_P02_cr ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P03_high_cr = case_when(
    is.na(P03_cr) ~ NA_real_,  
    P03_cr>q95_P03_cr ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P04_high_cr = case_when(
    is.na(P04_cr) ~ NA_real_,  
    P04_cr>q95_P04_cr ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P05_high_cr = case_when(
    is.na(P05_cr) ~ NA_real_,  
    P05_cr>q95_P05_cr ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P06_high_cr = case_when(
    is.na(P06_cr) ~ NA_real_,  
    P06_cr>q95_P06_cr ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P07_high_cr = case_when(
    is.na(P07_cr) ~ NA_real_,  
    P07_cr>q95_P07_cr ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P10_high_cr = case_when(
    is.na(P10_cr) ~ NA_real_,  
    P10_cr>q95_P10_cr ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P17_high_cr = case_when(
    is.na(P17_cr) ~ NA_real_,  
    P17_cr>q95_P17_cr ~ 1,
    TRUE ~ 0)) %>%  
  mutate(P19_high_cr = case_when(
    is.na(P19_cr) ~ NA_real_,  
    P19_cr>q95_P19_cr ~ 1,
    TRUE ~ 0)) %>%  
  mutate(UCD_high_cr = case_when(
    is.na(UCD_cr) ~ NA_real_,  
    UCD_cr>q95_UCD_cr ~ 1,
    TRUE ~ 0))  

bigdata <- bigdata %>%
  select(-q95_URXMZP, 
         -q95_URXMBP, 
         -q95_URXMEP, 
         -q95_URXMIB, 
         -q95_URXMHH, 
         -q95_URXCNP, 
         -q95_URXCOP, 
         -q95_URXBPH,
         -q95_URXBP3,
         -q95_URXTRS,
         -q95_URXMPB,
         -q95_URXPPB,
         -q95_URXBPS,
         -q95_URX14D,
         -q95_URXDCB,
         -q95_URX2MH,
         -q95_URX34M,
         -q95_URXAAM,
         -q95_URXATC,
         -q95_URXBMA,
         -q95_URXCYM,
         -q95_URXDHB, 
         -q95_URXHP2,
         -q95_URXMAD,
         -q95_URXMB3,
         -q95_LBXVCF,
         -q95_LBXVTO,
         -q95_URXP01,
         -q95_URXP02,
         -q95_URXP03,
         -q95_URXP04,
         -q95_URXP05,
         -q95_URXP06,
         -q95_URXP07,
         -q95_URXP10,
         -q95_URXP17,
         -q95_URXP19,
         -q95_LBXPFOA,
         -q95_LBXPFOS,
         -q95_LBXPFHS,
         -q95_LBXMPAH,
         -q95_LBXPFDE,
         -q95_LBXPFNA,
         -q95_LBXBPB,
         -q95_LBXTHG,
         -q95_URXUCD,
         -q95_MZP_cr, 
         -q95_MBP_cr, 
         -q95_MEP_cr, 
         -q95_MIB_cr, 
         -q95_MHH_cr, 
         -q95_CNP_cr, 
         -q95_COP_cr, 
         -q95_BPH_cr,
         -q95_BP3_cr,
         -q95_TRS_cr,
         -q95_MPB_cr,
         -q95_PPB_cr,
         -q95_BPS_cr,
         -q95_X14Dcr,
         -q95_DCB_cr,
         -q95_X2MHcr,
         -q95_X34Mcr,
         -q95_AAM_cr,
         -q95_ATC_cr,
         -q95_BMA_cr,
         -q95_CYM_cr,
         -q95_DHB_cr, 
         -q95_HP2_cr,
         -q95_MAD_cr,
         -q95_MB3_cr,
         -q95_P01_cr,
         -q95_P02_cr,
         -q95_P03_cr,
         -q95_P04_cr,
         -q95_P05_cr,
         -q95_P06_cr,
         -q95_P07_cr,
         -q95_P10_cr,
         -q95_P17_cr,
         -q95_P19_cr,
         -q95_UCD_cr)
   
#write_rds(bigdata, "data/public_dataset.rds")

