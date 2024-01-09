# Pulls data that is shown in regression tables for paper

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

#make pretty names and also add an ordering variable
prettynames <- as.data.frame(matrix(c(
  "ln_14D", "2,5-dichlorophenol (p-dichlorobenzene)", 2.2,
  "ln_2MH", "2-methylhippuric acid (xylene)", 4.8,
  "ln_34M", "3-methylhippuric acid & 4-methylhippuric acid (xylene)", 4.9,
  "ln_AAM", "n-acetyl-S-(2-carbamoylethyl)-l-cysteine (acrylamide)", 4.3,
  "ln_ATC", "2-aminothiazoline-4-carboxylic acid (cyanide)", 4.5,
  "ln_BMA", "n-acetyl-S-(benzyl)-l-cysteine (toluene)", 4.7,
  "ln_BP3", "benzophenone-3", 2.3,
  "ln_BPB", "lead", 7.2,
  "ln_BPH", "bisphenol A", 2.4,
  "ln_BPS", "bisphenol S", 2.5,
  "ln_CNP", "mono(carboxynonyl) phthalate (di-isodecyl phthalate)", 1.5,
  "ln_COP", "mono(carboxyoctyl) phthalate(di-isononyl phthalate)", 1.6,
  "ln_CYM", "n-acetyl-s-(2-cyanoethyl)-l-cysteine (acrylonitrile)", 4.4,
  "ln_DCB", "2,4-dichlorophenol", 2.1,
  "ln_DHB", "n-acetyl-s-(3,4-dihidroxybutyl)-l-cysteine (1,3-butadiene)", 4.1,
  "ln_HP2", "n-acetyl-s-(2-hydroxypropyl)-l-cysteine (propylene oxide)", 4.6,
  "ln_MAD", "mandelic acid (styrene)", 4.7,
  "ln_MB3", "n-acetyl-s-(4-hydroxy-2-butenyl)-l-cysteine (1,3-butadiene)", 4.2,
  "ln_MBP", "mono-n-butyl phthalate (di-n-butyl phthalate)", 1.7,
  "ln_MEP", "mono-ethyl phthalate (di-ethyl phthalate)", 1.3,
  "ln_MHH", "mono-(2-ethyl-5-hydroxyhexyl) phthalate (di(2-ethylhexyl) phthalate)", 1.2,
  "ln_MIB", "mono-isobutyl phthalate (di-isobutyl phthalate)", 1.4,
  "ln_MPAH", "N-MeFOSAA", 6.1,
  "ln_MPB", "methyl paraben", 2.6,
  "ln_MZP", "mono-benzyl phthalate (benzylbutyl phthalate)", 1.1,
  "ln_P01", "1-hydroxynaphthalene (napthalene)", 5.4,
  "ln_P02", "2-hydroxynaphthalene (napthalene)", 5.4,
  "ln_P03", "3-hydroxyfluorene (fluorene)", 5.2,
  "ln_P04", "2-hydroxyfluorene (fluorene)", 5.1,
  "ln_P05", "3-phenanthrene (phenanthrene)", 5.7,
  "ln_P06", "1-hydroxyphenanthrene (phenanthrene)", 5.5,
  "ln_P07", "2-phenanthrene (phenanthrene)", 5.6,
  "ln_P10", "1-hydroxypyrene (pyrene)", 5.9,
  "ln_P17", "9-hydroxyfluorene (fluorene)", 5.3,
  "ln_P19", "4-phenanthrene (phenanthrene)", 5.8,
  "ln_PFDE", "PFDA", 6.2,
  "ln_PFHS", "PFHxS", 6.3,
  "ln_PFNA", "PFNA", 6.4,
  "ln_PFOA", "PFOA", 6.5,
  "ln_PFOS", "PFOS", 6.6,
  "ln_PPB", "propyl paraben", 2.7, 
  "ln_THG", "mercury", 7.3,
  "ln_TRS", "triclosan", 2.8,
  "ln_UCD", "cadmium", 7.1,
  "ln_VCF", "chloroform", 3.1,
  "ln_VTO", "toluene", 3.2
), ncol=3, byrow=TRUE))
names(prettynames)<- c("chemical", "prettyname", "ordvar")

prettynames <- prettynames %>%
  mutate(ordvar=as.numeric(ordvar))


# Load in the observation counts
obs_counts <- read_rds("data/obs_counts_for_tables.rds")


# Load in the P65 reg results from the RDC
table8 <- read_csv("RDC Output/tables/tables/Table_8.csv")

# Load in the quantile regression results from the RDC
table12 <- read_csv("RDC Output/tables/tables/Table_12.csv") 

table12 <- table12 %>%
  mutate(chemical=str_trim(str_sub(chemical_quantile, 1, 7)),
         quant=str_sub(chemical_quantile, -4, -1)) %>%
  mutate(creatinine_indic = case_when(is.na(coef_ln_UCR)==F ~ "yes",
                                      TRUE ~ "no"),
         cotinine_indic = case_when(is.na(coef_ln_COT)==F ~ "yes",
                                    TRUE ~ "no"))


##########################################################

# Start with the regular regression results
# Pull out the specs with both CA and time (but not diffs-in-diffs)
timedata <- table8 %>%
  filter(is.na(coef_CA)==F) %>%
  filter(is.na(coef_time)==F)

# Need to filter out the correct cases
# So for phthalates and phenols, want creatinine specs. 
# For urine VOCs, want specs that control for both creatinine and cotinine 
# For PAHs, want specs that control for both creatinine and cotinine 
# For cadmium, want specs that control for both creatinine and cotinine 
# take out the P65 chemicals with before and after data - those are going in a separate table
timedata <- timedata %>%
  mutate(incl= case_when(
    chemical=="ln_MEP" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_MIB" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_BP3" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_TRS" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_14D" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_DCB" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_MPB" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_PPB" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_BPS" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_34M" & creatinine_indic=="yes"  & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_CYM" & creatinine_indic=="yes"  & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_DHB" & creatinine_indic=="yes"  & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_HP2" & creatinine_indic=="yes"  & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_MAD" & creatinine_indic=="yes"  & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_MB3" & creatinine_indic=="yes"  & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_P04" & creatinine_indic=="yes" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_P06" & creatinine_indic=="yes" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_P10" & creatinine_indic=="yes" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_P05" & creatinine_indic=="yes" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_UCD" & creatinine_indic=="yes" & cotinine_indic=="yes" ~ "yes", 
    chemical=="ln_BPB" ~ "yes",
    chemical=="ln_THG" ~ "yes",
    chemical=="ln_PFDE" ~ "yes",
    chemical=="ln_PFHS" ~ "yes",
    chemical=="ln_PFNA" ~ "yes",
    chemical=="ln_PFOA" ~ "yes",
    chemical=="ln_PFOS" ~ "yes",
    chemical=="ln_MPAH" ~ "yes",
    TRUE ~ "no"
  )) %>%
  filter(incl=="yes") 

# merge in pretty names
timedata <- timedata %>%
  left_join(prettynames, by="chemical")

# merge in the observation counts
timedata <- timedata %>%
  left_join(obs_counts, by="chemical")

# Now pull in the quantile regression results
# Pull out the specs with both CA and time (but not diffs-in-diffs)
timedata_quantile <- table12 %>%
  filter(is.na(coef_CA)==F) %>%
  filter(is.na(coef_time)==F)

# Need to filter out the correct cases
timedata_quantile <- timedata_quantile %>%
  mutate(incl= case_when(
    chemical=="ln_MEP" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_MIB" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_BP3" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_TRS" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_14D" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_DCB" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_MPB" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_PPB" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_BPS" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_34M" & creatinine_indic=="yes"  & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_CYM" & creatinine_indic=="yes"  & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_DHB" & creatinine_indic=="yes"  & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_HP2" & creatinine_indic=="yes"  & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_MAD" & creatinine_indic=="yes"  & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_MB3" & creatinine_indic=="yes"  & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_P04" & creatinine_indic=="yes" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_P06" & creatinine_indic=="yes" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_P10" & creatinine_indic=="yes" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_P05" & creatinine_indic=="yes" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_UCD" & creatinine_indic=="yes" & cotinine_indic=="yes" ~ "yes", 
    chemical=="ln_BPB" ~ "yes",
    chemical=="ln_THG" ~ "yes",
    chemical=="ln_PFDE" ~ "yes",
    chemical=="ln_PFHS" ~ "yes",
    chemical=="ln_PFNA" ~ "yes",
    chemical=="ln_PFOA" ~ "yes",
    chemical=="ln_PFOS" ~ "yes",
    chemical=="ln_MPAH" ~ "yes",
    TRUE ~ "no"
  )) %>%
  filter(incl=="yes") 

# merge in pretty names
timedata_quantile <- timedata_quantile %>%
  left_join(prettynames, by="chemical")

# Make confidence intervals for CA and time
timedata_quantile <- timedata_quantile %>%
  mutate(CA_CI_low = coef_CA-1.96*se_CA,
         CA_CI_high = coef_CA+1.96*se_CA,
         time_CI_low = coef_time-1.96*se_time,
         time_CI_high = coef_time+1.96*se_time) 


###### Table 4 #####

P65_only_after_data <- timedata %>%
  filter(chemical %in% c("ln_14D", "ln_CYM", "ln_DHB", "ln_MB3", "ln_HP2", "ln_2MH", "ln_34M", "ln_P10", "ln_P03", "ln_P04", "ln_P17", "ln_P05", "ln_P06", "ln_P07", "ln_P19", "ln_BPB", "ln_UCD", "ln_THG")) %>%
  select(chemical, coef_CA, se_CA, p_CA, coef_time, se_time, p_time, prettyname, ordvar, num_obs)

P65_only_after_data <- P65_only_after_data %>%
  mutate(CA_CI_low = coef_CA-1.96*se_CA,
         CA_CI_high = coef_CA+1.96*se_CA,
         time_CI_low = coef_time-1.96*se_time,
         time_CI_high = coef_time+1.96*se_time)

P65_only_after_data <- P65_only_after_data %>%
  mutate(CA_pretty = paste0(as.character(signif(coef_CA,1)),"\n(", signif(CA_CI_low,1), ", ", signif(CA_CI_high,1), ")"),
         time_pretty = paste0(as.character(signif(coef_time,1)),"\n(", signif(time_CI_low,1), ", ", signif(time_CI_high,1), ")")) %>%
  select(prettyname, ordvar, CA_pretty, time_pretty, num_obs) %>%
  rename(chemical=prettyname,
         CA=CA_pretty,
         time=time_pretty)


P65_only_after_data_quantile <- timedata_quantile %>%
  filter(chemical %in% c("ln_14D", "ln_CYM", "ln_DHB", "ln_MB3", "ln_HP2", "ln_2MH", "ln_34M", "ln_P10", "ln_P03", "ln_P04", "ln_P17", "ln_P05", "ln_P06", "ln_P07", "ln_P19", "ln_BPB", "ln_UCD", "ln_THG")) %>%
  select(chemical, quant, coef_CA, CA_CI_low, CA_CI_high, coef_time, time_CI_low, time_CI_high, prettyname)


P65_only_after_data_quantile <- P65_only_after_data_quantile %>%
  mutate(CA_pretty = paste0(as.character(signif(coef_CA,1)),"\n(", signif(CA_CI_low,1), ", ", signif(CA_CI_high,1), ")"),
         time_pretty = paste0(as.character(signif(coef_time,1)),"\n(", signif(time_CI_low,1), ", ", signif(time_CI_high,1), ")")) %>%
  select(prettyname, quant, CA_pretty, time_pretty) %>%
  rename(chemical=prettyname,
         CA=CA_pretty,
         time=time_pretty)

P65_only_after_data_quantile <- P65_only_after_data_quantile %>%
  pivot_wider(names_from=quant, values_from=c(CA, time)) %>%
  select(chemical, CA_0.25, time_0.25, CA_0.75, time_0.75, CA_0.95, time_0.95)

paper_table4 <- P65_only_after_data %>%
  left_join(P65_only_after_data_quantile, by="chemical")



###### Table 5 #####
P65_not_listed_yet <- timedata %>%
  filter(chemical %in% c("ln_MEP", "ln_MIB", "ln_MPAH", "ln_PFDE", "ln_PFHS", "ln_PFNA", "ln_PFOA", "ln_PFOS", "ln_BP3", "ln_TRS", "ln_DCB", "ln_MPB", "ln_PPB", "ln_BPS", "ln_MAD")) %>%
  select(chemical, coef_CA, se_CA, p_CA, coef_time, se_time, p_time, prettyname, ordvar, num_obs)


P65_not_listed_yet <- P65_not_listed_yet %>%
  mutate(CA_CI_low = coef_CA-1.96*se_CA,
         CA_CI_high = coef_CA+1.96*se_CA,
         time_CI_low = coef_time-1.96*se_time,
         time_CI_high = coef_time+1.96*se_time)

P65_not_listed_yet <- P65_not_listed_yet %>%
  mutate(CA_pretty = paste0(as.character(signif(coef_CA,1)),"\n(", signif(CA_CI_low,1), ", ", signif(CA_CI_high,1), ")"),
         time_pretty = paste0(as.character(signif(coef_time,1)),"\n(", signif(time_CI_low,1), ", ", signif(time_CI_high,1), ")")) %>%
  select(prettyname, ordvar, num_obs, CA_pretty, time_pretty) %>%
  rename(chemical=prettyname,
         CA=CA_pretty,
         time=time_pretty)

# Now pull in the quantile regression results
P65_only_before_data_quantile <- timedata_quantile %>%
  filter(chemical %in% c("ln_MEP", "ln_MIB", "ln_MPAH", "ln_PFDE", "ln_PFHS", "ln_PFNA", "ln_PFOA", "ln_PFOS", "ln_BP3", "ln_TRS", "ln_DCB", "ln_MPB", "ln_PPB", "ln_BPS", "ln_MAD")) %>%
  select(chemical, quant, coef_CA, CA_CI_low, CA_CI_high, coef_time, time_CI_low, time_CI_high, prettyname)

P65_only_before_data_quantile <- P65_only_before_data_quantile %>%
  mutate(CA_pretty = paste0(as.character(signif(coef_CA,1)),"\n(", signif(CA_CI_low,1), ", ", signif(CA_CI_high,1), ")"),
         time_pretty = paste0(as.character(signif(coef_time,1)),"\n(", signif(time_CI_low,1), ", ", signif(time_CI_high,1), ")")) %>%
  select(prettyname, quant, CA_pretty, time_pretty) %>%
  rename(chemical=prettyname,
         CA=CA_pretty,
         time=time_pretty)

P65_only_before_data_quantile <- P65_only_before_data_quantile %>%
  pivot_wider(names_from=quant, values_from=c(CA, time)) %>%
  select(chemical, CA_0.25, time_0.25, CA_0.75, time_0.75, CA_0.95, time_0.95)

paper_table5 <- P65_not_listed_yet %>%
  left_join(P65_only_before_data_quantile, by="chemical")


# Now look at the P65 specs (diffs-in-diffs)

########## Table 2 ###########

P65specs <- table8 %>%
  filter(chemical %in% c("ln_MZP", "ln_MBP", "ln_MHH", "ln_CNP", "ln_COP", "ln_BPH", "ln_AAM", "ln_ATC", "ln_BMA", "ln_VCF", "ln_VTO","ln_P02")) %>%
  filter(is.na(coef_CA)==F) %>%
  filter(is.na(coef_time)==T)


P65specs <- P65specs %>%
  mutate(coef_P65 =  case_when(
    chemical=="ln_MZP" ~ coef_P65_URXMZP,
    chemical=="ln_MBP" ~ coef_P65_URXMBP,
    chemical=="ln_MHH" ~ coef_P65_URXMHH,
    chemical=="ln_CNP" ~ coef_P65_URXCNP,
    chemical=="ln_COP" ~ coef_P65_URXCOP,
    chemical=="ln_BPH" ~ coef_P65_URXBPH,
    chemical=="ln_AAM" ~ coef_P65_URXAAM,
    chemical=="ln_ATC" ~ coef_P65_URXATC,
    chemical=="ln_BMA" ~ coef_P65_URXBMA,
    chemical=="ln_VCF" ~ coef_P65_LBXVCF,
    chemical=="ln_VTO" ~ coef_P65_LBXVTO,
    chemical=="ln_P02" ~ coef_P65_URXP02),
    se_P65 = case_when(
      chemical=="ln_MZP" ~ se_P65_URXMZP,
      chemical=="ln_MBP" ~ se_P65_URXMBP,
      chemical=="ln_MHH" ~ se_P65_URXMHH,
      chemical=="ln_CNP" ~ se_P65_URXCNP,
      chemical=="ln_COP" ~ se_P65_URXCOP,
      chemical=="ln_BPH" ~ se_P65_URXBPH,
      chemical=="ln_AAM" ~ se_P65_URXAAM,
      chemical=="ln_ATC" ~ se_P65_URXATC,
      chemical=="ln_BMA" ~ se_P65_URXBMA,
      chemical=="ln_VCF" ~ se_P65_LBXVCF,
      chemical=="ln_VTO" ~ se_P65_LBXVTO,
      chemical=="ln_P02" ~ se_P65_URXP02),
    p_P65 = case_when(
      chemical=="ln_MZP" ~ p_P65_URXMZP,
      chemical=="ln_MBP" ~ p_P65_URXMBP,
      chemical=="ln_MHH" ~ p_P65_URXMHH,
      chemical=="ln_CNP" ~ p_P65_URXCNP,
      chemical=="ln_COP" ~ p_P65_URXCOP,
      chemical=="ln_BPH" ~ p_P65_URXBPH,
      chemical=="ln_AAM" ~ p_P65_URXAAM,
      chemical=="ln_ATC" ~ p_P65_URXATC,
      chemical=="ln_BMA" ~ p_P65_URXBMA,
      chemical=="ln_VCF" ~ p_P65_LBXVCF,
      chemical=="ln_VTO" ~ p_P65_LBXVTO,
      chemical=="ln_P02" ~ p_P65_URXP02),
    coef_P65_CA = case_when(
      chemical=="ln_MZP" ~ coef_P65_URXMZP_CA,
      chemical=="ln_MBP" ~ coef_P65_URXMBP_CA,
      chemical=="ln_MHH" ~ coef_P65_URXMHH_CA,
      chemical=="ln_CNP" ~ coef_P65_URXCNP_CA,
      chemical=="ln_COP" ~ coef_P65_URXCOP_CA,
      chemical=="ln_BPH" ~ coef_P65_URXBPH_CA,
      chemical=="ln_AAM" ~ coef_P65_URXAAM_CA,
      chemical=="ln_ATC" ~ coef_P65_URXATC_CA,
      chemical=="ln_BMA" ~ coef_P65_URXBMA_CA,
      chemical=="ln_VCF" ~ coef_P65_LBXVCF_CA,
      chemical=="ln_VTO" ~ coef_P65_LBXVTO_CA,
      chemical=="ln_P02" ~ coef_P65_URXP02_CA),
    se_P65_CA = case_when(
      chemical=="ln_MZP" ~ se_P65_URXMZP_CA,
      chemical=="ln_MBP" ~ se_P65_URXMBP_CA,
      chemical=="ln_MHH" ~ se_P65_URXMHH_CA,
      chemical=="ln_CNP" ~ se_P65_URXCNP_CA,
      chemical=="ln_COP" ~ se_P65_URXCOP_CA,
      chemical=="ln_BPH" ~ se_P65_URXBPH_CA,
      chemical=="ln_AAM" ~ se_P65_URXAAM_CA,
      chemical=="ln_ATC" ~ se_P65_URXATC_CA,
      chemical=="ln_BMA" ~ se_P65_URXBMA_CA,
      chemical=="ln_VCF" ~ se_P65_LBXVCF_CA,
      chemical=="ln_VTO" ~ se_P65_LBXVTO_CA,
      chemical=="ln_P02" ~ se_P65_URXP02_CA),
    p_P65_CA = case_when(
      chemical=="ln_MZP" ~ p_P65_URXMZP_CA,
      chemical=="ln_MBP" ~ p_P65_URXMBP_CA,
      chemical=="ln_MHH" ~ p_P65_URXMHH_CA,
      chemical=="ln_CNP" ~ p_P65_URXCNP_CA,
      chemical=="ln_COP" ~ p_P65_URXCOP_CA,
      chemical=="ln_BPH" ~ p_P65_URXBPH_CA,
      chemical=="ln_AAM" ~ p_P65_URXAAM_CA,
      chemical=="ln_ATC" ~ p_P65_URXATC_CA,
      chemical=="ln_BMA" ~ p_P65_URXBMA_CA,
      chemical=="ln_VCF" ~ p_P65_LBXVCF_CA,
      chemical=="ln_VTO" ~ p_P65_LBXVTO_CA,
      chemical=="ln_P02" ~ p_P65_URXP02_CA))


#make the test statistic - we want to report chisq/[(lambda1+lambda2)/2] - see Lumley and Scott (2017)
# we also want to take out the ddf and p-value (df is always 2 because 2 test terms)
P65specs <- P65specs %>%
  mutate(working2logLR=lrt_chi/((lrt_lambda1+lrt_lambda2)/2))

P65specs <- P65specs %>%
  select(chemical, creatinine_indic, cotinine_indic, coef_P65, se_P65, p_P65, coef_P65_CA, se_P65_CA, p_P65_CA, coef_CA, se_CA, p_CA, working2logLR, lrt_p, lrt_ddf)

P65specs <- P65specs %>%
  mutate(incl= case_when(
    chemical=="ln_MZP" & creatinine_indic=="yes" ~ "yes", 
    chemical=="ln_MBP" & creatinine_indic=="yes" ~ "yes",  
    chemical=="ln_MHH" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_CNP" & creatinine_indic=="yes" ~ "yes",  
    chemical=="ln_COP" & creatinine_indic=="yes" ~ "yes",  
    chemical=="ln_BPH" & creatinine_indic=="yes" ~ "yes",  
    chemical=="ln_AAM" & creatinine_indic=="yes" & cotinine_indic=="yes" ~ "yes",  
    chemical=="ln_ATC" & creatinine_indic=="yes" & cotinine_indic=="yes" ~ "yes",  
    chemical=="ln_VCF" ~ "yes", 
    chemical=="ln_VTO" & cotinine_indic=="yes" ~ "yes",  
    chemical=="ln_P02" & creatinine_indic=="yes" & cotinine_indic=="yes" ~ "yes",
    TRUE ~ "no")) %>%
  filter(incl=="yes") %>%
  select(-incl, -creatinine_indic, -cotinine_indic)



#look at change for CA versus change for non-CA
pre_to_post_changes <- P65specs %>%
  mutate(CA_delta=coef_P65+coef_P65_CA,
         non_delta=coef_P65,
         CA_sig=case_when(lrt_p<0.05 ~ "yes",
                          TRUE ~ "no"),
         non_sig=case_when(p_P65<0.05 ~ "yes",
                           TRUE ~ "no"))%>%
  select(chemical, CA_delta, non_delta, CA_sig, non_sig, coef_P65_CA)


P65_reg_table <- P65specs %>%
  left_join(obs_counts, by="chemical") %>%
  left_join(prettynames, by="chemical") %>%
  mutate(CA_CI_low = coef_CA-1.96*se_CA,
         CA_CI_high = coef_CA+1.96*se_CA,
         P65_CI_low = coef_P65-1.96*se_P65,
         P65_CI_high = coef_P65+1.96*se_P65,
         P65_CA_CI_low = coef_P65_CA-1.96*se_P65_CA,
         P65_CA_CI_high = coef_P65_CA+1.96*se_P65_CA)


P65_reg_table <- P65_reg_table %>%
  mutate(CA_pretty = paste0(signif(coef_CA,1), "\n(", signif(CA_CI_low,1), ", ", signif(CA_CI_high,1), ")"),
         P65_pretty = paste0(signif(coef_P65,1), "\n(", signif(P65_CI_low,1), ", ", signif(P65_CI_high,1), ")"),
         P65_CA_pretty = paste0(signif(coef_P65_CA,1), "\n(", signif(P65_CA_CI_low,1), ", ", signif(P65_CA_CI_high,1), ")"),
         lrt_pretty = paste0(as.character(signif(working2logLR, 1)),"\np=",as.character(signif(lrt_p,1)))) %>%
  select(prettyname, CA_pretty, P65_pretty, P65_CA_pretty, lrt_pretty, ordvar, num_obs) %>%
  rename(chemical=prettyname,
         CA=CA_pretty,
         P65=P65_pretty)

paper_table2 <- P65_reg_table

############### Table 3 ##############################

# now look at the quantile results for the diffs-in-diffs
diffs_quantile <- table12 %>%
  filter(is.na(coef_CA)==F) %>%
  filter(is.na(coef_time)==T)

# Need to filter out the correct cases
diffs_quantile <- diffs_quantile %>%
  mutate(incl= case_when(
    chemical=="ln_MZP" & creatinine_indic=="yes" ~ "yes", 
    chemical=="ln_MBP" & creatinine_indic=="yes" ~ "yes",  
    chemical=="ln_MHH" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_CNP" & creatinine_indic=="yes" ~ "yes",  
    chemical=="ln_COP" & creatinine_indic=="yes" ~ "yes",
    chemical=="ln_BPH" & creatinine_indic=="yes" ~ "yes",  
    chemical=="ln_AAM" & creatinine_indic=="yes" & cotinine_indic=="yes" ~ "yes",  
    chemical=="ln_ATC" & creatinine_indic=="yes" & cotinine_indic=="yes" ~ "yes",  
    chemical=="ln_VCF" ~ "yes", 
    chemical=="ln_VTO" & cotinine_indic=="yes" ~ "yes",  
    chemical=="ln_P02" & creatinine_indic=="yes" & cotinine_indic=="yes" ~ "yes",
    TRUE ~ "no"
  )) %>%
  filter(incl=="yes") 


# merge in pretty names and observation counts
diffs_quantile <- diffs_quantile %>%
  left_join(prettynames, by="chemical") %>%
  left_join(obs_counts, by="chemical")

diffs_quantile <- diffs_quantile %>%
  mutate(coef_P65 =  case_when(
    chemical=="ln_MZP" ~ coef_P65_URXMZP,
    chemical=="ln_MBP" ~ coef_P65_URXMBP,
    chemical=="ln_MHH" ~ coef_P65_URXMHH,
    chemical=="ln_CNP" ~ coef_P65_URXCNP,
    chemical=="ln_COP" ~ coef_P65_URXCOP,
    chemical=="ln_BPH" ~ coef_P65_URXBPH,
    chemical=="ln_AAM" ~ coef_P65_URXAAM,
    chemical=="ln_ATC" ~ coef_P65_URXATC,
    chemical=="ln_BMA" ~ coef_P65_URXBMA,
    chemical=="ln_VCF" ~ coef_P65_LBXVCF,
    chemical=="ln_VTO" ~ coef_P65_LBXVTO,
    chemical=="ln_P02" ~ coef_P65_URXP02),
    se_P65 = case_when(
      chemical=="ln_MZP" ~ se_P65_URXMZP,
      chemical=="ln_MBP" ~ se_P65_URXMBP,
      chemical=="ln_MHH" ~ se_P65_URXMHH,
      chemical=="ln_CNP" ~ se_P65_URXCNP,
      chemical=="ln_COP" ~ se_P65_URXCOP,
      chemical=="ln_BPH" ~ se_P65_URXBPH,
      chemical=="ln_AAM" ~ se_P65_URXAAM,
      chemical=="ln_ATC" ~ se_P65_URXATC,
      chemical=="ln_BMA" ~ se_P65_URXBMA,
      chemical=="ln_VCF" ~ se_P65_LBXVCF,
      chemical=="ln_VTO" ~ se_P65_LBXVTO,
      chemical=="ln_P02" ~ se_P65_URXP02),
    coef_P65_CA = case_when(
      chemical=="ln_MZP" ~ coef_P65_URXMZP_CA,
      chemical=="ln_MBP" ~ coef_P65_URXMBP_CA,
      chemical=="ln_MHH" ~ coef_P65_URXMHH_CA,
      chemical=="ln_CNP" ~ coef_P65_URXCNP_CA,
      chemical=="ln_COP" ~ coef_P65_URXCOP_CA,
      chemical=="ln_BPH" ~ coef_P65_URXBPH_CA,
      chemical=="ln_AAM" ~ coef_P65_URXAAM_CA,
      chemical=="ln_ATC" ~ coef_P65_URXATC_CA,
      chemical=="ln_BMA" ~ coef_P65_URXBMA_CA,
      chemical=="ln_VCF" ~ coef_P65_LBXVCF_CA,
      chemical=="ln_VTO" ~ coef_P65_LBXVTO_CA,
      chemical=="ln_P02" ~ coef_P65_URXP02_CA),
    se_P65_CA = case_when(
      chemical=="ln_MZP" ~ se_P65_URXMZP_CA,
      chemical=="ln_MBP" ~ se_P65_URXMBP_CA,
      chemical=="ln_MHH" ~ se_P65_URXMHH_CA,
      chemical=="ln_CNP" ~ se_P65_URXCNP_CA,
      chemical=="ln_COP" ~ se_P65_URXCOP_CA,
      chemical=="ln_BPH" ~ se_P65_URXBPH_CA,
      chemical=="ln_AAM" ~ se_P65_URXAAM_CA,
      chemical=="ln_ATC" ~ se_P65_URXATC_CA,
      chemical=="ln_BMA" ~ se_P65_URXBMA_CA,
      chemical=="ln_VCF" ~ se_P65_LBXVCF_CA,
      chemical=="ln_VTO" ~ se_P65_LBXVTO_CA,
      chemical=="ln_P02" ~ se_P65_URXP02_CA)) %>%
  select(chemical, quant, prettyname, ordvar, num_obs, coef_P65, se_P65, coef_P65_CA, se_P65_CA, coef_CA, se_CA, cov_CA_P65)

# make confidence intervals
diffs_quantile <- diffs_quantile %>%
  mutate(CA_CI_low = coef_CA-1.96*se_CA,
         CA_CI_high = coef_CA+1.96*se_CA,
         P65_CI_low = coef_P65-1.96*se_P65,
         P65_CI_high = coef_P65+1.96*se_P65,
         P65_CA_CI_low = coef_P65_CA-1.96*se_P65_CA,
         P65_CA_CI_high = coef_P65_CA+1.96*se_P65_CA,
         sumP65CA = coef_P65+coef_P65_CA,
         sum_se=sqrt((se_P65^2)+(se_P65_CA^2)+2*cov_CA_P65),
         sum_CI_low = sumP65CA - 1.96*sum_se,
         sum_CI_high = sumP65CA + 1.96*sum_se)

diffs_quantile <- diffs_quantile %>%
  mutate(CA_pretty = paste0(signif(coef_CA,1), "\n(", signif(CA_CI_low,1), ", ", signif(CA_CI_high,1), ")"),
       P65_pretty = paste0(signif(coef_P65,1), "\n(", signif(P65_CI_low,1), ", ", signif(P65_CI_high,1), ")"),
       P65_CA_pretty = paste0(signif(coef_P65_CA,1), "\n(", signif(P65_CA_CI_low,1), ", ", signif(P65_CA_CI_high,1), ")"),
       `P65+P65*CA` = paste0(signif(sumP65CA,1), "\n(", signif(sum_CI_low,1), ",", signif(sum_CI_high,1), ")")) %>%
  select(-chemical) %>%
  rename(chemical=prettyname,
         CA=CA_pretty,
         P65=P65_pretty) %>%
  select(chemical, quant, ordvar, num_obs, CA, P65, P65_CA_pretty,`P65+P65*CA`)

diffs_quantile <- diffs_quantile %>%
  pivot_wider(names_from=quant, values_from=c(CA, P65, P65_CA_pretty,`P65+P65*CA`))

paper_table3 <- diffs_quantile
