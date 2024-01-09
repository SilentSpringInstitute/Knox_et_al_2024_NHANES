# Sensitivity analysis to check if our controlling for creatinine substantively changes the results
# WRITTEN IN R VERSION: R version 4.0.3 (2020-10-10)

library(tidyverse)

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

# Only need to look at the urine chemicals
# Want the specs with and without creatinine
timedata <- timedata %>%
  mutate(incl= case_when(
    chemical=="ln_MEP" ~ "yes",
    chemical=="ln_MIB" ~ "yes",
    chemical=="ln_BP3" ~ "yes",
    chemical=="ln_TRS" ~ "yes",
    chemical=="ln_14D" ~ "yes",
    chemical=="ln_DCB" ~ "yes",
    chemical=="ln_MPB" ~ "yes",
    chemical=="ln_PPB" ~ "yes",
    chemical=="ln_BPS" ~ "yes",
    chemical=="ln_34M" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_CYM" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_DHB" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_HP2" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_MAD" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_MB3" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_P04" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_P06" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_P10" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_P05" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_UCD" & cotinine_indic=="yes" ~ "yes", 
    TRUE ~ "no"
  )) %>%
  filter(incl=="yes") 

# merge in pretty names
timedata <- timedata %>%
  left_join(prettynames, by="chemical")


# table 4 regular regression checking
P65_only_after_data <- timedata %>%
  filter(chemical %in% c("ln_14D", "ln_CYM", "ln_DHB", "ln_MB3", "ln_HP2", "ln_2MH", "ln_34M", "ln_P10", "ln_P03", "ln_P04", "ln_P17", "ln_P05", "ln_P06", "ln_P07", "ln_P19", "ln_BPB", "ln_UCD", "ln_THG")) %>%
  select(chemical, coef_CA, p_CA, coef_time, p_time, prettyname, creatinine_indic) %>%
  mutate(CA_sig=case_when(p_CA<0.05 ~ "yes",
                          TRUE ~ "no"),
         time_sig=case_when(p_time<0.05 ~ "yes",
                            TRUE ~ "no")
  ) %>%
  select(-p_CA, -p_time) %>%
  pivot_wider(names_from=creatinine_indic, values_from=c(coef_CA, CA_sig, coef_time, time_sig)) %>%
  mutate(CA_check=case_when(
    sign(coef_CA_no)==sign(coef_CA_yes) & CA_sig_no==CA_sig_yes ~ "yes",
    CA_sig_no=="no" & CA_sig_yes=="no" ~ "yes",
    TRUE ~ "no"),
    time_check=case_when(
      sign(coef_time_no)==sign(coef_time_yes) & time_sig_no==time_sig_yes ~ "yes",
      time_sig_no=="no" & time_sig_yes=="no" ~ "yes",
      TRUE ~ "no"
    ))
# For chemicals for which we only have "after" data, controlling for creatinine has no impact on the CA results.
# It changes the time results for:
# 3-methylhippuric acid & 4-methylhippuric acid (xylene) - both time coefficients negative but only sig if don't include creatinine
# n-acetyl-s-(3,4-dihidroxybutyl)-l-cysteine (1,3-butadiene) - both time coefficients positive but only sig if include creatinine
# n-acetyl-s-(2-hydroxypropyl)-l-cysteine (propylene oxide)- both time coefficients negative but only sig if don't include creatinine
# n-acetyl-s-(4-hydroxy-2-butenyl)-l-cysteine (1,3-butadiene)- both time coefficients negative but only sig if don't include creatinine


# Now pull in the quantile regression results
# Pull out the specs with both CA and time (but not diffs-in-diffs)
timedata_quantile <- table12 %>%
  filter(is.na(coef_CA)==F) %>%
  filter(is.na(coef_time)==F)

# Need to filter out the correct cases
timedata_quantile <- timedata_quantile %>%
  mutate(incl= case_when(
    chemical=="ln_MEP" ~ "yes",
    chemical=="ln_MIB" ~ "yes",
    chemical=="ln_BP3" ~ "yes",
    chemical=="ln_BP3" ~ "yes",
    chemical=="ln_BP3" ~ "yes",
    chemical=="ln_BP3" ~ "yes",
    chemical=="ln_TRS" ~ "yes",
    chemical=="ln_14D" ~ "yes",
    chemical=="ln_DCB" ~ "yes",
    chemical=="ln_MPB" ~ "yes",
    chemical=="ln_PPB" ~ "yes",
    chemical=="ln_BPS" ~ "yes",
    chemical=="ln_34M" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_CYM" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_DHB" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_HP2" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_MAD" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_MB3" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_P04" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_P06" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_P10" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_P05" & cotinine_indic=="yes" ~ "yes",
    chemical=="ln_UCD" & cotinine_indic=="yes" ~ "yes", 
    TRUE ~ "no"
  )) %>%
  filter(incl=="yes") 

# merge in pretty names
timedata_quantile <- timedata_quantile %>%
  left_join(prettynames, by="chemical") %>%
  mutate(CA_CI_low = coef_CA-1.96*se_CA,
         CA_CI_high = coef_CA+1.96*se_CA,
         time_CI_low = coef_time-1.96*se_time,
         time_CI_high = coef_time+1.96*se_time) 
  

# table 4 quantile regression checking
P65_only_after_data_quantile <- timedata_quantile %>%
  filter(chemical %in% c("ln_14D", "ln_CYM", "ln_DHB", "ln_MB3", "ln_HP2", "ln_2MH", "ln_34M", "ln_P10", "ln_P03", "ln_P04", "ln_P17", "ln_P05", "ln_P06", "ln_P07", "ln_P19", "ln_BPB", "ln_UCD", "ln_THG")) %>%
  select(chemical, quant, coef_CA, CA_CI_low, CA_CI_high, coef_time, time_CI_low, time_CI_high, prettyname, creatinine_indic)

P65_only_after_data_quantile <- P65_only_after_data_quantile %>%
  mutate(CA_sig=case_when(CA_CI_low<0 & CA_CI_high>0 ~ "no",
                          TRUE ~ "yes"),
         time_sig=case_when(time_CI_low<0 & time_CI_high>0 ~ "no",
                            TRUE ~ "yes")) %>%
  select(-CA_CI_low, -CA_CI_high, -time_CI_low, -time_CI_high) %>%
  pivot_wider(names_from=creatinine_indic, values_from=c(coef_CA, CA_sig, coef_time, time_sig)) %>%
  mutate(CA_check=case_when(
    sign(coef_CA_no)==sign(coef_CA_yes) & CA_sig_no==CA_sig_yes ~ "yes",
    CA_sig_no=="no" & CA_sig_yes=="no" ~ "yes",
    TRUE ~ "no"),
    time_check=case_when(
      sign(coef_time_no)==sign(coef_time_yes) & time_sig_no==time_sig_yes ~ "yes",
      time_sig_no=="no" & time_sig_yes=="no" ~ "yes",
      TRUE ~ "no"
    ))

#creatinine sensitivity check for quantile results for chemicals with only "after" data
# n-acetyl-s-(2-cyanoethyl)-l-cysteine (acrylonitrile)25th: CA coefficient + in both cases but only sig if control for creatinine
# n-acetyl-s-(2-cyanoethyl)-l-cysteine (acrylonitrile)25th: time coefficient - in both cases but only sig if do not control for creatinine
# 3-methylhippuric acid & 4-methylhippuric acid (xylene) 25th: CA coefficient not sig if do not control for creatinine, + and sig if control for creatinine
# cadmium 25th: CA coefficient + in both cases but only sig if control for creatinine
# n-acetyl-s-(4-hydroxy-2-butenyl)-l-cysteine (1,3-butadiene) 75th: CA coefficient + in both cases but only sig if control for creatinine
# 1-hydroxypyrene (pyrene) 95th: CA coefficient - in both cases but only sig if control for creatinine
# n-acetyl-s-(3,4-dihidroxybutyl)-l-cysteine (1,3-butadiene) 25th: time coefficient + in both cases but only sig if control for creatinine
# n-acetyl-s-(2-cyanoethyl)-l-cysteine (acrylonitrile) 75th: time coefficient + in both cases but only sig if control for creatinine
# n-acetyl-s-(3,4-dihidroxybutyl)-l-cysteine (1,3-butadiene) 75th: time coefficient + in both cases but only sig if control for creatinine
# 3-methylhippuric acid & 4-methylhippuric acid (xylene) 95th: time coefficient - in both cases but only sig if do not control for creatinine
# n-acetyl-s-(2-cyanoethyl)-l-cysteine (acrylonitrile) 95th: time coefficient + in both cases but only sig if control for creatinine
# n-acetyl-s-(3,4-dihidroxybutyl)-l-cysteine (1,3-butadiene) 95th: time coefficient not sig if do not control for creatinine, + and sig if control for creatinine
# n-acetyl-s-(4-hydroxy-2-butenyl)-l-cysteine (1,3-butadiene) 95th: time coefficient - but only sig if do not control for creatinine


###### Creatinine check for Table 5 - unlisted chemicals, measured in urine #####
# regular regression results
P65_not_listed_yet <- timedata %>%
  filter(chemical %in% c("ln_MEP", "ln_MIB", "ln_BP3", "ln_TRS", "ln_DCB", "ln_MPB", "ln_PPB", "ln_BPS", "ln_MAD")) %>%
  select(chemical, coef_CA, se_CA, p_CA, coef_time, se_time, p_time, prettyname, creatinine_indic) %>%
  mutate(CA_sig=case_when(p_CA<0.05 ~ "yes",
                          TRUE ~ "no"),
         time_sig=case_when(p_time<0.05 ~ "yes",
                            TRUE ~ "no")
  ) %>%
  select(-se_CA, -se_time, -p_CA, -p_time) %>%
  pivot_wider(names_from=creatinine_indic, values_from=c(coef_CA, CA_sig, coef_time, time_sig)) %>%
  mutate(CA_check=case_when(
    sign(coef_CA_no)==sign(coef_CA_yes) & CA_sig_no==CA_sig_yes ~ "yes",
    CA_sig_no=="no" & CA_sig_yes=="no" ~ "yes",
    TRUE ~ "no"),
    time_check=case_when(
      sign(coef_time_no)==sign(coef_time_yes) & time_sig_no==time_sig_yes ~ "yes",
      time_sig_no=="no" & time_sig_yes=="no" ~ "yes",
      TRUE ~ "no"
    ))

# For chemicals for which we only have "before" data, controlling for creatinine only changes one CA result:
# mandelic acid (styrene): CA coefficient is + in both cases but only sig if control for creatinine
# It changes the time results for:
# mandelic acid (styrene): time coefficient is - in both cases but only sig if do not control for creatinine
# benzophenone-3: time coefficient is + in both cases but only sig if control for creatinine

# quantile regression results
P65_only_before_data_quantile <- timedata_quantile %>%
  filter(chemical %in% c("ln_MEP", "ln_MIB", "ln_BP3", "ln_TRS", "ln_DCB", "ln_MPB", "ln_PPB", "ln_BPS", "ln_MAD")) %>%
  select(chemical, quant, coef_CA, CA_CI_low, CA_CI_high, coef_time, time_CI_low, time_CI_high, prettyname, creatinine_indic)

P65_only_before_data_quantile <- P65_only_before_data_quantile %>%
  mutate(CA_sig=case_when(CA_CI_low<0 & CA_CI_high>0 ~ "no",
                          TRUE ~ "yes"),
         time_sig=case_when(time_CI_low<0 & time_CI_high>0 ~ "no",
                            TRUE ~ "yes")) %>%
  select(-CA_CI_low, -CA_CI_high, -time_CI_low, -time_CI_high) %>%
  pivot_wider(names_from=creatinine_indic, values_from=c(coef_CA, CA_sig, coef_time, time_sig)) %>%
  mutate(CA_check=case_when(
    sign(coef_CA_no)==sign(coef_CA_yes) & CA_sig_no==CA_sig_yes ~ "yes",
    CA_sig_no=="no" & CA_sig_yes=="no" ~ "yes",
    TRUE ~ "no"),
    time_check=case_when(
      sign(coef_time_no)==sign(coef_time_yes) & time_sig_no==time_sig_yes ~ "yes",
      time_sig_no=="no" & time_sig_yes=="no" ~ "yes",
      TRUE ~ "no"
    ))

#creatinine sensitivity check for quantile results for chemicals with only "before" data
# propyl paraben 95th: CA coefficient + in both cases but only sig if do not control for creatinine
# propyl paraben 95th: time coefficient - in both cases but only sig if do not control for creatinine
# mono-ethyl phthalate (di-ethyl phthalate) 95th: CA coefficient - in both cases but only sig if do not control for creatinine
# 2,4-dichlorophenol 95th: CA coefficient - in both cases but only sig if do not control for creatinine
# bisphenol S 75th: time coefficient + in both cases buts only sig if control for creatinine


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
    chemical=="ln_MZP" ~ "yes", 
    chemical=="ln_MBP" ~ "yes",  
    chemical=="ln_MHH" ~ "yes",
    chemical=="ln_CNP" ~ "yes",  
    chemical=="ln_COP" ~ "yes",  
    chemical=="ln_BPH" ~ "yes",  
    chemical=="ln_AAM" & cotinine_indic=="yes" ~ "yes",  
    chemical=="ln_ATC" & cotinine_indic=="yes" ~ "yes",  
    chemical=="ln_P02" & cotinine_indic=="yes" ~ "yes",
    TRUE ~ "no")) %>%
  filter(incl=="yes") %>%
  select(-incl, -cotinine_indic)


P65specs <- P65specs %>%
  mutate(CA_delta=coef_P65+coef_P65_CA,
         non_delta=coef_P65,
         CA_delta_sig=case_when(lrt_p<0.05 ~ "yes",
                          TRUE ~ "no"),
         non_delta_sig=case_when(p_P65<0.05 ~ "yes",
                           TRUE ~ "no"),
         CA_sig=case_when(p_CA<0.05 ~ "yes",
                          TRUE ~ "no"))%>%
  select(chemical, creatinine_indic, CA_delta, non_delta, CA_delta_sig, non_delta_sig, coef_CA, CA_sig)

table2_check <- P65specs %>%
  pivot_wider(names_from=creatinine_indic, values_from=c(coef_CA, CA_sig, CA_delta, CA_delta_sig, non_delta, non_delta_sig)) %>%
  mutate(CA_check=case_when(
    sign(coef_CA_no)==sign(coef_CA_yes) & CA_sig_no==CA_sig_yes ~ "yes",
    CA_sig_no=="no" & CA_sig_yes=="no" ~ "yes",
    TRUE ~ "no"),
    CA_delta_check=case_when(
      sign(CA_delta_no)==sign(CA_delta_yes) & CA_delta_sig_no==CA_delta_sig_yes ~ "yes",
      CA_delta_no=="no" & CA_delta_yes=="no" ~ "yes",
      TRUE ~ "no"),
    non_delta_check=case_when(
      sign(non_delta_no)==sign(non_delta_yes) & non_delta_sig_no==non_delta_sig_yes ~ "yes",
      non_delta_no=="no" & non_delta_yes=="no" ~ "yes",
      TRUE ~ "no"))

#Only differences
# For CA versus rest of U.S. before listing - nothing changes
# For the delta from pre- to post-listing for rest of US - nothing changes
# For the delta from pre- to post-listing for CA:
# CNP [mono(carboxynonyl) phthalate/di-isodecyl phthalate] - CA concentrations decrease post-listing, but only sig if don't control for creatinine
# AAM [acrylamide] - CA concentrations increase post-listing, but only sig if control for creatinine ***


############### Table 3 ##############################

# now look at the quantile results for the diffs-in-diffs
diffs_quantile <- table12 %>%
  filter(is.na(coef_CA)==F) %>%
  filter(is.na(coef_time)==T)

# Need to filter out the correct cases
diffs_quantile <- diffs_quantile %>%
  mutate(incl= case_when(
    chemical=="ln_MZP" ~ "yes", 
    chemical=="ln_MBP" ~ "yes",  
    chemical=="ln_MHH" ~ "yes",
    chemical=="ln_CNP" ~ "yes",  
    chemical=="ln_COP" ~ "yes",  
    chemical=="ln_BPH" ~ "yes",  
    chemical=="ln_AAM" & cotinine_indic=="yes" ~ "yes",  
    chemical=="ln_ATC" & cotinine_indic=="yes" ~ "yes",  
    chemical=="ln_P02" & cotinine_indic=="yes" ~ "yes",
    TRUE ~ "no")) %>%
  filter(incl=="yes") %>%
  select(-incl, -cotinine_indic)


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
      chemical=="ln_P02" ~ se_P65_URXP02_CA)) %>%
  select(chemical, quant, creatinine_indic,coef_P65, se_P65, coef_P65_CA, se_P65_CA, coef_CA, se_CA, cov_CA_P65)

# make significance indicator:
diffs_quantile <- diffs_quantile %>%
  mutate(CA_sig = case_when(
    (coef_CA-1.96*se_CA)<0 & (coef_CA+1.96*se_CA)<0 ~ "yes",
    (coef_CA-1.96*se_CA)>0 & (coef_CA+1.96*se_CA)>0 ~ "yes",
    TRUE ~ "no"),
    delta_non_sig = case_when(
      (coef_P65-1.96*se_P65)<0 & (coef_P65+1.96*se_P65)<0 ~ "yes",
      (coef_P65-1.96*se_P65)>0 & (coef_P65+1.96*se_P65)>0 ~ "yes",
      TRUE ~ "no"),
    delta_CA=coef_P65+coef_P65_CA,
    delta_CA_se=sqrt((se_P65^2)+(se_P65_CA^2)+2*cov_CA_P65),
    delta_CA_sig = case_when(
      (delta_CA-1.96*delta_CA_se)<0 &(delta_CA+1.96*delta_CA_se)<0 ~ "yes",
      (delta_CA-1.96*delta_CA_se)>0 &(delta_CA+1.96*delta_CA_se)>0 ~ "yes",
      TRUE ~ "no")
    )

table3check <- diffs_quantile %>%
  select(chemical, quant, creatinine_indic, coef_CA, CA_sig,  delta_CA, delta_CA_sig, coef_P65, delta_non_sig) %>%
  pivot_wider(names_from=creatinine_indic, values_from=c(coef_CA, CA_sig, delta_CA, delta_CA_sig, coef_P65, delta_non_sig)) %>%
  mutate(CA_check=case_when(
    sign(coef_CA_no)==sign(coef_CA_yes) & CA_sig_no==CA_sig_yes ~ "yes",
    CA_sig_no=="no" & CA_sig_yes=="no" ~ "yes",
    TRUE ~ "no"),
    CA_delta_check=case_when(
      sign(delta_CA_no)==sign(delta_CA_yes) & delta_CA_sig_no==delta_CA_sig_yes ~ "yes",
      delta_CA_sig_no=="no" & delta_CA_sig_yes=="no" ~ "yes",
      TRUE ~ "no"),
    non_delta_check=case_when(
      sign(coef_P65_no)==sign(coef_P65_yes) & delta_non_sig_no==delta_non_sig_yes ~ "yes",
      delta_non_sig_no=="no" & delta_non_sig_yes=="no" ~ "yes",
      TRUE ~ "no"))

#Only differences
#CA versus rest of U.S. prior to listing
# AAM (acrylamide) 25th: CA + and sig only if control for creatinine
# MHH (di(2-ethylhexyl) phthalate) 25th: CA - for both but only sig if do not control for creatinine
# MHH di(2-ethylhexyl) phthalate 75th: CA - for both but only sig if do not control for creatinine
# P02 (napthalene) 75th: CA - for both but only sig if control for creatinine
# P02 (napthalene) 95th: CA - for both but only sig if control for creatinine
# MBP (di-n-butyl phthalate) 75th: CA - for both but only sig if do not control for creatinine
# MBP (di-n-butyl phthalate) 95th: CA - for both but only sig if do not control for creatinine

# Pre to Post Listing for CA:
# CNP 25th: change - for both but only sig if do not control for creatinine
# ATC (cyanide) 25th: change + for both but only sig if control for creatinine
# MHH 75th: change - for both but only sig if do not control for creatinine
# MHH 95th: change - for both but only sig if do not control for creatinine
# P02 25th: change + for both but only sig if control for creatinine
# P02 75th: change + for both but only sig if control for creatinine
# BPH 75th: change - for both but only sig if control for creatinine
# MZP 95th: change - for both but only sig if do not control for creatinine

# Pre to Post Listing for rest of US:
# AAM 25th: change - for both but only sig if do not control for creatinine
# MHH 95th: change - for both but only sig if do not control for creatinine
# COP 75th: change + for both but only sig if control for creatinine
# COP 95th: change + for both but only sig if do not control for creatinine