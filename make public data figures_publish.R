# Set up the public data we will use to make the figures for the paper

# WRITTEN IN R VERSION: R version 4.0.3 (2020-10-10)

#library(devtools)
#install_github("silentspringinstitute/RNHANES")
#remotes::install_github("silentspringinstitute/RNHANES")

library(tidyverse)
library(survey)
library(RNHANES)

# Set the working directory
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)
setwd('..') 
setwd('..') 

options(stringsAsFactors = FALSE)
options(scipen = 999)


##########################################################################################
##########################################################################################

### Set up the data

##########################################################################################

# Load in the public data file
bd <- read_rds("data/public_dataset.rds")


#Need to bring in the survey design parameters from the public data
dem9900 <- nhanes_load_data("DEMO", "1999-2000") %>%
  mutate(cycle="1999-2000") %>%
  select(SEQN, cycle, SDMVPSU, SDMVSTRA) 

dem0102 <- nhanes_load_data("DEMO_B", "2001-2002") %>%
  mutate(cycle="2001-2002") %>%
  select(SEQN, cycle, SDMVPSU, SDMVSTRA) 

dem0304 <- nhanes_load_data("DEMO_C", "2003-2004") %>%
  mutate(cycle="2003-2004") %>%
  select(SEQN, cycle, SDMVPSU, SDMVSTRA) 

dem0506 <- nhanes_load_data("DEMO_D", "2005-2006") %>%
  mutate(cycle="2005-2006") %>%
  select(SEQN, cycle, SDMVPSU, SDMVSTRA) 

dem0708 <- nhanes_load_data("DEMO_E", "2007-2008") %>%
  mutate(cycle="2007-2008") %>%
  select(SEQN, cycle, SDMVPSU, SDMVSTRA) 

dem0910 <- nhanes_load_data("DEMO_F", "2009-2010") %>%
  mutate(cycle="2009-2010") %>%
  select(SEQN, cycle, SDMVPSU, SDMVSTRA) 

dem1112 <- nhanes_load_data("DEMO_G", "2011-2012") %>%
  mutate(cycle="2011-2012") %>%
  select(SEQN, cycle, SDMVPSU, SDMVSTRA) 

dem1314 <- nhanes_load_data("DEMO_H", "2013-2014") %>%
  select(SEQN, cycle, SDMVPSU, SDMVSTRA) 

dem1516 <- nhanes_load_data("DEMO_I", "2015-2016") %>%
  select(SEQN, cycle, SDMVPSU, SDMVSTRA) 

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
  rename(T_VPSU=SDMVPSU,
         T_VSTRAT=SDMVSTRA)


bd<- bd %>%
  left_join(dems, by=c("cycle", "SEQN")) %>%
  filter(is.na(T_VPSU)==0)


##########################################################################################
##########################################################################################

### Create survey designs

##########################################################################################

# 18-year phthalates
phthalates18 <- bd %>%
  filter(is.na(weight18_phthalates)==0) 

phthalates18_design <-svydesign(id = ~T_VPSU,
                                strata = ~T_VSTRAT,
                                weights = ~weight18_phthalates,
                                nest = TRUE,
                                data = phthalates18)


# 16-year phthalates
phthalates16 <- bd %>%
  filter(is.na(weight16_phthalates)==0) 

phthalates16_design <-svydesign(id = ~T_VPSU,
                                strata = ~T_VSTRAT,
                                weights = ~weight16_phthalates,
                                nest = TRUE,
                                data = phthalates16)

# 12-year phthalates
phthalates12 <- bd %>%
  filter(is.na(weight12_phthalates)==0) 

phthalates12_design <-svydesign(id = ~T_VPSU,
                                strata = ~T_VSTRAT,
                                weights = ~weight12_phthalates,
                                nest = TRUE,
                                data = phthalates12)

# 14 year phenols
phenols14 <- bd %>%
  filter(is.na(weight14_phenols)==0)

phenols14_design <-svydesign(id = ~T_VPSU,
                             strata = ~T_VSTRAT,
                             weights = ~weight14_phenols,
                             nest = TRUE,
                             data = phenols14)

# 12 year phenols
phenols12 <- bd %>%
  filter(is.na(weight12_phenols)==0)

phenols12_design <-svydesign(id = ~T_VPSU,
                             strata = ~T_VSTRAT,
                             weights = ~weight12_phenols,
                             nest = TRUE,
                             data = phenols12)

# 4 year phenols
phenols4 <- bd %>%
  filter(is.na(weight4_phenols)==0)

phenols4_design <-svydesign(id = ~T_VPSU,
                            strata = ~T_VSTRAT,
                            weights = ~weight4_phenols,
                            nest = TRUE,
                            data = phenols4)

# VOCs in urine
urineVOC8 <- bd %>%
  filter(is.na(weight8_urineVOC)==0)

urineVOC8_design <-svydesign(id = ~T_VPSU,
                             strata = ~T_VSTRAT,
                             weights = ~weight8_urineVOC,
                             nest = TRUE,
                             data = urineVOC8)

# 18 year blood VOCs
bloodVOC18 <- bd %>%
  filter(is.na(weight18_bloodVOC)==0) %>%
  filter(is.na(T_VPSU)==0)

bloodVOC18_design <-svydesign(id = ~T_VPSU,
                              strata = ~T_VSTRAT,
                              weights = ~weight18_bloodVOC,
                              nest = TRUE,
                              data = bloodVOC18)

# 16 year blood VOCs
bloodVOC16 <- bd %>%
  filter(is.na(weight16_bloodVOC)==0) 

bloodVOC16_design <-svydesign(id = ~T_VPSU,
                              strata = ~T_VSTRAT,
                              weights = ~weight16_bloodVOC,
                              nest = TRUE,
                              data = bloodVOC16)


# 14 year PAHs
PAH14 <- bd %>%
  filter(is.na(weight14_PAH)==0)

PAH14_design <-svydesign(id = ~T_VPSU,
                         strata = ~T_VSTRAT,
                         weights = ~weight14_PAH,
                         nest = TRUE,
                         data = PAH14)
# 12 year PAHs
PAH12 <- bd %>%
  filter(is.na(weight12_PAH)==0)

PAH12_design <-svydesign(id = ~T_VPSU,
                         strata = ~T_VSTRAT,
                         weights = ~weight12_PAH,
                         nest = TRUE,
                         data = PAH12)
# 10 year PAHs
PAH10 <- bd %>%
  filter(is.na(weight10_PAH)==0)

PAH10_design <-svydesign(id = ~T_VPSU,
                         strata = ~T_VSTRAT,
                         weights = ~weight10_PAH,
                         nest = TRUE,
                         data = PAH10)
# 6 year PAHs
PAH6 <- bd %>%
  filter(is.na(weight6_PAH)==0)

PAH6_design <-svydesign(id = ~T_VPSU,
                        strata = ~T_VSTRAT,
                        weights = ~weight6_PAH,
                        nest = TRUE,
                        data = PAH6)


#PFAS - only 14-year weights
pfas14 <- bd %>%
  filter(is.na(weight14_pfas)==0)

pfas14_design <-svydesign(id = ~T_VPSU,
                          strata = ~T_VSTRAT,
                          weights = ~weight14_pfas,
                          nest = TRUE,
                          data = pfas14)


#Lead/Mercury
lead_merc18 <- bd %>%
  filter(is.na(weight18_lead_mercury)==0)

lead_merc18_design <-svydesign(id = ~T_VPSU,
                               strata = ~T_VSTRAT,
                               weights = ~weight18_lead_mercury,
                               nest = TRUE,
                               data = lead_merc18)


#Cadmium
cadmium18 <- bd %>%
  filter(is.na(weight18_cadmium)==0)

cadmium18_design <-svydesign(id = ~T_VPSU,
                             strata = ~T_VSTRAT,
                             weights = ~weight18_cadmium,
                             nest = TRUE,
                             data = cadmium18)


#Overall
bd_design <-svydesign(id = ~T_VPSU,
                      strata = ~T_VSTRAT,
                      weights = ~weight18_MEC,
                      nest = TRUE,
                      data = bd)


##########################################################################################
##########################################################################################

### Summary Statistics for Chemical Concentrations in NHANES Participants by Cycle

##########################################################################################

# Function to compute GMs by cycle
sumstat_chem_GM <- function(chem, des) {
  # chem = ln(chemical concentration)
  # des = non-subsetted survey design object
  mynhanesdesign<-eval(parse(text=des))
  a <-svyby(chem, ~cycle, design=mynhanesdesign, FUN=svymean, na.rm=TRUE)
  b <- as.data.frame(a)
  colnames(b) <- c("cycle","coef","se")
  b$GM<-exp(b$coef)
  c<-as.data.frame(confint(a, level=0.95, df=degf(mynhanesdesign)))
  colnames(c)<- c("CI_low_raw", "CI_high_raw")
  c$CI_low<-exp(c$CI_low_raw)
  c$CI_high<-exp(c$CI_high_raw)
  out<-cbind(b,c) %>% select(cycle, GM, CI_low, CI_high)
  out2<-remove_rownames(out)
  return(out2)
}


# Function to compute quantiles by cycle
sumstat_chem_quantile <- function(chem, des) {
  # chem = ln(chemical concentration)
  # des = non-subsetted survey design object
  mynhanesdesign<-eval(parse(text=des))
  a <-svyby(chem, ~cycle, design=mynhanesdesign, FUN=svyquantile, quantiles=c(0.05, 0.25, 0.5, 0.75, 0.95), keep.var = FALSE, na.rm=TRUE)
  b <- as.data.frame(a)
  colnames(b) <- c("cycle","p5","p25","p50","p75","p95")
  out<-remove_rownames(b)
  return(out)
}


#make list of ln chems
chem_list <- c(~ln_MZP, ~ln_MZP_cr, ~ln_MBP, ~ln_MBP_cr, ~ln_MEP, ~ln_MEP_cr, ~ln_MIB, ~ln_MIB_cr, ~ln_MHH, ~ln_MHH_cr, ~ln_CNP, ~ln_CNP_cr, ~ln_COP, ~ln_COP_cr,
               ~ln_BPH, ~ln_BPH_cr, ~ln_BP3, ~ln_BP3_cr, ~ln_TRS, ~ln_TRS_cr, ~ln_14D, ~ln_14D_cr, ~ln_DCB, ~ln_DCB_cr, ~ln_MPB, ~ln_MPB_cr, ~ln_PPB, ~ln_PPB_cr, ~ln_BPS, ~ln_BPS_cr,
               ~ln_2MH, ~ln_2MH_cr, ~ln_34M, ~ln_34M_cr, ~ln_AAM, ~ln_AAM_cr, ~ln_ATC, ~ln_ATC_cr, ~ln_BMA, ~ln_BMA_cr, ~ln_CYM, ~ln_CYM_cr, ~ln_DHB, ~ln_DHB_cr, ~ln_HP2, ~ln_HP2_cr, ~ln_MAD, ~ln_MAD_cr, ~ln_MB3, ~ln_MB3_cr,
               ~ln_VCF, ~ln_VTO,
               ~ln_P01, ~ln_P01_cr, ~ln_P02, ~ln_P02_cr, ~ln_P03, ~ln_P03_cr, ~ln_P04, ~ln_P04_cr, ~ln_P06, ~ln_P06_cr, ~ln_P10, ~ln_P10_cr, ~ln_P05, ~ln_P05_cr, ~ln_P07, ~ln_P07_cr, ~ln_P17, ~ln_P17_cr, ~ln_P19, ~ln_P19_cr,
               ~ln_MPAH, ~ln_PFDE, ~ln_PFHS, ~ln_PFNA, ~ln_PFOA, ~ln_PFOS,
               ~ln_BPB, ~ln_THG,
               ~ln_UCD, ~ln_UCD_cr)


#make list of un-logged chems that we will use for the quantiles
chem_list_2 <- c(~URXMZP, ~MZP_cr, ~URXMBP, ~MBP_cr, ~URXMEP, ~MEP_cr, ~URXMIB, ~MIB_cr, ~URXMHH, ~MHH_cr, ~URXCNP, ~CNP_cr, ~URXCOP, ~COP_cr,
               ~URXBPH, ~BPH_cr, ~URXBP3, ~BP3_cr, ~URXTRS, ~TRS_cr, ~URX14D, ~X14Dcr, ~URXDCB, ~DCB_cr, ~URXMPB, ~MPB_cr, ~URXPPB, ~PPB_cr, ~URXBPS, ~BPS_cr,
               ~URX2MH, ~X2MHcr, ~URX34M, ~X34Mcr, ~URXAAM, ~AAM_cr, ~URXATC, ~ATC_cr, ~URXBMA, ~BMA_cr, ~URXCYM, ~CYM_cr, ~URXDHB, ~DHB_cr, ~URXHP2, ~HP2_cr, ~URXMAD, ~MAD_cr, ~URXMB3, ~MB3_cr,
               ~LBXVCF, ~LBXVTO,
               ~URXP01, ~P01_cr, ~URXP02, ~P02_cr, ~URXP03, ~P03_cr, ~URXP04, ~P04_cr, ~URXP06, ~P06_cr, ~URXP10, ~P10_cr, ~URXP05, ~P05_cr, ~URXP07, ~P07_cr, ~URXP17, ~P17_cr, ~URXP19, ~P19_cr,
               ~LBXMPAH, ~LBXPFDE, ~LBXPFHS, ~LBXPFNA, ~LBXPFOA, ~LBXPFOS,
               ~LBXBPB, ~LBXTHG,
               ~URXUCD, ~UCD_cr)


#make list of survey design objects
des_sum <- as.list(c(rep("phthalates18_design", 6), rep("phthalates16_design", 4), rep("phthalates12_design", 4), rep("phenols14_design", 10), rep("phenols12_design", 4), rep("phenols4_design", 2), rep("urineVOC8_design", 20), rep("bloodVOC18_design", 1), rep("bloodVOC16_design", 1), rep("PAH14_design", 12), rep("PAH12_design", 4), rep("PAH10_design", 2), rep("PAH6_design", 2), rep("pfas14_design", 6), rep("lead_merc18_design", 2), rep("cadmium18_design", 2)))


# Compute the geometric means
chem_stats_GM <- pmap(list(chem_list, des_sum), sumstat_chem_GM)
names(chem_stats_GM) <- str_sub(as.character(chem_list), 5, 10)
GM_out<-bind_rows(chem_stats_GM, .id="chemical")


# Compute the quantiles
chem_stats_quantile <- pmap(list(chem_list_2, des_sum), sumstat_chem_quantile)
names(chem_stats_quantile) <- str_sub(as.character(chem_list_2), 2, 10)
quantile_out<-bind_rows(chem_stats_quantile, .id="chemical")


# save the dataset
#write_rds(quantile_out, "data/public_data_for_figures.rds")
