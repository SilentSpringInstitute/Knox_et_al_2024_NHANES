# Analysis Script that was run at the National Center for Health Statistics
# Restricted Data Center

# WRITTEN IN R VERSION: R version 4.0.3 (2020-10-10)


library(tidyverse)
library(stringr)
library(survey)
library(broom)
library(quantreg)
library(haven)


options(stringsAsFactors = FALSE)
options(scipen = 999)

# Set the working directory
setwd('C:/Researcher Proposal/Robinson_Dodson_1886/Dodson_update_file/Output') 


##########################################################################################
##########################################################################################

### General Functions

##########################################################################################

# Function to make a regression formula 
make_formula <- function(front, back) {
  return(str_c(front, back)) 
}


##########################################################################################
##########################################################################################

### Load the data and create additional variables needed for analysis


##########################################################################################

# Number of variables should be 293 = 274 (original public dataset) + 2 (CA, chem_law) + 13 (P65 indicators) + 1 (ExamYEAR)  + survey design parameters (2) + SDSSRVYR
# Number of observations should be 92,062

# We can read in everything as numeric except cycle, race_cat, PIR_cat

#Read in analytic dataset from the RDC as a .csv
bd <- read_csv("nhanes_final_file.csv", col_types = cols(
  cycle=col_character(),
  race_cat=col_character(),
  PIR_cat=col_character(),
  .default=col_double())
)

stopifnot(nrow(bd)==92062)
stopifnot(ncol(bd)==293)

str(bd)

bd <- bd %>%
  rename(LBXPFNA=LBXPF,
         PFNA_high=PF_high,
         ln_PFNA=ln_PF)


# Data Checks 
summary(bd$URXMHH)
# results should be 0.14, 5.96, 13.70, 41.54, 31.80, 9326.10, NAs=70625

summary(bd$weight14_phenols)
# results should be 654.1, 5569.3, 9951.6, 14911.5, 18881.3, 124948.3, NAs=73426

summary(bd$P02_cr)
#results should be 0.01, 1.78, 3.38, 6.20, 7.21, 404.57, NAs=73885

summary(bd$ln_VTO)
#results should be -4.03, -3.02, -2.50, -2.30, -1.71, 4.03, NAs=75328

summary(bd$ln_MAD)
# results should be 2.14, 4.28, 4.89, 4.83, 5.41, 11.24, NAs=80717


# need to check if ExamYEAR has been adjusted to account for people whose exam date falls outside of their survey cycle
# Year of examination values need to be recoded such that all participants belonging to the same PSU have the same year 
# of examination value. "The year of examination values different from the survey year should be recoded to the survey 
# year. "Survey year" herein means: the first year of a given two-year survey cycle for the first 15 PSUs; and the 
# second year of a given two-year survey cycle for the last 15 PSUs.
table(bd$cycle, bd$ExamYEAR)
table(bd$cycle, bd$SDSSRVYR) 

# Yes, we need to adjust ExamYEAR
bd <- bd %>%
  select(-ExamYEAR) %>%
  rename(ExamYEAR=SDSSRVYR)
table(bd$cycle, bd$ExamYEAR)


# Set up factor variables
bd <- bd %>%
  mutate(race_cat=factor(race_cat, levels=c("Non-Hispanic White", "Hispanic", "Non-Hispanic Black", "Other")),
         PIR_cat=factor(PIR_cat, levels=c("middle","low","high")))

#checks on the factor variables:
table(bd$race_cat)
# race_cat should be 34282 Non-Hispanic White, 28556 Hispanic, 21529 Non-Hispanic Black, 7695 Other

table(bd$PIR_cat)
#PIR_cat should be 35005 middle, 23001 low, 26091 high


bd <- bd %>%
  mutate(Y2000=ifelse(ExamYEAR==2000, 1, 0),
         Y2001=ifelse(ExamYEAR==2001, 1, 0),
         Y2002=ifelse(ExamYEAR==2002, 1, 0),
         Y2003=ifelse(ExamYEAR==2003, 1, 0),
         Y2004=ifelse(ExamYEAR==2004, 1, 0),
         Y2005=ifelse(ExamYEAR==2005, 1, 0),
         Y2006=ifelse(ExamYEAR==2006, 1, 0),
         Y2007=ifelse(ExamYEAR==2007, 1, 0),
         Y2008=ifelse(ExamYEAR==2008, 1, 0),
         Y2009=ifelse(ExamYEAR==2009, 1, 0),
         Y2010=ifelse(ExamYEAR==2010, 1, 0),
         Y2011=ifelse(ExamYEAR==2011, 1, 0),
         Y2012=ifelse(ExamYEAR==2012, 1, 0),
         Y2013=ifelse(ExamYEAR==2013, 1, 0),
         Y2014=ifelse(ExamYEAR==2014, 1, 0),
         Y2015=ifelse(ExamYEAR==2015, 1, 0),
         Y2016=ifelse(ExamYEAR==2016, 1, 0))


bd <- bd %>%
  mutate(time=ExamYEAR-1998)
stopifnot(max(bd$time)==18)

bd <- bd %>%
  mutate(P65_URXMZP_CA=P65_URXMZP*CA,
         P65_URXMBP_CA=P65_URXMBP*CA,
         P65_URXCNP_CA=P65_URXCNP*CA,
         P65_URXMHH_CA=P65_URXMHH*CA,
         P65_URXCOP_CA=P65_URXCOP*CA,
         P65_URXBPH_CA=P65_URXBPH*CA,
         P65_URXAAM_CA=P65_URXAAM*CA,
         P65_URXATC_CA=P65_URXATC*CA,
         P65_URXBMA_CA=P65_URXBMA*CA,
         P65_LBXVCF_CA=P65_LBXVCF*CA,
         P65_LBXVTO_CA=P65_LBXVTO*CA,
         P65_URXP01_CA=P65_URXP01*CA,
         P65_URXP02_CA=P65_URXP02*CA,
         P65_URXMZP_time=P65_URXMZP*time,
         P65_URXMBP_time=P65_URXMBP*time,
         P65_URXCNP_time=P65_URXCNP*time,
         P65_URXMHH_time=P65_URXMHH*time,
         P65_URXCOP_time=P65_URXCOP*time,
         P65_URXBPH_time=P65_URXBPH*time,
         P65_URXAAM_time=P65_URXAAM*time,
         P65_URXATC_time=P65_URXATC*time,
         P65_URXBMA_time=P65_URXBMA*time,
         P65_LBXVCF_time=P65_LBXVCF*time,
         P65_LBXVTO_time=P65_LBXVTO*time,
         P65_URXP01_time=P65_URXP01*time,
         P65_URXP02_time=P65_URXP02*time,
         CA_time=CA*time)


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
#check
stopifnot(nrow(phthalates18)>nrow(phthalates16)) 
stopifnot(nrow(phthalates16)>nrow(phthalates12)) 

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

#check
stopifnot(nrow(phenols14)>nrow(phenols12))
stopifnot(nrow(phenols12)>nrow(phenols4))

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

#check
stopifnot(nrow(bloodVOC18)>nrow(bloodVOC16))

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

#check
stopifnot(nrow(PAH14)>nrow(PAH12))
stopifnot(nrow(PAH12)>nrow(PAH10))
stopifnot(nrow(PAH10)>nrow(PAH6))


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

### Output Table 8 -  Regression Output from OLS Models II.a-c

##########################################################################################

# Use a loop to run the svyglm continuous regressions for the Prop 65 regressions and then do the joint tests on the coefficients
# Note on the test output - to get the Working 2logLR statistic that is reported if you run regTermTest in the console,
# take the chisq statistic reported in the test output, and divide by the average of the lambdas (eigenvalues)
# reported in the test output.


OLS_II_formulas <- c(
  "ln_MZP ~ P65_URXMZP + female + child + wcba + race_cat + PIR_cat",
  "ln_MZP ~ P65_URXMZP + CA + P65_URXMZP_CA + female + child + wcba + race_cat + PIR_cat",
  "ln_MZP ~ P65_URXMZP + CA + P65_URXMZP_CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_MZP ~ P65_URXMZP + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MZP ~ P65_URXMZP + CA + P65_URXMZP_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MZP ~ P65_URXMZP + CA + P65_URXMZP_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MBP ~ P65_URXMBP + female + child + wcba + race_cat + PIR_cat",
  "ln_MBP ~ P65_URXMBP + CA + P65_URXMBP_CA + female + child + wcba + race_cat + PIR_cat",
  "ln_MBP ~ P65_URXMBP + CA + P65_URXMBP_CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_MBP ~ P65_URXMBP + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MBP ~ P65_URXMBP + CA + P65_URXMBP_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MBP ~ P65_URXMBP + CA + P65_URXMBP_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MEP ~ female + child + wcba + race_cat + PIR_cat",
  "ln_MEP ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_MEP ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_MEP ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MEP ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MEP ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MIB ~ female + child + wcba + race_cat + PIR_cat",
  "ln_MIB ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_MIB ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_MIB ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MIB ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MIB ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MHH ~ P65_URXMHH + female + child + wcba + race_cat + PIR_cat",
  "ln_MHH ~ P65_URXMHH + CA + P65_URXMHH_CA + female + child + wcba + race_cat + PIR_cat",
  "ln_MHH ~ P65_URXMHH + CA + P65_URXMHH_CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_MHH ~ P65_URXMHH + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MHH ~ P65_URXMHH + CA + P65_URXMHH_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MHH ~ P65_URXMHH + CA + P65_URXMHH_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_CNP ~ P65_URXCNP + female + child + wcba + race_cat + PIR_cat",
  "ln_CNP ~ P65_URXCNP + CA + P65_URXCNP_CA + female + child + wcba + race_cat + PIR_cat",
  "ln_CNP ~ P65_URXCNP + CA + P65_URXCNP_CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_CNP ~ P65_URXCNP + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_CNP ~ P65_URXCNP + CA + P65_URXCNP_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_CNP ~ P65_URXCNP + CA + P65_URXCNP_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_COP ~ P65_URXCOP + female + child + wcba + race_cat + PIR_cat",
  "ln_COP ~ P65_URXCOP + CA + P65_URXCOP_CA + female + child + wcba + race_cat + PIR_cat",
  "ln_COP ~ P65_URXCOP + CA + P65_URXCOP_CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_COP ~ P65_URXCOP + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_COP ~ P65_URXCOP + CA + P65_URXCOP_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_COP ~ P65_URXCOP + CA + P65_URXCOP_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_BPH ~ P65_URXBPH + female + child + wcba + race_cat + PIR_cat",
  "ln_BPH ~ P65_URXBPH + CA + P65_URXBPH_CA + female + child + wcba + race_cat + PIR_cat",
  "ln_BPH ~ P65_URXBPH + CA + P65_URXBPH_CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_BPH ~ P65_URXBPH + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_BPH ~ P65_URXBPH + CA + P65_URXBPH_CA + female + child + wcba + race_cat + PIR_cat+ln_UCR",
  "ln_BPH ~ P65_URXBPH + CA + P65_URXBPH_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_BP3 ~ female + child + wcba + race_cat + PIR_cat",
  "ln_BP3 ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_BP3 ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_BP3 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_BP3 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_BP3 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_TRS ~ female + child + wcba + race_cat + PIR_cat",
  "ln_TRS ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_TRS ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_TRS ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_TRS ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_TRS ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_14D ~ female + child + wcba + race_cat + PIR_cat",
  "ln_14D ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_14D ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_14D ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_14D ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_14D ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_DCB ~ female + child + wcba + race_cat + PIR_cat",
  "ln_DCB ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_DCB ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_DCB ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_DCB ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_DCB ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MPB ~ female + child + wcba + race_cat + PIR_cat",
  "ln_MPB ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_MPB ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_MPB ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MPB ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MPB ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_PPB ~ female + child + wcba + race_cat + PIR_cat",
  "ln_PPB ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_PPB ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_PPB ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_PPB ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_PPB ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_BPS ~ female + child + wcba + race_cat + PIR_cat",
  "ln_BPS ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_BPS ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_BPS ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_BPS ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_BPS ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_2MH ~ female + child + wcba + race_cat + PIR_cat",
  "ln_2MH ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_2MH ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_2MH ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_2MH ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_2MH ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_2MH ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_2MH ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_2MH ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_2MH ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_2MH ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_2MH ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_34M ~ female + child + wcba + race_cat + PIR_cat",
  "ln_34M ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_34M ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_34M ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_34M ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_34M ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_34M ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_34M ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_34M ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_34M ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_34M ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_34M ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_AAM ~ P65_URXAAM + female + child + wcba + race_cat + PIR_cat",
  "ln_AAM ~ P65_URXAAM + CA + P65_URXAAM_CA + female + child + wcba + race_cat + PIR_cat",
  "ln_AAM ~ P65_URXAAM + CA + P65_URXAAM_CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_AAM ~ P65_URXAAM + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_AAM ~ P65_URXAAM + CA + P65_URXAAM_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_AAM ~ P65_URXAAM + CA + P65_URXAAM_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_AAM ~ P65_URXAAM + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_AAM ~ P65_URXAAM + CA + P65_URXAAM_CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_AAM ~ P65_URXAAM + CA + P65_URXAAM_CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_AAM ~ P65_URXAAM + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_AAM ~ P65_URXAAM + CA + P65_URXAAM_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_AAM ~ P65_URXAAM + CA + P65_URXAAM_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_ATC ~ P65_URXATC + female + child + wcba + race_cat + PIR_cat",
  "ln_ATC ~ P65_URXATC + CA + P65_URXATC_CA + female + child + wcba + race_cat + PIR_cat",
  "ln_ATC ~ P65_URXATC + CA + P65_URXATC_CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_ATC ~ P65_URXATC + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_ATC ~ P65_URXATC + CA + P65_URXATC_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_ATC ~ P65_URXATC + CA + P65_URXATC_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_ATC ~ P65_URXATC + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_ATC ~ P65_URXATC + CA + P65_URXATC_CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_ATC ~ P65_URXATC + CA + P65_URXATC_CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_ATC ~ P65_URXATC + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_ATC ~ P65_URXATC + CA + P65_URXATC_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_ATC ~ P65_URXATC + CA + P65_URXATC_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_BMA ~ P65_URXBMA + female + child + wcba + race_cat + PIR_cat",
  "ln_BMA ~ P65_URXBMA + CA + P65_URXBMA_CA + female + child + wcba + race_cat + PIR_cat",
  "ln_BMA ~ P65_URXBMA + CA + P65_URXBMA_CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_BMA ~ P65_URXBMA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_BMA ~ P65_URXBMA + CA + P65_URXBMA_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_BMA ~ P65_URXBMA + CA + P65_URXBMA_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_BMA ~ P65_URXBMA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_BMA ~ P65_URXBMA + CA + P65_URXBMA_CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_BMA ~ P65_URXBMA + CA + P65_URXBMA_CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_BMA ~ P65_URXBMA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_BMA ~ P65_URXBMA + CA + P65_URXBMA_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_BMA ~ P65_URXBMA + CA + P65_URXBMA_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_CYM ~ female + child + wcba + race_cat + PIR_cat",
  "ln_CYM ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_CYM ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_CYM ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_CYM ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_CYM ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_CYM ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_CYM ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_CYM ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_CYM ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_CYM ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_CYM ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_DHB ~ female + child + wcba + race_cat + PIR_cat",
  "ln_DHB ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_DHB ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_DHB ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_DHB ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_DHB ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_DHB ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_DHB ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_DHB ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_DHB ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_DHB ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_DHB ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_HP2 ~ female + child + wcba + race_cat + PIR_cat",
  "ln_HP2 ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_HP2 ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_HP2 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_HP2 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_HP2 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_HP2 ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_HP2 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_HP2 ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_HP2 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_HP2 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_HP2 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_MAD ~ female + child + wcba + race_cat + PIR_cat",
  "ln_MAD ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_MAD ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_MAD ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MAD ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MAD ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MAD ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_MAD ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_MAD ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_MAD ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_MAD ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_MAD ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_MB3 ~ female + child + wcba + race_cat + PIR_cat",
  "ln_MB3 ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_MB3 ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_MB3 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MB3 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MB3 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MB3 ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_MB3 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_MB3 ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_MB3 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_MB3 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_MB3 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_VCF ~ P65_LBXVCF + female + wcba + race_cat + PIR_cat",
  "ln_VCF ~ P65_LBXVCF + CA + P65_LBXVCF_CA + female + wcba + race_cat + PIR_cat",  
  "ln_VCF ~ P65_LBXVCF + CA + P65_LBXVCF_CA + time + female + wcba + race_cat + PIR_cat", 
  "ln_VTO ~ P65_LBXVTO + female + wcba + race_cat + PIR_cat",
  "ln_VTO ~ P65_LBXVTO + CA + P65_LBXVTO_CA + female + wcba + race_cat + PIR_cat",
  "ln_VTO ~ P65_LBXVTO + CA + P65_LBXVTO_CA +time + female + wcba + race_cat + PIR_cat",
  "ln_VTO ~ P65_LBXVTO + female + wcba + race_cat + PIR_cat + ln_COT",
  "ln_VTO ~ P65_LBXVTO + CA + P65_LBXVTO_CA + female + wcba + race_cat + PIR_cat + ln_COT",
  "ln_VTO ~ P65_LBXVTO + CA + P65_LBXVTO_CA +time + female + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P01 ~ P65_URXP01 + female + child + wcba + race_cat + PIR_cat",
  "ln_P01 ~ P65_URXP01 + CA + P65_URXP01_CA + female + child + wcba + race_cat + PIR_cat",
  "ln_P01 ~ P65_URXP01 + CA + P65_URXP01_CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_P01 ~ P65_URXP01 + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P01 ~ P65_URXP01 + CA + P65_URXP01_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P01 ~ P65_URXP01 + CA + P65_URXP01_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P01 ~ P65_URXP01 + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P01 ~ P65_URXP01 + CA + P65_URXP01_CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P01 ~ P65_URXP01 + CA + P65_URXP01_CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P01 ~ P65_URXP01 + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P01 ~ P65_URXP01 + CA + P65_URXP01_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P01 ~ P65_URXP01 + CA + P65_URXP01_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P02 ~ P65_URXP02 + female + child + wcba + race_cat + PIR_cat",
  "ln_P02 ~ P65_URXP02 + CA + P65_URXP02_CA + female + child + wcba + race_cat + PIR_cat",
  "ln_P02 ~ P65_URXP02 + CA + P65_URXP02_CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_P02 ~ P65_URXP02 + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P02 ~ P65_URXP02 + CA + P65_URXP02_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P02 ~ P65_URXP02 + CA + P65_URXP02_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P02 ~ P65_URXP02 + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P02 ~ P65_URXP02 + CA + P65_URXP02_CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P02 ~ P65_URXP02 + CA + P65_URXP02_CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P02 ~ P65_URXP02 + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P02 ~ P65_URXP02 + CA + P65_URXP02_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P02 ~ P65_URXP02 + CA + P65_URXP02_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P03 ~ female + child + wcba + race_cat + PIR_cat",
  "ln_P03 ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_P03 ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_P03 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P03 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P03 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P03 ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P03 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P03 ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P03 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P03 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P03 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P04 ~ female + child + wcba + race_cat + PIR_cat",
  "ln_P04 ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_P04 ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_P04 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P04 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P04 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P04 ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P04 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P04 ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P04 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P04 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P04 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P06 ~ female + child + wcba + race_cat + PIR_cat",
  "ln_P06 ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_P06 ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_P06 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P06 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P06 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P06 ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P06 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P06 ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P06 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P06 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P06 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P10 ~ female + child + wcba + race_cat + PIR_cat",
  "ln_P10 ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_P10 ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_P10 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P10 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P10 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P10 ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P10 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P10 ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P10 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P10 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P10 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P05 ~ female + child + wcba + race_cat + PIR_cat",
  "ln_P05 ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_P05 ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_P05 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P05 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P05 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P05 ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P05 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P05 ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P05 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P05 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P05 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P07 ~ female + child + wcba + race_cat + PIR_cat",
  "ln_P07 ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_P07 ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_P07 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P07 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P07 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P07 ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P07 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P07 ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P07 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P07 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P07 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P17 ~ female + child + wcba + race_cat + PIR_cat",
  "ln_P17 ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_P17 ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_P17 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P17 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P17 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P17 ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P17 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P17 ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P17 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P17 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P17 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P19 ~ female + child + wcba + race_cat + PIR_cat",
  "ln_P19 ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_P19 ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_P19 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P19 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P19 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P19 ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P19 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P19 ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P19 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P19 ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P19 ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_MPAH ~ female + wcba + race_cat + PIR_cat",
  "ln_MPAH ~ CA + female + wcba + race_cat + PIR_cat",
  "ln_MPAH ~ CA + time + female + wcba + race_cat + PIR_cat",
  "ln_PFDE ~ female + wcba + race_cat + PIR_cat",
  "ln_PFDE ~ CA + female + wcba + race_cat + PIR_cat",
  "ln_PFDE ~ CA + time + female + wcba + race_cat + PIR_cat",
  "ln_PFHS ~ female + wcba + race_cat + PIR_cat",
  "ln_PFHS ~ CA + female + wcba + race_cat + PIR_cat",
  "ln_PFHS ~ CA + time + female + wcba + race_cat + PIR_cat",
  "ln_PFNA ~ female + wcba + race_cat + PIR_cat",
  "ln_PFNA ~ CA + female + wcba + race_cat + PIR_cat",
  "ln_PFNA ~ CA + time + female + wcba + race_cat + PIR_cat",
  "ln_PFOA ~ female + wcba + race_cat + PIR_cat",
  "ln_PFOA ~ CA + female + wcba + race_cat + PIR_cat",
  "ln_PFOA ~ CA + time + female + wcba + race_cat + PIR_cat",
  "ln_PFOS ~ female + wcba + race_cat + PIR_cat",
  "ln_PFOS ~ CA + female + wcba + race_cat + PIR_cat",
  "ln_PFOS ~ CA + time + female + wcba + race_cat + PIR_cat",
  "ln_BPB ~ female + child + wcba + race_cat + PIR_cat",
  "ln_BPB ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_BPB ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_THG ~ female + child + wcba + race_cat + PIR_cat",
  "ln_THG ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_THG ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_UCD ~ female + child + wcba + race_cat + PIR_cat",
  "ln_UCD ~ CA + female + child + wcba + race_cat + PIR_cat",
  "ln_UCD ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "ln_UCD ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_UCD ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_UCD ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_UCD ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_UCD ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_UCD ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_UCD ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_UCD ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_UCD ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT"
)
length(OLS_II_formulas)

des_P65 <- as.list(c(rep("phthalates18_design", 18), 
                     rep("phthalates16_design", 12), 
                     rep("phthalates12_design", 12), 
                     rep("phenols14_design", 30), 
                     rep("phenols12_design", 12), 
                     rep("phenols4_design", 6), 
                     rep("urineVOC8_design", 120), 
                     rep("bloodVOC18_design", 3), 
                     rep("bloodVOC16_design", 6), 
                     rep("PAH14_design", 72), 
                     rep("PAH12_design", 24), 
                     rep("PAH10_design", 12), 
                     rep("PAH6_design", 12), 
                     rep("pfas14_design", 18), 
                     rep("lead_merc18_design", 6), 
                     rep("cadmium18_design", 12)
                     ))

tv <- as.list(c(rep("NOTEST", 1), 
                rep("~P65_URXMZP+P65_URXMZP_CA", 2), 
                rep("NOTEST", 1), 
                rep("~P65_URXMZP+P65_URXMZP_CA", 2), 
                rep("NOTEST", 1), 
                rep("~P65_URXMBP+P65_URXMBP_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXMBP+P65_URXMBP_CA", 2),
                rep("NOTEST", 12), 
                rep("NOTEST", 1), 
                rep("~P65_URXMHH+P65_URXMHH_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXMHH+P65_URXMHH_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXCNP+P65_URXCNP_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXCNP+P65_URXCNP_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXCOP+P65_URXCOP_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXCOP+P65_URXCOP_CA", 2), 
                rep("NOTEST", 1), 
                rep("~P65_URXBPH+P65_URXBPH_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXBPH+P65_URXBPH_CA", 2),
                rep("NOTEST", 42), 
                rep("NOTEST", 24), 
                rep("NOTEST", 1), 
                rep("~P65_URXAAM+P65_URXAAM_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXAAM+P65_URXAAM_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXAAM+P65_URXAAM_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXAAM+P65_URXAAM_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXATC+P65_URXATC_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXATC+P65_URXATC_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXATC+P65_URXATC_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXATC+P65_URXATC_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXBMA+P65_URXBMA_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXBMA+P65_URXBMA_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXBMA+P65_URXBMA_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXBMA+P65_URXBMA_CA", 2),
                rep("NOTEST", 60), 
                rep("NOTEST", 1), 
                rep("~P65_LBXVCF+P65_LBXVCF_CA", 2), 
                rep("NOTEST", 1), 
                rep("~P65_LBXVTO+P65_LBXVTO_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_LBXVTO+P65_LBXVTO_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXP01+P65_URXP01_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXP01+P65_URXP01_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXP01+P65_URXP01_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXP01+P65_URXP01_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXP02+P65_URXP02_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXP02+P65_URXP02_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXP02+P65_URXP02_CA", 2),
                rep("NOTEST", 1), 
                rep("~P65_URXP02+P65_URXP02_CA", 2),
                rep("NOTEST",96),
                rep("NOTEST", 18),
                rep("NOTEST", 6),
                rep("NOTEST", 12)
                ))

stopifnot(length(OLS_II_formulas)==length(des_P65))
stopifnot(length(OLS_II_formulas)==length(tv))

rough_OLS_II<-NULL
for(i in 1:length(OLS_II_formulas)) {
  #for(i in 1:3) {    #use this code if something breaks at RDC and we need to run specific specifications
  form=OLS_II_formulas[i]
  des=des_P65[i]
  testvar=tv[i]
  mynhanesdesign<-eval(parse(text=des))
  mod<-svyglm(form, design=mynhanesdesign)
  aic <- mod$aic
  reg_results<-tidy(mod) %>% 
    mutate(coef=estimate,
           se=std.error,
           p=p.value) %>%
    select(c(term, coef, se, p)) %>% 
    pivot_wider(names_from = term, values_from = c(coef, se, p))
  if(testvar!="NOTEST"){
    test_lrt <- regTermTest(mod, eval(parse(text=testvar)), method="LRT", lrt.approximation="saddlepoint")
    lrt_chi <-test_lrt$chisq
    lrt_df <- test_lrt$df
    lrt_ddf <- test_lrt$ddf
    lrt_p <- test_lrt$p
    lrt_lambda1 <- test_lrt$lambda[1]
    lrt_lambda2 <- test_lrt$lambda[2]
  } else {
    lrt_chi <- NA
    lrt_df <- NA
    lrt_ddf <- NA
    lrt_p <- NA
    lrt_lambda1 <- NA
    lrt_lambda2 <- NA
  }
  testout<-as.data.frame(cbind(lrt_chi, lrt_df, lrt_ddf, lrt_p, lrt_lambda1, lrt_lambda2))
  rough_OLS_II[[i]] <- cbind(reg_results, aic, testout)
}


names(rough_OLS_II) <- str_sub(as.character(OLS_II_formulas), 1, 7)
results_OLS_II <-bind_rows(rough_OLS_II, .id="chemical")


# add indicators so we know which specification is which
results_OLS_II <- results_OLS_II %>%
  mutate(creatinine_indic = case_when(is.na(coef_ln_UCR)==F ~ "yes",
                                      TRUE ~ "no"),
         cotinine_indic = case_when(is.na(coef_ln_COT)==F ~ "yes",
                                    TRUE ~ "no"))


stopifnot(table(results_OLS_II$creatinine_indic)[2]==171)
stopifnot(table(results_OLS_II$cotinine_indic)[2]==129)


# Take out the variables we can't take out of the RDC
table8 <- results_OLS_II %>%
  select(-coef_race_catOther, -se_race_catOther, -p_race_catOther)


# Output the data to a .csv
write_csv(table8, "tables/Table_8.csv")



##########################################################################################
##########################################################################################

### Output Table 9 -  Compute marginal means for CA-Pre, CA-Post, Non-CA-Pre, and Non-CA-Post

##########################################################################################

OLS_II_model_with_EMM <- function(form, des, EMMvar) { 
  # form is the base regression model specification
  # des is the survey design object
  # EMMvar is the groupfactor
  mynhanesdesign<-eval(parse(text=des))
  mod<-svyglm(form, design=mynhanesdesign)
  results<-tidy(mod) %>% 
    mutate(coef=estimate,
           se=std.error,
           p=p.value) %>%
    select(c(term, coef)) %>% 
    pivot_wider(names_from = term, values_from = c(coef))
  means <- as.data.frame(svypredmeans(mod, eval(parse(text=EMMvar)))) %>%
    mutate(group=c("Non_Pre", "CA_Pre", "Non_Post", "CA_Post"))%>%
    pivot_wider(names_from=group, values_from=c(mean, SE))
  out<-cbind(results, means)
  return(out) 
}


OLS_II_EMM_formulas <- c(
  "ln_MZP ~ female + child + wcba + race_cat + PIR_cat",
  "ln_MZP ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MBP ~ female + child + wcba + race_cat + PIR_cat",
  "ln_MBP ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_MHH ~ female + child + wcba + race_cat + PIR_cat",
  "ln_MHH ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_CNP ~ female + child + wcba + race_cat + PIR_cat",
  "ln_CNP ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_COP ~ female + child + wcba + race_cat + PIR_cat",
  "ln_COP ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_BPH ~ female + child + wcba + race_cat + PIR_cat",
  "ln_BPH ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_AAM ~ female + child + wcba + race_cat + PIR_cat",
  "ln_AAM ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_AAM ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_AAM ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_ATC ~ female + child + wcba + race_cat + PIR_cat",
  "ln_ATC ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_ATC ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_ATC ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_BMA ~ female + child + wcba + race_cat + PIR_cat",
  "ln_BMA ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_BMA ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_BMA ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_VCF ~ female + wcba + race_cat + PIR_cat",
  "ln_VTO ~ female + wcba + race_cat + PIR_cat",
  "ln_VTO ~ female + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P01 ~ female + child + wcba + race_cat + PIR_cat",
  "ln_P01 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P01 ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P01 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ln_P02 ~ female + child + wcba + race_cat + PIR_cat",
  "ln_P02 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ln_P02 ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ln_P02 ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT"
)



OLS_II_EMM_des <- as.list(c(rep("phthalates18_design", 4), 
                     rep("phthalates16_design", 2), 
                     rep("phthalates12_design", 4), 
                     rep("phenols14_design", 2), 
                     rep("urineVOC8_design", 12), 
                     rep("bloodVOC18_design", 1), 
                     rep("bloodVOC16_design", 2), 
                     rep("PAH14_design", 8)
))


stopifnot(length(OLS_II_EMM_formulas)==length(OLS_II_EMM_des))

OLS_II_EMM_groupfactor <- c(
  "~interaction(CA, P65_URXMZP)",
  "~interaction(CA, P65_URXMZP)",
  "~interaction(CA, P65_URXMBP)",
  "~interaction(CA, P65_URXMBP)",
  "~interaction(CA, P65_URXMHH)",
  "~interaction(CA, P65_URXMHH)",
  "~interaction(CA, P65_URXCNP)",
  "~interaction(CA, P65_URXCNP)",
  "~interaction(CA, P65_URXCOP)",
  "~interaction(CA, P65_URXCOP)",
  "~interaction(CA, P65_URXBPH)",
  "~interaction(CA, P65_URXBPH)",
  "~interaction(CA, P65_URXAAM)",
  "~interaction(CA, P65_URXAAM)",
  "~interaction(CA, P65_URXAAM)",
  "~interaction(CA, P65_URXAAM)",
  "~interaction(CA, P65_URXATC)",
  "~interaction(CA, P65_URXATC)",
  "~interaction(CA, P65_URXATC)",
  "~interaction(CA, P65_URXATC)",
  "~interaction(CA, P65_URXBMA)",
  "~interaction(CA, P65_URXBMA)",
  "~interaction(CA, P65_URXBMA)",
  "~interaction(CA, P65_URXBMA)",
  "~interaction(CA, P65_LBXVCF)",
  "~interaction(CA, P65_LBXVTO)",
  "~interaction(CA, P65_LBXVTO)",
  "~interaction(CA, P65_URXP01)",
  "~interaction(CA, P65_URXP01)",
  "~interaction(CA, P65_URXP01)",
  "~interaction(CA, P65_URXP01)",
  "~interaction(CA, P65_URXP02)",
  "~interaction(CA, P65_URXP02)",
  "~interaction(CA, P65_URXP02)",
  "~interaction(CA, P65_URXP02)"
)

stopifnot(length(OLS_II_EMM_formulas)==length(OLS_II_EMM_groupfactor))


rough_OLS_II_means <- pmap(list(OLS_II_EMM_formulas, OLS_II_EMM_des, OLS_II_EMM_groupfactor), OLS_II_model_with_EMM)
names(rough_OLS_II_means) <- str_sub(as.character(OLS_II_EMM_formulas), 1, 7)
OLS_II_means <-bind_rows(rough_OLS_II_means, .id="chemical")

OLS_II_means <- OLS_II_means %>%
  mutate(creatinine_indic = case_when(is.na(ln_UCR)==F ~ "yes",
                                      TRUE ~ "no"),
         cotinine_indic = case_when(is.na(ln_COT)==F ~ "yes",
                                    TRUE ~ "no"))


stopifnot(table(OLS_II_means$creatinine_indic)[2]==16)
stopifnot(table(OLS_II_means$cotinine_indic)[2]==11)


table9 <- OLS_II_means %>%
  select(chemical, creatinine_indic, cotinine_indic, mean_Non_Pre, mean_CA_Pre, mean_Non_Post, mean_CA_Post, SE_Non_Pre, SE_CA_Pre, SE_Non_Post, SE_CA_Post)


# Output the data to a .csv
write_csv(table9, "tables/Table_9.csv")


##########################################################################################
##########################################################################################

### Output Table 10 -  Regression Output from Logit Models II.L.a-c

##########################################################################################


logit_II_formulas <- c(
  "MZP_high ~ P65_URXMZP + female + child + wcba + race_cat + PIR_cat",
  "MZP_high ~ P65_URXMZP + CA + P65_URXMZP_CA + female + child + wcba + race_cat + PIR_cat",
  "MZP_high ~ P65_URXMZP + CA + P65_URXMZP_CA + time + female + child + wcba + race_cat + PIR_cat",
  "MZP_high ~ P65_URXMZP + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MZP_high ~ P65_URXMZP + CA + P65_URXMZP_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MZP_high ~ P65_URXMZP + CA + P65_URXMZP_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MBP_high ~ P65_URXMBP + female + child + wcba + race_cat + PIR_cat",
  "MBP_high ~ P65_URXMBP + CA + P65_URXMBP_CA + female + child + wcba + race_cat + PIR_cat",
  "MBP_high ~ P65_URXMBP + CA + P65_URXMBP_CA + time + female + child + wcba + race_cat + PIR_cat",
  "MBP_high ~ P65_URXMBP + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MBP_high ~ P65_URXMBP + CA + P65_URXMBP_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MBP_high ~ P65_URXMBP + CA + P65_URXMBP_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MEP_high ~ female + child + wcba + race_cat + PIR_cat",
  "MEP_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "MEP_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "MEP_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MEP_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MEP_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MIB_high ~ female + child + wcba + race_cat + PIR_cat",
  "MIB_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "MIB_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "MIB_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MIB_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MIB_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MHH_high ~ P65_URXMHH + female + child + wcba + race_cat + PIR_cat",
  "MHH_high ~ P65_URXMHH + CA + P65_URXMHH_CA + female + child + wcba + race_cat + PIR_cat",
  "MHH_high ~ P65_URXMHH + CA + P65_URXMHH_CA + time + female + child + wcba + race_cat + PIR_cat",
  "MHH_high ~ P65_URXMHH + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MHH_high ~ P65_URXMHH + CA + P65_URXMHH_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MHH_high ~ P65_URXMHH + CA + P65_URXMHH_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "CNP_high ~ P65_URXCNP + female + child + wcba + race_cat + PIR_cat",
  "CNP_high ~ P65_URXCNP + CA + P65_URXCNP_CA + female + child + wcba + race_cat + PIR_cat",
  "CNP_high ~ P65_URXCNP + CA + P65_URXCNP_CA + time + female + child + wcba + race_cat + PIR_cat",
  "CNP_high ~ P65_URXCNP + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "CNP_high ~ P65_URXCNP + CA + P65_URXCNP_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "CNP_high ~ P65_URXCNP + CA + P65_URXCNP_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "COP_high ~ P65_URXCOP + female + child + wcba + race_cat + PIR_cat",
  "COP_high ~ P65_URXCOP + CA + P65_URXCOP_CA + female + child + wcba + race_cat + PIR_cat",
  "COP_high ~ P65_URXCOP + CA + P65_URXCOP_CA + time + female + child + wcba + race_cat + PIR_cat",
  "COP_high ~ P65_URXCOP + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "COP_high ~ P65_URXCOP + CA + P65_URXCOP_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "COP_high ~ P65_URXCOP + CA + P65_URXCOP_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "BPH_high ~ P65_URXBPH + female + child + wcba + race_cat + PIR_cat",
  "BPH_high ~ P65_URXBPH + CA + P65_URXBPH_CA + female + child + wcba + race_cat + PIR_cat",
  "BPH_high ~ P65_URXBPH + CA + P65_URXBPH_CA + time + female + child + wcba + race_cat + PIR_cat",
  "BPH_high ~ P65_URXBPH + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "BPH_high ~ P65_URXBPH + CA + P65_URXBPH_CA + female + child + wcba + race_cat + PIR_cat+ln_UCR",
  "BPH_high ~ P65_URXBPH + CA + P65_URXBPH_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "BP3_high ~ female + child + wcba + race_cat + PIR_cat",
  "BP3_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "BP3_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "BP3_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "BP3_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "BP3_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "TRS_high ~ female + child + wcba + race_cat + PIR_cat",
  "TRS_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "TRS_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "TRS_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "TRS_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "TRS_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "X14D_high ~ female + child + wcba + race_cat + PIR_cat",
  "X14D_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "X14D_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "X14D_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "X14D_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "X14D_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "DCB_high ~ female + child + wcba + race_cat + PIR_cat",
  "DCB_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "DCB_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "DCB_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "DCB_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "DCB_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MPB_high ~ female + child + wcba + race_cat + PIR_cat",
  "MPB_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "MPB_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "MPB_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MPB_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MPB_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "PPB_high ~ female + child + wcba + race_cat + PIR_cat",
  "PPB_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "PPB_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "PPB_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "PPB_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "PPB_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "BPS_high ~ female + child + wcba + race_cat + PIR_cat",
  "BPS_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "BPS_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "BPS_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "BPS_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "BPS_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "X2MH_high ~ female + child + wcba + race_cat + PIR_cat",
  "X2MH_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "X2MH_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "X2MH_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "X2MH_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "X2MH_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "X2MH_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "X2MH_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "X2MH_high ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "X2MH_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "X2MH_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "X2MH_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "X34M_high ~ female + child + wcba + race_cat + PIR_cat",
  "X34M_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "X34M_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "X34M_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "X34M_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "X34M_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "X34M_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "X34M_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "X34M_high ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "X34M_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "X34M_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "X34M_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "AAM_high ~ P65_URXAAM + female + child + wcba + race_cat + PIR_cat",
  "AAM_high ~ P65_URXAAM + CA + P65_URXAAM_CA + female + child + wcba + race_cat + PIR_cat",
  "AAM_high ~ P65_URXAAM + CA + P65_URXAAM_CA + time + female + child + wcba + race_cat + PIR_cat",
  "AAM_high ~ P65_URXAAM + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "AAM_high ~ P65_URXAAM + CA + P65_URXAAM_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "AAM_high ~ P65_URXAAM + CA + P65_URXAAM_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "AAM_high ~ P65_URXAAM + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "AAM_high ~ P65_URXAAM + CA + P65_URXAAM_CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "AAM_high ~ P65_URXAAM + CA + P65_URXAAM_CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "AAM_high ~ P65_URXAAM + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "AAM_high ~ P65_URXAAM + CA + P65_URXAAM_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "AAM_high ~ P65_URXAAM + CA + P65_URXAAM_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ATC_high ~ P65_URXATC + female + child + wcba + race_cat + PIR_cat",
  "ATC_high ~ P65_URXATC + CA + P65_URXATC_CA + female + child + wcba + race_cat + PIR_cat",
  "ATC_high ~ P65_URXATC + CA + P65_URXATC_CA + time + female + child + wcba + race_cat + PIR_cat",
  "ATC_high ~ P65_URXATC + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ATC_high ~ P65_URXATC + CA + P65_URXATC_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ATC_high ~ P65_URXATC + CA + P65_URXATC_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ATC_high ~ P65_URXATC + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ATC_high ~ P65_URXATC + CA + P65_URXATC_CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ATC_high ~ P65_URXATC + CA + P65_URXATC_CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ATC_high ~ P65_URXATC + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ATC_high ~ P65_URXATC + CA + P65_URXATC_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ATC_high ~ P65_URXATC + CA + P65_URXATC_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "BMA_high ~ P65_URXBMA + female + child + wcba + race_cat + PIR_cat",
  "BMA_high ~ P65_URXBMA + CA + P65_URXBMA_CA + female + child + wcba + race_cat + PIR_cat",
  "BMA_high ~ P65_URXBMA + CA + P65_URXBMA_CA + time + female + child + wcba + race_cat + PIR_cat",
  "BMA_high ~ P65_URXBMA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "BMA_high ~ P65_URXBMA + CA + P65_URXBMA_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "BMA_high ~ P65_URXBMA + CA + P65_URXBMA_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "BMA_high ~ P65_URXBMA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "BMA_high ~ P65_URXBMA + CA + P65_URXBMA_CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "BMA_high ~ P65_URXBMA + CA + P65_URXBMA_CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "BMA_high ~ P65_URXBMA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "BMA_high ~ P65_URXBMA + CA + P65_URXBMA_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "BMA_high ~ P65_URXBMA + CA + P65_URXBMA_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "CYM_high ~ female + child + wcba + race_cat + PIR_cat",
  "CYM_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "CYM_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "CYM_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "CYM_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "CYM_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "CYM_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "CYM_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "CYM_high ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "CYM_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "CYM_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "CYM_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "DHB_high ~ female + child + wcba + race_cat + PIR_cat",
  "DHB_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "DHB_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "DHB_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "DHB_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "DHB_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "DHB_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "DHB_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "DHB_high ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "DHB_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "DHB_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "DHB_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "HP2_high ~ female + child + wcba + race_cat + PIR_cat",
  "HP2_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "HP2_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "HP2_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "HP2_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "HP2_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "HP2_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "HP2_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "HP2_high ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "HP2_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "HP2_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "HP2_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "MAD_high ~ female + child + wcba + race_cat + PIR_cat",
  "MAD_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "MAD_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "MAD_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MAD_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MAD_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MAD_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "MAD_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "MAD_high ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "MAD_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "MAD_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "MAD_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "MB3_high ~ female + child + wcba + race_cat + PIR_cat",
  "MB3_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "MB3_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "MB3_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MB3_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MB3_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MB3_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "MB3_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "MB3_high ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "MB3_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "MB3_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "MB3_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "VCF_high ~ P65_LBXVCF + female + wcba + race_cat + PIR_cat",
  "VCF_high ~ P65_LBXVCF + CA + P65_LBXVCF_CA + female + wcba + race_cat + PIR_cat",
  "VCF_high ~ P65_LBXVCF + CA + P65_LBXVCF_CA +time + female + wcba + race_cat + PIR_cat",
  "VTO_high ~ P65_LBXVTO + female + wcba + race_cat + PIR_cat",
  "VTO_high ~ P65_LBXVTO + CA + P65_LBXVTO_CA + female + wcba + race_cat + PIR_cat",
  "VTO_high ~ P65_LBXVTO + CA + P65_LBXVTO_CA +time + female + wcba + race_cat + PIR_cat",
  "VTO_high ~ P65_LBXVTO + female + wcba + race_cat + PIR_cat + ln_COT",
  "VTO_high ~ P65_LBXVTO + CA + P65_LBXVTO_CA + female + wcba + race_cat + PIR_cat + ln_COT",
  "VTO_high ~ P65_LBXVTO + CA + P65_LBXVTO_CA +time + female + wcba + race_cat + PIR_cat + ln_COT",
  "P01_high ~ P65_URXP01 + female + child + wcba + race_cat + PIR_cat",
  "P01_high ~ P65_URXP01 + CA + P65_URXP01_CA + female + child + wcba + race_cat + PIR_cat",
  "P01_high ~ P65_URXP01 + CA + P65_URXP01_CA + time + female + child + wcba + race_cat + PIR_cat",
  "P01_high ~ P65_URXP01 + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P01_high ~ P65_URXP01 + CA + P65_URXP01_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P01_high ~ P65_URXP01 + CA + P65_URXP01_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P01_high ~ P65_URXP01 + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P01_high ~ P65_URXP01 + CA + P65_URXP01_CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P01_high ~ P65_URXP01 + CA + P65_URXP01_CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P01_high ~ P65_URXP01 + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P01_high ~ P65_URXP01 + CA + P65_URXP01_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P01_high ~ P65_URXP01 + CA + P65_URXP01_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P02_high ~ P65_URXP02 + female + child + wcba + race_cat + PIR_cat",
  "P02_high ~ P65_URXP02 + CA + P65_URXP02_CA + female + child + wcba + race_cat + PIR_cat",
  "P02_high ~ P65_URXP02 + CA + P65_URXP02_CA + time + female + child + wcba + race_cat + PIR_cat",
  "P02_high ~ P65_URXP02 + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P02_high ~ P65_URXP02 + CA + P65_URXP02_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P02_high ~ P65_URXP02 + CA + P65_URXP02_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P02_high ~ P65_URXP02 + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P02_high ~ P65_URXP02 + CA + P65_URXP02_CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P02_high ~ P65_URXP02 + CA + P65_URXP02_CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P02_high ~ P65_URXP02 + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P02_high ~ P65_URXP02 + CA + P65_URXP02_CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P02_high ~ P65_URXP02 + CA + P65_URXP02_CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P03_high ~ female + child + wcba + race_cat + PIR_cat",
  "P03_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "P03_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "P03_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P03_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P03_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P03_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P03_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P03_high ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P03_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P03_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P03_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P04_high ~ female + child + wcba + race_cat + PIR_cat",
  "P04_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "P04_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "P04_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P04_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P04_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P04_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P04_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P04_high ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P04_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P04_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P04_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P06_high ~ female + child + wcba + race_cat + PIR_cat",
  "P06_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "P06_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "P06_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P06_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P06_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P06_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P06_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P06_high ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P06_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P06_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P06_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P10_high ~ female + child + wcba + race_cat + PIR_cat",
  "P10_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "P10_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "P10_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P10_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P10_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P10_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P10_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P10_high ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P10_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P10_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P10_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P05_high ~ female + child + wcba + race_cat + PIR_cat",
  "P05_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "P05_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "P05_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P05_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P05_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P05_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P05_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P05_high ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P05_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P05_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P05_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P07_high ~ female + child + wcba + race_cat + PIR_cat",
  "P07_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "P07_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "P07_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P07_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P07_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P07_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P07_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P07_high ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P07_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P07_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P07_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P17_high ~ female + child + wcba + race_cat + PIR_cat",
  "P17_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "P17_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "P17_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P17_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P17_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P17_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P17_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P17_high ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P17_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P17_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P17_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P19_high ~ female + child + wcba + race_cat + PIR_cat",
  "P19_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "P19_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "P19_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P19_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P19_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P19_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P19_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P19_high ~ CA + time +  female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P19_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P19_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P19_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "MPAH_high ~ female + wcba + race_cat + PIR_cat",
  "MPAH_high ~ CA + female + wcba + race_cat + PIR_cat",
  "MPAH_high ~ CA + time + female + wcba + race_cat + PIR_cat",
  "PFDE_high ~ female + wcba + race_cat + PIR_cat",
  "PFDE_high ~ CA + female + wcba + race_cat + PIR_cat",
  "PFDE_high ~ CA + time + female + wcba + race_cat + PIR_cat",
  "PFHS_high ~ female + wcba + race_cat + PIR_cat",
  "PFHS_high ~ CA + female + wcba + race_cat + PIR_cat",
  "PFHS_high ~ CA + time + female + wcba + race_cat + PIR_cat",
  "PFNA_high ~ female + wcba + race_cat + PIR_cat",
  "PFNA_high ~ CA + female + wcba + race_cat + PIR_cat",
  "PFNA_high ~ CA + time + female + wcba + race_cat + PIR_cat",
  "PFOA_high ~ female + wcba + race_cat + PIR_cat",
  "PFOA_high ~ CA + female + wcba + race_cat + PIR_cat",
  "PFOA_high ~ CA + time + female + wcba + race_cat + PIR_cat",
  "PFOS_high ~ female + wcba + race_cat + PIR_cat",
  "PFOS_high ~ CA + female + wcba + race_cat + PIR_cat",
  "PFOS_high ~ CA + time + female + wcba + race_cat + PIR_cat",
  "BPB_high ~ female + child + wcba + race_cat + PIR_cat",
  "BPB_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "BPB_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "THG_high ~ female + child + wcba + race_cat + PIR_cat",
  "THG_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "THG_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "UCD_high ~ female + child + wcba + race_cat + PIR_cat",
  "UCD_high ~ CA + female + child + wcba + race_cat + PIR_cat",
  "UCD_high ~ CA + time + female + child + wcba + race_cat + PIR_cat",
  "UCD_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "UCD_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "UCD_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "UCD_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "UCD_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "UCD_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_COT",
  "UCD_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "UCD_high ~ CA + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "UCD_high ~ CA + time + female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT"
)

stopifnot(length(logit_II_formulas)==length(des_P65))

rough_logit_II<-NULL
for(i in 1:length(logit_II_formulas)) {
  form=logit_II_formulas[i]
  des=des_P65[i]
  testvar=tv[i]
  mynhanesdesign<-eval(parse(text=des))
  mod<-svyglm(form, design=mynhanesdesign, family=binomial())
  aic <- mod$aic
  reg_results<-tidy(mod) %>% 
    mutate(coef=estimate,
           se=std.error,
           p=p.value) %>%
    select(c(term, coef, se, p)) %>% 
    pivot_wider(names_from = term, values_from = c(coef, se, p))
  if(testvar!="NOTEST"){
    test_lrt <- regTermTest(mod, eval(parse(text=testvar)), method="LRT", lrt.approximation="saddlepoint")
    lrt_chi <-test_lrt$chisq
    lrt_df <- test_lrt$df
    lrt_ddf <- test_lrt$ddf
    lrt_p <- test_lrt$p
    lrt_lambda1 <- test_lrt$lambda[1]
    lrt_lambda2 <- test_lrt$lambda[2]
  } else {
    lrt_chi <- NA
    lrt_df <- NA
    lrt_ddf <- NA
    lrt_p <- NA
    lrt_lambda1 <- NA
    lrt_lambda2 <- NA
  }
  testout<-as.data.frame(cbind(lrt_chi, lrt_df, lrt_ddf, lrt_p, lrt_lambda1, lrt_lambda2))
  rough_logit_II[[i]] <- cbind(reg_results, aic, testout)
}


names(rough_logit_II) <- str_sub(as.character(logit_II_formulas), 1, 7)
results_logit_II <-bind_rows(rough_logit_II, .id="chemical")


# add indicators so we know which specification is which
results_logit_II <- results_logit_II %>%
  mutate(creatinine_indic = case_when(is.na(coef_ln_UCR)==F ~ "yes",
                                      TRUE ~ "no"),
         cotinine_indic = case_when(is.na(coef_ln_COT)==F ~ "yes",
                                    TRUE ~ "no"))

stopifnot(table(results_logit_II$creatinine_indic)[2]==171)
stopifnot(table(results_logit_II$cotinine_indic)[2]==129)


# Take out the variables we can't take out of the RDC
table10 <- results_logit_II %>%
  select(-coef_race_catOther, -se_race_catOther, -p_race_catOther)


# Output the data to a .csv
write_csv(table10, "tables/Table_10.csv")


##########################################################################################
##########################################################################################

### Output Table 11 -  Compute marginal means for CA-Pre, CA-Post, Non-CA-Pre, and Non-CA-Post

##########################################################################################

logit_II_model_with_EMM <- function(form, des, EMMvar) { 
  # form is the base regression model specification
  # des is the survey design object
  # EMMvar is the groupfactor
  mynhanesdesign<-eval(parse(text=des))
  mod<-svyglm(form, design=mynhanesdesign, family=binomial())
  results<-tidy(mod) %>% 
    mutate(coef=estimate,
           se=std.error,
           p=p.value) %>%
    select(c(term, coef)) %>% 
    pivot_wider(names_from = term, values_from = c(coef))
  means <- as.data.frame(svypredmeans(mod, eval(parse(text=EMMvar)))) %>%
    mutate(group=c("Non_Pre", "CA_Pre", "Non_Post", "CA_Post"))%>%   
    pivot_wider(names_from=group, values_from=c(mean, SE))
  out<-cbind(results, means)
  return(out) 
}


logit_II_EMM_formulas <- c(
  "MZP_high ~ female + child + wcba + race_cat + PIR_cat",
  "MZP_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MBP_high ~ female + child + wcba + race_cat + PIR_cat",
  "MBP_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "MHH_high ~ female + child + wcba + race_cat + PIR_cat",
  "MHH_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "CNP_high ~ female + child + wcba + race_cat + PIR_cat",
  "CNP_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "COP_high ~ female + child + wcba + race_cat + PIR_cat",
  "COP_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "BPH_high ~ female + child + wcba + race_cat + PIR_cat",
  "BPH_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "AAM_high ~ female + child + wcba + race_cat + PIR_cat",
  "AAM_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "AAM_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "AAM_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "ATC_high ~ female + child + wcba + race_cat + PIR_cat",
  "ATC_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "ATC_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "ATC_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "BMA_high ~ female + child + wcba + race_cat + PIR_cat",
  "BMA_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "BMA_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "BMA_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "VCF_high ~ female + wcba + race_cat + PIR_cat",
  "VTO_high ~ female + wcba + race_cat + PIR_cat",
  "VTO_high ~ female + wcba + race_cat + PIR_cat + ln_COT",
  "P01_high ~ female + child + wcba + race_cat + PIR_cat",
  "P01_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P01_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P01_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT",
  "P02_high ~ female + child + wcba + race_cat + PIR_cat",
  "P02_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR",
  "P02_high ~ female + child + wcba + race_cat + PIR_cat + ln_COT",
  "P02_high ~ female + child + wcba + race_cat + PIR_cat + ln_UCR + ln_COT"
)


rough_logit_II_means <- pmap(list(logit_II_EMM_formulas, OLS_II_EMM_des, OLS_II_EMM_groupfactor), logit_II_model_with_EMM)
names(rough_logit_II_means) <- str_sub(as.character(logit_II_EMM_formulas), 1, 8)
logit_II_means <-bind_rows(rough_logit_II_means, .id="chemical")

logit_II_means <- logit_II_means %>%
  mutate(creatinine_indic = case_when(is.na(ln_UCR)==F ~ "yes",
                                      TRUE ~ "no"),
         cotinine_indic = case_when(is.na(ln_COT)==F ~ "yes",
                                    TRUE ~ "no"))

stopifnot(table(logit_II_means$creatinine_indic)[2]==16)
stopifnot(table(logit_II_means$cotinine_indic)[2]==11)

table11 <- logit_II_means %>%
  select(chemical, creatinine_indic, cotinine_indic, mean_Non_Pre, mean_CA_Pre, mean_Non_Post, mean_CA_Post, SE_Non_Pre, SE_CA_Pre, SE_Non_Post, SE_CA_Post)


# Output the data to a .csv
write_csv(table11, "tables/Table_11.csv")




##########################################################################################
##########################################################################################

### Output Table 12 -  Quantile Regression Output from Models II.a-c

# Run quantile regressions of the 25th, 75th, and 95th percentiles

##########################################################################################


# D., this section takes a long time to run (I had to run it overnight on my computer)
# Also, right now we are boot-strapping the survey designs with 100 replicates
# We would like to increase this to 1000 replicates if there is sufficient computing power


# Function to run the quantile regressions
qfit <- function(regform, tau, bd) {
# regform is the regression formula
# tau is the percentile we are modeling
# bd is the survey design object with replicates weights
  a <- paste0("quote(coef(rq(",regform,", ",tau,", weights=.weights, method='fn')))")
  desb<-eval(parse(text=bd))
  try({mod <- withReplicates(desb, eval(parse(text=a)))
  V <- attr(mod, "var")
  se <- sqrt(diag(V))
  val <- cbind(names(mod), as.matrix(mod), se)
  colnames(val) <- c("term","coef", "se") 
  val2<-as.data.frame(val) %>%
  pivot_wider(names_from = term, values_from = c(coef, se))
  val2$cov_CA_P65 <- V[2,4]
  return(val2)})
}


# regform input will be the same as table 8: OLS_II_formulas but we will need to repeat it three times
OLS_II_formulas_quantile <- c(OLS_II_formulas, OLS_II_formulas, OLS_II_formulas)
 
# Make list of tau values
tau_P65 <- as.list(c(rep("0.25", 375),
                     rep("0.75", 375),
                     rep("0.95", 375)
                     ))


# Convert the survey design objects to use replicate weights
boot_phthalates18 <- as.svrepdesign(phthalates18_design, type="bootstrap", replicates=100)
boot_phthalates16 <- as.svrepdesign(phthalates16_design, type="bootstrap", replicates=100)
boot_phthalates12 <- as.svrepdesign(phthalates12_design, type="bootstrap", replicates=100)
boot_phenols14 <- as.svrepdesign(phenols14_design, type="bootstrap", replicates=100)
boot_phenols12 <- as.svrepdesign(phenols12_design, type="bootstrap", replicates=100)
boot_phenols4 <- as.svrepdesign(phenols4_design, type="bootstrap", replicates=100)
boot_urineVOC8 <- as.svrepdesign(urineVOC8_design, type="bootstrap", replicates=100)
boot_bloodVOC18 <- as.svrepdesign(bloodVOC18_design, type="bootstrap", replicates=100)
boot_bloodVOC16 <- as.svrepdesign(bloodVOC16_design, type="bootstrap", replicates=100)
boot_PAH14<- as.svrepdesign(PAH14_design, type="bootstrap", replicates=100)
boot_PAH12 <- as.svrepdesign(PAH12_design, type="bootstrap", replicates=100)
boot_PAH10<- as.svrepdesign(PAH10_design, type="bootstrap", replicates=100)
boot_PAH6<- as.svrepdesign(PAH6_design, type="bootstrap", replicates=100)
boot_pfas14<- as.svrepdesign(pfas14_design, type="bootstrap", replicates=100)
boot_lead_merc18<- as.svrepdesign(lead_merc18_design, type="bootstrap", replicates=100)
boot_cadmium18<- as.svrepdesign(cadmium18_design, type="bootstrap", replicates=100)


boot_des_P65 <- as.list(c(rep("boot_phthalates18", 18), 
                          rep("boot_phthalates16", 12), 
                          rep("boot_phthalates12", 12), 
                          rep("boot_phenols14", 30),
                          rep("boot_phenols12", 12), 
                          rep("boot_phenols4", 6),
                          rep("boot_urineVOC8", 120), 
                          rep("boot_bloodVOC18", 3), 
                          rep("boot_bloodVOC16", 6), 
                          rep("boot_PAH14", 72), 
                          rep("boot_PAH12", 24), 
                          rep("boot_PAH10", 12), 
                          rep("boot_PAH6", 12), 
                          rep("boot_pfas14", 18), 
                          rep("boot_lead_merc18", 6), 
                          rep("boot_cadmium18", 12),
                          rep("boot_phthalates18", 18), 
                          rep("boot_phthalates16", 12), 
                          rep("boot_phthalates12", 12), 
                          rep("boot_phenols14", 30),
                          rep("boot_phenols12", 12), 
                          rep("boot_phenols4", 6),
                          rep("boot_urineVOC8", 120), 
                          rep("boot_bloodVOC18", 3), 
                          rep("boot_bloodVOC16", 6), 
                          rep("boot_PAH14", 72), 
                          rep("boot_PAH12", 24), 
                          rep("boot_PAH10", 12), 
                          rep("boot_PAH6", 12), 
                          rep("boot_pfas14", 18), 
                          rep("boot_lead_merc18", 6), 
                          rep("boot_cadmium18", 12),
                          rep("boot_phthalates18", 18), 
                          rep("boot_phthalates16", 12), 
                          rep("boot_phthalates12", 12), 
                          rep("boot_phenols14", 30),
                          rep("boot_phenols12", 12), 
                          rep("boot_phenols4", 6),
                          rep("boot_urineVOC8", 120), 
                          rep("boot_bloodVOC18", 3), 
                          rep("boot_bloodVOC16", 6), 
                          rep("boot_PAH14", 72), 
                          rep("boot_PAH12", 24), 
                          rep("boot_PAH10", 12), 
                          rep("boot_PAH6", 12), 
                          rep("boot_pfas14", 18), 
                          rep("boot_lead_merc18", 6), 
                          rep("boot_cadmium18", 12)
))
stopifnot(length(OLS_II_formulas_quantile)==length(boot_des_P65))

# Run all 1125 specifications through the qfit function
rough_OLS_II_quant <- pmap(list(OLS_II_formulas_quantile, tau_P65, boot_des_P65), qfit)
names(rough_OLS_II_quant) <- paste0(str_sub(as.character(OLS_II_formulas_quantile)), "***", as.character(tau_P65))

# Drop the specifications that didn't work
short_OLS_II_quant <- NULL
name_short_OLS_II_quant <-NULL
j=0
for(i in 1:length(rough_OLS_II_quant)) {
  if(is_tibble(rough_OLS_II_quant[[i]])) {
    j=j+1
    short_OLS_II_quant[j]<-rough_OLS_II_quant[i]
    name_short_OLS_II_quant[j]<-names(rough_OLS_II_quant[i])
  } else {
    j=j
  }
}

names(short_OLS_II_quant) <- name_short_OLS_II_quant
results_OLS_II_quant <-bind_rows(short_OLS_II_quant, .id="chemical_quantile")


table12 <- results_OLS_II_quant %>%
  select(-coef_race_catOther, -se_race_catOther)


# Output the data to a .csv
write_csv(table12, "tables/Table_12.csv")


