# This file creates the 95th percentile cut-offs using the NHANES public data
# This file is a necessary pre-cursor to "RDC - Create Public Dataset_publish.R"

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

# Function to create the 95th percentiles
sum_q95 <- function(chem, des) {
  # chem = chemical variable name
  # des = survey design object
  a <-svyquantile(chem, design=des, quantiles=0.95, na.rm=TRUE, method="constant", f=1, interval.type="betaWald")
  b <- as.data.frame(a)
  colnames(b) <- c("q95")
  out<-b
  out2<-remove_rownames(out)
  return(out2)
}


# load in creatinine first so we have it for all the relevant chemicals
creat9900 <- nhanes_load_data("LAB16", "1999-2000", demographics = FALSE) %>%
  select(SEQN, URXUCR) 

creat0102 <- nhanes_load_data("L16_B", "2001-2002", demographics = FALSE) %>%
  select(SEQN, URXUCR) 

creat0304 <- nhanes_load_data("L16_C", "2003-2004", demographics = FALSE) %>%
  select(SEQN, URXUCR) 

creat0506 <- nhanes_load_data("ALB_CR_D", "2005-2006", demographics = FALSE) %>%
  select(SEQN, URXUCR) 

creat0708 <- nhanes_load_data("ALB_CR_E", "2007-2008", demographics = FALSE) %>%
  select(SEQN, URXUCR) 

creat0910 <- nhanes_load_data("ALB_CR_F", "2009-2010", demographics = FALSE) %>%
  select(SEQN, URXUCR) 

creat1112 <- nhanes_load_data("ALB_CR_G", "2011-2012", demographics = FALSE) %>%
  select(SEQN, URXUCR) 

creat1314 <- nhanes_load_data("ALB_CR_H", "2013-2014", demographics = FALSE) %>%
  select(SEQN, URXUCR) 

creat1516 <- nhanes_load_data("ALB_CR_I", "2015-2016", demographics = FALSE) %>%
  select(SEQN, URXUCR) 


# Phthalates
phthalates9900 <- nhanes_load_data("PHPYPA", "1999-2000", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSPH2YR, URXMBP, URXMEP, URXMZP) %>%
  left_join(creat9900, by="SEQN") %>%
  mutate(MZP_cr=100*URXMZP/URXUCR,
         MBP_cr=100*URXMBP/URXUCR,
         MEP_cr=100*URXMEP/URXUCR)

phthalates9900_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTSPH2YR,
                            nest = TRUE,
                            data = phthalates9900)


phthalates9900_chems<-c(~URXMZP, ~URXMBP, ~URXMEP, ~MZP_cr, ~MBP_cr, ~MEP_cr)

phthalates9900_qrl<-map(phthalates9900_chems, sum_q95, phthalates9900_design)
names(phthalates9900_qrl) <- paste0("q95_",str_sub(as.character(phthalates9900_chems), 2, 7))
phthalates9900_qrdf<-bind_rows(phthalates9900_qrl, .id="chemical")
phthalates9900_95q<- pivot_wider(phthalates9900_qrdf, names_from = "chemical", values_from = "q95" )
phthalates9900_95q$cycle<- "1999-2000"



phthalates0102 <- nhanes_load_data("PHPYPA_B", "2001-2002", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSPH2YR, URXMBP, URXMEP, URXMZP, URXMHH, URXMIB) %>%
  left_join(creat0102, by="SEQN") %>%
  mutate(MZP_cr=100*URXMZP/URXUCR,
         MBP_cr=100*URXMBP/URXUCR,
         MEP_cr=100*URXMEP/URXUCR,
         MHH_cr=100*URXMHH/URXUCR,
         MIB_cr=100*URXMIB/URXUCR)

phthalates0102_design <-svydesign(id = ~SDMVPSU,
                                  strata = ~SDMVSTRA,
                                  weights = ~WTSPH2YR,
                                  nest = TRUE,
                                  data = phthalates0102)

phthalates0102_chems<-c(~URXMZP, ~URXMBP, ~URXMEP, ~URXMHH, ~URXMIB, ~MZP_cr, ~MBP_cr, ~MEP_cr, ~MHH_cr, ~MIB_cr)

phthalates0102_qrl<-map(phthalates0102_chems, sum_q95, phthalates0102_design)
names(phthalates0102_qrl) <- paste0("q95_",str_sub(as.character(phthalates0102_chems), 2, 7))
phthalates0102_qrdf<-bind_rows(phthalates0102_qrl, .id="chemical")
phthalates0102_95q<- pivot_wider(phthalates0102_qrdf, names_from = "chemical", values_from = "q95" )
phthalates0102_95q$cycle<- "2001-2002"



phthalates0304 <- nhanes_load_data("L24PH_C", "2003-2004", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, URXMBP, URXMEP, URXMHH, URXMIB, URXMZP) %>%
  left_join(creat0304, by="SEQN") %>%
  mutate(MZP_cr=100*URXMZP/URXUCR,
         MBP_cr=100*URXMBP/URXUCR,
         MEP_cr=100*URXMEP/URXUCR,
         MHH_cr=100*URXMHH/URXUCR,
         MIB_cr=100*URXMIB/URXUCR)

phthalates0304_design <-svydesign(id = ~SDMVPSU,
                                  strata = ~SDMVSTRA,
                                  weights = ~WTSB2YR,
                                  nest = TRUE,
                                  data = phthalates0304)

phthalates0304_chems<-c(~URXMZP, ~URXMBP, ~URXMEP, ~URXMHH, ~URXMIB, ~MZP_cr, ~MBP_cr, ~MEP_cr, ~MHH_cr, ~MIB_cr)

phthalates0304_qrl<-map(phthalates0304_chems, sum_q95, phthalates0304_design)
names(phthalates0304_qrl) <- paste0("q95_",str_sub(as.character(phthalates0304_chems), 2, 7))
phthalates0304_qrdf<-bind_rows(phthalates0304_qrl, .id="chemical")
phthalates0304_95q<- pivot_wider(phthalates0304_qrdf, names_from = "chemical", values_from = "q95" )
phthalates0304_95q$cycle<- "2003-2004"


phthalates0506 <- nhanes_load_data("PHTHTE_D", "2005-2006", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, URXCNP, URXCOP, URXMBP, URXMEP, URXMHH, URXMIB, URXMZP) %>%
  left_join(creat0506, by="SEQN") %>%
  mutate(MZP_cr=100*URXMZP/URXUCR,
         MBP_cr=100*URXMBP/URXUCR,
         MEP_cr=100*URXMEP/URXUCR,
         MHH_cr=100*URXMHH/URXUCR,
         MIB_cr=100*URXMIB/URXUCR,
         CNP_cr=100*URXCNP/URXUCR,
         COP_cr=100*URXCOP/URXUCR)


phthalates0506_design <-svydesign(id = ~SDMVPSU,
                                  strata = ~SDMVSTRA,
                                  weights = ~WTSB2YR,
                                  nest = TRUE,
                                  data = phthalates0506)

phthalates0506_chems<-c(~URXMZP, ~URXMBP, ~URXMEP, ~URXMHH, ~URXMIB, ~URXCNP, ~URXCOP, ~MZP_cr, ~MBP_cr, ~MEP_cr, ~MHH_cr, ~MIB_cr, ~CNP_cr, ~COP_cr)

phthalates0506_qrl<-map(phthalates0506_chems, sum_q95, phthalates0506_design)
names(phthalates0506_qrl) <- paste0("q95_",str_sub(as.character(phthalates0506_chems), 2, 7))
phthalates0506_qrdf<-bind_rows(phthalates0506_qrl, .id="chemical")
phthalates0506_95q<- pivot_wider(phthalates0506_qrdf, names_from = "chemical", values_from = "q95" )
phthalates0506_95q$cycle<- "2005-2006"


phthalates0708 <- nhanes_load_data("PHTHTE_E", "2007-2008", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, URXCNP, URXCOP, URXMBP, URXMEP, URXMHH, URXMIB, URXMZP) %>%
  left_join(creat0708, by="SEQN") %>%
  mutate(MZP_cr=100*URXMZP/URXUCR,
         MBP_cr=100*URXMBP/URXUCR,
         MEP_cr=100*URXMEP/URXUCR,
         MHH_cr=100*URXMHH/URXUCR,
         MIB_cr=100*URXMIB/URXUCR,
         CNP_cr=100*URXCNP/URXUCR,
         COP_cr=100*URXCOP/URXUCR)


phthalates0708_design <-svydesign(id = ~SDMVPSU,
                                  strata = ~SDMVSTRA,
                                  weights = ~WTSB2YR,
                                  nest = TRUE,
                                  data = phthalates0708)

phthalates0708_chems<-c(~URXMZP, ~URXMBP, ~URXMEP, ~URXMHH, ~URXMIB, ~URXCNP, ~URXCOP, ~MZP_cr, ~MBP_cr, ~MEP_cr, ~MHH_cr, ~MIB_cr, ~CNP_cr, ~COP_cr)

phthalates0708_qrl<-map(phthalates0708_chems, sum_q95, phthalates0708_design)
names(phthalates0708_qrl) <- paste0("q95_",str_sub(as.character(phthalates0708_chems), 2, 7))
phthalates0708_qrdf<-bind_rows(phthalates0708_qrl, .id="chemical")
phthalates0708_95q<- pivot_wider(phthalates0708_qrdf, names_from = "chemical", values_from = "q95" )
phthalates0708_95q$cycle<- "2007-2008"


phthalates0910 <- nhanes_load_data("PHTHTE_F", "2009-2010", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, URXCNP, URXCOP, URXMBP, URXMEP, URXMHH, URXMIB, URXMZP) %>%
  left_join(creat0910, by="SEQN") %>%
  mutate(MZP_cr=100*URXMZP/URXUCR,
         MBP_cr=100*URXMBP/URXUCR,
         MEP_cr=100*URXMEP/URXUCR,
         MHH_cr=100*URXMHH/URXUCR,
         MIB_cr=100*URXMIB/URXUCR,
         CNP_cr=100*URXCNP/URXUCR,
         COP_cr=100*URXCOP/URXUCR)

phthalates0910_design <-svydesign(id = ~SDMVPSU,
                                  strata = ~SDMVSTRA,
                                  weights = ~WTSB2YR,
                                  nest = TRUE,
                                  data = phthalates0910)

phthalates0910_chems<-c(~URXMZP, ~URXMBP, ~URXMEP, ~URXMHH, ~URXMIB, ~URXCNP, ~URXCOP, ~MZP_cr, ~MBP_cr, ~MEP_cr, ~MHH_cr, ~MIB_cr, ~CNP_cr, ~COP_cr)

phthalates0910_qrl<-map(phthalates0910_chems, sum_q95, phthalates0910_design)
names(phthalates0910_qrl) <- paste0("q95_",str_sub(as.character(phthalates0910_chems), 2, 7))
phthalates0910_qrdf<-bind_rows(phthalates0910_qrl, .id="chemical")
phthalates0910_95q<- pivot_wider(phthalates0910_qrdf, names_from = "chemical", values_from = "q95" )
phthalates0910_95q$cycle<- "2009-2010"

phthalates1112 <- nhanes_load_data("PHTHTE_G", "2011-2012", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, URXCNP, URXCOP, URXMBP, URXMEP, URXMHH, URXMIB, URXMZP) %>%
  # note: contains 43 records with WTSA2YR and all phthalate data missing - just going to delete these here
  filter(is.na(WTSA2YR)==F)%>%
  left_join(creat1112, by="SEQN") %>%
  mutate(MZP_cr=100*URXMZP/URXUCR,
         MBP_cr=100*URXMBP/URXUCR,
         MEP_cr=100*URXMEP/URXUCR,
         MHH_cr=100*URXMHH/URXUCR,
         MIB_cr=100*URXMIB/URXUCR,
         CNP_cr=100*URXCNP/URXUCR,
         COP_cr=100*URXCOP/URXUCR)


phthalates1112_design <-svydesign(id = ~SDMVPSU,
                                  strata = ~SDMVSTRA,
                                  weights = ~WTSA2YR,
                                  nest = TRUE,
                                  data = phthalates1112)

phthalates1112_chems<-c(~URXMZP, ~URXMBP, ~URXMEP, ~URXMHH, ~URXMIB, ~URXCNP, ~URXCOP, ~MZP_cr, ~MBP_cr, ~MEP_cr, ~MHH_cr, ~MIB_cr, ~CNP_cr, ~COP_cr)

phthalates1112_qrl<-map(phthalates1112_chems, sum_q95, phthalates1112_design)
names(phthalates1112_qrl) <- paste0("q95_",str_sub(as.character(phthalates1112_chems), 2, 7))
phthalates1112_qrdf<-bind_rows(phthalates1112_qrl, .id="chemical")
phthalates1112_95q<- pivot_wider(phthalates1112_qrdf, names_from = "chemical", values_from = "q95" )
phthalates1112_95q$cycle<- "2011-2012"

phthalates1314 <- nhanes_load_data("PHTHTE_H", "2013-2014", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, URXCNP, URXCOP, URXMBP, URXMEP, URXMHH, URXMIB, URXMZP) %>%
  left_join(creat1314, by="SEQN") %>%
  mutate(MZP_cr=100*URXMZP/URXUCR,
         MBP_cr=100*URXMBP/URXUCR,
         MEP_cr=100*URXMEP/URXUCR,
         MHH_cr=100*URXMHH/URXUCR,
         MIB_cr=100*URXMIB/URXUCR,
         CNP_cr=100*URXCNP/URXUCR,
         COP_cr=100*URXCOP/URXUCR)


phthalates1314_design <-svydesign(id = ~SDMVPSU,
                                  strata = ~SDMVSTRA,
                                  weights = ~WTSB2YR,
                                  nest = TRUE,
                                  data = phthalates1314)

phthalates1314_chems<-c(~URXMZP, ~URXMBP, ~URXMEP, ~URXMHH, ~URXMIB, ~URXCNP, ~URXCOP, ~MZP_cr, ~MBP_cr, ~MEP_cr, ~MHH_cr, ~MIB_cr, ~CNP_cr, ~COP_cr)

phthalates1314_qrl<-map(phthalates1314_chems, sum_q95, phthalates1314_design)
names(phthalates1314_qrl) <- paste0("q95_",str_sub(as.character(phthalates1314_chems), 2, 7))
phthalates1314_qrdf<-bind_rows(phthalates1314_qrl, .id="chemical")
phthalates1314_95q<- pivot_wider(phthalates1314_qrdf, names_from = "chemical", values_from = "q95" )
phthalates1314_95q$cycle<- "2013-2014"

phthalates1516 <- nhanes_load_data("PHTHTE_I", "2015-2016", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, URXCNP, URXCOP, URXMBP, URXMEP, URXMHH, URXMIB, URXMZP) %>%
  left_join(creat1516, by="SEQN") %>%
  mutate(MZP_cr=100*URXMZP/URXUCR,
         MBP_cr=100*URXMBP/URXUCR,
         MEP_cr=100*URXMEP/URXUCR,
         MHH_cr=100*URXMHH/URXUCR,
         MIB_cr=100*URXMIB/URXUCR,
         CNP_cr=100*URXCNP/URXUCR,
         COP_cr=100*URXCOP/URXUCR)

phthalates1516_design <-svydesign(id = ~SDMVPSU,
                                  strata = ~SDMVSTRA,
                                  weights = ~WTSB2YR,
                                  nest = TRUE,
                                  data = phthalates1516)

phthalates1516_chems<-c(~URXMZP, ~URXMBP, ~URXMEP, ~URXMHH, ~URXMIB, ~URXCNP, ~URXCOP, ~MZP_cr, ~MBP_cr, ~MEP_cr, ~MHH_cr, ~MIB_cr, ~CNP_cr, ~COP_cr)

phthalates1516_qrl<-map(phthalates1516_chems, sum_q95, phthalates1516_design)
names(phthalates1516_qrl) <- paste0("q95_",str_sub(as.character(phthalates1516_chems), 2, 7))
phthalates1516_qrdf<-bind_rows(phthalates1516_qrl, .id="chemical")
phthalates1516_95q<- pivot_wider(phthalates1516_qrdf, names_from = "chemical", values_from = "q95" )
phthalates1516_95q$cycle<- "2015-2016"


# Combine the cycles of data together
phthalates_95q <- bind_rows(phthalates9900_95q,
                            phthalates0102_95q,
                            phthalates0304_95q,
                            phthalates0506_95q,
                            phthalates0708_95q,
                            phthalates0910_95q,
                            phthalates1112_95q,
                            phthalates1314_95q,
                            phthalates1516_95q)



# Phenols
# Data from 2003-2016; separate pesticide files for 2003-2012
# Note that for a given cycle, it's the same set of SEQN for both the phenols and the
# pesticide files (setdiff on the SEQN yields 0 observations and also it's the same weights)

phenols0304 <- nhanes_load_data("L24EPH_C", "2003-2004", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSC2YR, URXBPH, URXBP3, URDTRS) %>%
  rename(URXTRS=URDTRS) #correct triclosan variable name

pesticides0304 <- nhanes_load_data("L24PP_C", "2003-2004", demographics = FALSE) %>%
  select(SEQN, URX14D, URXDCB)

phenols0304 <- phenols0304 %>%
  full_join(pesticides0304,by="SEQN") %>%
  left_join(creat0304, by="SEQN") %>%
  mutate(BPH_cr=100*URXBPH/URXUCR,
         BP3_cr=100*URXBP3/URXUCR,
         TRS_cr=100*URXTRS/URXUCR,
         X14Dcr=100*URX14D/URXUCR,
         DCB_cr=100*URXDCB/URXUCR)

phenols0304_design <-svydesign(id = ~SDMVPSU,
                               strata = ~SDMVSTRA,
                               weights = ~WTSC2YR,
                               nest = TRUE,
                               data = phenols0304)

phenols0304_chems<-c(~URXBPH, ~URXBP3, ~URXTRS, ~URX14D, ~URXDCB, ~BPH_cr, ~BP3_cr, ~TRS_cr, ~X14Dcr, ~DCB_cr)

phenols0304_qrl<-map(phenols0304_chems, sum_q95, phenols0304_design)
names(phenols0304_qrl) <- paste0("q95_",str_sub(as.character(phenols0304_chems), 2, 7))
phenols0304_qrdf<-bind_rows(phenols0304_qrl, .id="chemical")
phenols0304_95q<- pivot_wider(phenols0304_qrdf, names_from = "chemical", values_from = "q95" )
phenols0304_95q$cycle<- "2003-2004"



phenols0506 <- nhanes_load_data("EPH_D", "2005-2006", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, URXBPH, URXBP3, URXTRS, URXMPB, URXPPB)

pesticides0506 <- nhanes_load_data("PP_D", "2005-2006", demographics = FALSE) %>%
  select(SEQN, URX14D, URXDCB)

phenols0506 <- phenols0506 %>%
  full_join(pesticides0506, by="SEQN") %>%
  left_join(creat0506, by="SEQN") %>%
  mutate(BPH_cr=100*URXBPH/URXUCR,
         BP3_cr=100*URXBP3/URXUCR,
         TRS_cr=100*URXTRS/URXUCR,
         MPB_cr=100*URXMPB/URXUCR,
         PPB_cr=100*URXPPB/URXUCR,
         X14Dcr=100*URX14D/URXUCR,
         DCB_cr=100*URXDCB/URXUCR)

phenols0506_design <-svydesign(id = ~SDMVPSU,
                               strata = ~SDMVSTRA,
                               weights = ~WTSB2YR,
                               nest = TRUE,
                               data = phenols0506)

phenols0506_chems<-c(~URXBPH, ~URXBP3, ~URXTRS, ~URXMPB, ~URXPPB, ~URX14D, ~URXDCB, ~BPH_cr, ~BP3_cr, ~TRS_cr, ~MPB_cr, ~PPB_cr, ~X14Dcr, ~DCB_cr)

phenols0506_qrl<-map(phenols0506_chems, sum_q95, phenols0506_design)
names(phenols0506_qrl) <- paste0("q95_",str_sub(as.character(phenols0506_chems), 2, 7))
phenols0506_qrdf<-bind_rows(phenols0506_qrl, .id="chemical")
phenols0506_95q<- pivot_wider(phenols0506_qrdf, names_from = "chemical", values_from = "q95" )
phenols0506_95q$cycle<- "2005-2006"


phenols0708 <- nhanes_load_data("EPH_E", "2007-2008", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, URXBPH, URXBP3, URXTRS, URXMPB, URXPPB)

pesticides0708 <- nhanes_load_data("PP_E", "2007-2008", demographics = FALSE) %>%
  select(SEQN, URX14D, URXDCB)

phenols0708 <- phenols0708 %>%
  full_join(pesticides0708, by="SEQN") %>%
  left_join(creat0708, by="SEQN") %>%
  mutate(BPH_cr=100*URXBPH/URXUCR,
         BP3_cr=100*URXBP3/URXUCR,
         TRS_cr=100*URXTRS/URXUCR,
         MPB_cr=100*URXMPB/URXUCR,
         PPB_cr=100*URXPPB/URXUCR,
         X14Dcr=100*URX14D/URXUCR,
         DCB_cr=100*URXDCB/URXUCR)

phenols0708_design <-svydesign(id = ~SDMVPSU,
                               strata = ~SDMVSTRA,
                               weights = ~WTSB2YR,
                               nest = TRUE,
                               data = phenols0708)

phenols0708_chems<-c(~URXBPH, ~URXBP3, ~URXTRS, ~URXMPB, ~URXPPB, ~URX14D, ~URXDCB, ~BPH_cr, ~BP3_cr, ~TRS_cr, ~MPB_cr, ~PPB_cr, ~X14Dcr, ~DCB_cr)

phenols0708_qrl<-map(phenols0708_chems, sum_q95, phenols0708_design)
names(phenols0708_qrl) <- paste0("q95_",str_sub(as.character(phenols0708_chems), 2, 7))
phenols0708_qrdf<-bind_rows(phenols0708_qrl, .id="chemical")
phenols0708_95q<- pivot_wider(phenols0708_qrdf, names_from = "chemical", values_from = "q95" )
phenols0708_95q$cycle<- "2007-2008"



phenols0910 <- nhanes_load_data("EPH_F", "2009-2010", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, URXBPH, URXBP3, URXTRS, URXMPB, URXPPB)

pesticides0910 <- nhanes_load_data("PP_F", "2009-2010", demographics = FALSE) %>%
  select(SEQN, URX14D, URXDCB)

phenols0910 <- phenols0910 %>%
  full_join(pesticides0910, by="SEQN") %>%
  left_join(creat0910, by="SEQN") %>%
  mutate(BPH_cr=100*URXBPH/URXUCR,
         BP3_cr=100*URXBP3/URXUCR,
         TRS_cr=100*URXTRS/URXUCR,
         MPB_cr=100*URXMPB/URXUCR,
         PPB_cr=100*URXPPB/URXUCR,
         X14Dcr=100*URX14D/URXUCR,
         DCB_cr=100*URXDCB/URXUCR)

phenols0910_design <-svydesign(id = ~SDMVPSU,
                               strata = ~SDMVSTRA,
                               weights = ~WTSB2YR,
                               nest = TRUE,
                               data = phenols0910)

phenols0910_chems<-c(~URXBPH, ~URXBP3, ~URXTRS, ~URXMPB, ~URXPPB, ~URX14D, ~URXDCB, ~BPH_cr, ~BP3_cr, ~TRS_cr, ~MPB_cr, ~PPB_cr, ~X14Dcr, ~DCB_cr)

phenols0910_qrl<-map(phenols0910_chems, sum_q95, phenols0910_design)
names(phenols0910_qrl) <- paste0("q95_",str_sub(as.character(phenols0910_chems), 2, 7))
phenols0910_qrdf<-bind_rows(phenols0910_qrl, .id="chemical")
phenols0910_95q<- pivot_wider(phenols0910_qrdf, names_from = "chemical", values_from = "q95" )
phenols0910_95q$cycle<- "2009-2010"


phenols1112 <- nhanes_load_data("EPH_G", "2011-2012", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, URXBPH, URXBP3, URXTRS, URXMPB, URXPPB)

pesticides1112 <- nhanes_load_data("PP_G", "2011-2012", demographics = FALSE) %>%
  select(SEQN, URX14D, URXDCB)

phenols1112 <- phenols1112 %>%
  full_join(pesticides1112, by="SEQN") %>%
  left_join(creat1112, by="SEQN") %>%
  mutate(BPH_cr=100*URXBPH/URXUCR,
         BP3_cr=100*URXBP3/URXUCR,
         TRS_cr=100*URXTRS/URXUCR,
         MPB_cr=100*URXMPB/URXUCR,
         PPB_cr=100*URXPPB/URXUCR,
         X14Dcr=100*URX14D/URXUCR,
         DCB_cr=100*URXDCB/URXUCR) %>%
  filter(is.na(WTSA2YR)==F) # get rid of 43 observations with missing weights and all other data

phenols1112_design <-svydesign(id = ~SDMVPSU,
                               strata = ~SDMVSTRA,
                               weights = ~WTSA2YR,
                               nest = TRUE,
                               data = phenols1112)

phenols1112_chems<-c(~URXBPH, ~URXBP3, ~URXTRS, ~URXMPB, ~URXPPB, ~URX14D, ~URXDCB, ~BPH_cr, ~BP3_cr, ~TRS_cr, ~MPB_cr, ~PPB_cr, ~X14Dcr, ~DCB_cr)

phenols1112_qrl<-map(phenols1112_chems, sum_q95, phenols1112_design)
names(phenols1112_qrl) <- paste0("q95_",str_sub(as.character(phenols1112_chems), 2, 7))
phenols1112_qrdf<-bind_rows(phenols1112_qrl, .id="chemical")
phenols1112_95q<- pivot_wider(phenols1112_qrdf, names_from = "chemical", values_from = "q95" )
phenols1112_95q$cycle<- "2011-2012"


phenols1314 <- nhanes_load_data("EPHPP_H", "2013-2014", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, URXBPH, URXBP3, URXTRS, URXMPB, URXPPB, URXBPS, URX14D, URXDCB) %>%
  left_join(creat1314, by="SEQN") %>%
  mutate(BPH_cr=100*URXBPH/URXUCR,
         BP3_cr=100*URXBP3/URXUCR,
         TRS_cr=100*URXTRS/URXUCR,
         MPB_cr=100*URXMPB/URXUCR,
         PPB_cr=100*URXPPB/URXUCR,
         BPS_cr=100*URXBPS/URXUCR,
         X14Dcr=100*URX14D/URXUCR,
         DCB_cr=100*URXDCB/URXUCR)

phenols1314_design <-svydesign(id = ~SDMVPSU,
                               strata = ~SDMVSTRA,
                               weights = ~WTSB2YR,
                               nest = TRUE,
                               data = phenols1314)

phenols1314_chems<-c(~URXBPH, ~URXBP3, ~URXTRS, ~URXMPB, ~URXPPB, ~URXBPS, ~URX14D, ~URXDCB, ~BPH_cr, ~BP3_cr, ~TRS_cr, ~MPB_cr, ~PPB_cr, ~BPS_cr, ~X14Dcr, ~DCB_cr)

phenols1314_qrl<-map(phenols1314_chems, sum_q95, phenols1314_design)
names(phenols1314_qrl) <- paste0("q95_",str_sub(as.character(phenols1314_chems), 2, 7))
phenols1314_qrdf<-bind_rows(phenols1314_qrl, .id="chemical")
phenols1314_95q<- pivot_wider(phenols1314_qrdf, names_from = "chemical", values_from = "q95" )
phenols1314_95q$cycle<- "2013-2014"


phenols1516 <- nhanes_load_data("EPHPP_I", "2015-2016", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, URXBPH, URXBP3, URXTRS, URXMPB, URXPPB, URXBPS, URX14D, URXDCB) %>%
  left_join(creat1516, by="SEQN") %>%
  mutate(BPH_cr=100*URXBPH/URXUCR,
         BP3_cr=100*URXBP3/URXUCR,
         TRS_cr=100*URXTRS/URXUCR,
         MPB_cr=100*URXMPB/URXUCR,
         PPB_cr=100*URXPPB/URXUCR,
         BPS_cr=100*URXBPS/URXUCR,
         X14Dcr=100*URX14D/URXUCR,
         DCB_cr=100*URXDCB/URXUCR)

phenols1516_design <-svydesign(id = ~SDMVPSU,
                               strata = ~SDMVSTRA,
                               weights = ~WTSB2YR,
                               nest = TRUE,
                               data = phenols1516)

phenols1516_chems<-c(~URXBPH, ~URXBP3, ~URXTRS, ~URXMPB, ~URXPPB, ~URXBPS, ~URX14D, ~URXDCB, ~BPH_cr, ~BP3_cr, ~TRS_cr, ~MPB_cr, ~PPB_cr, ~BPS_cr, ~X14Dcr, ~DCB_cr)

phenols1516_qrl<-map(phenols1516_chems, sum_q95, phenols1516_design)
names(phenols1516_qrl) <- paste0("q95_",str_sub(as.character(phenols1516_chems), 2, 7))
phenols1516_qrdf<-bind_rows(phenols1516_qrl, .id="chemical")
phenols1516_95q<- pivot_wider(phenols1516_qrdf, names_from = "chemical", values_from = "q95" )
phenols1516_95q$cycle<- "2015-2016"


# Combine the cycles of data together
phenols_95q <- bind_rows(phenols0304_95q,
                         phenols0506_95q,
                         phenols0708_95q,
                         phenols0910_95q,
                         phenols1112_95q,
                         phenols1314_95q,
                         phenols1516_95q)



# VOCs in urine
urineVOC0506 <- nhanes_load_data("SSUVOC_D", "2005-2006", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSVOC2Y, URX2MH, URX34M, URXAAM, URXATC, URXBMA, URXCYM, URXDHB, URXHP2, URXMAD, URXMB3) %>%
  left_join(creat0506, by="SEQN") %>%
  mutate(X2MHcr=100*URX2MH/URXUCR,
         X34Mcr=100*URX34M/URXUCR,
         AAM_cr=100*URXAAM/URXUCR,
         ATC_cr=100*URXATC/URXUCR,
         BMA_cr=100*URXBMA/URXUCR,
         CYM_cr=100*URXCYM/URXUCR,
         DHB_cr=100*URXDHB/URXUCR,
         HP2_cr=100*URXHP2/URXUCR,
         MAD_cr=100*URXMAD/URXUCR,
         MB3_cr=100*URXMB3/URXUCR)

urineVOC0506_design <-svydesign(id = ~SDMVPSU,
                               strata = ~SDMVSTRA,
                               weights = ~WTSVOC2Y,
                               nest = TRUE,
                               data = urineVOC0506)

urineVOC0506_chems<-c(~URX2MH, ~URX34M, ~URXAAM, ~URXATC, ~URXBMA, ~URXCYM, ~URXDHB, ~URXHP2, ~URXMAD, ~URXMB3, ~X2MHcr, ~X34Mcr, ~AAM_cr, ~ATC_cr, ~BMA_cr, ~CYM_cr, ~DHB_cr, ~HP2_cr, ~MAD_cr, ~MB3_cr)

urineVOC0506_qrl<-map(urineVOC0506_chems, sum_q95, urineVOC0506_design)
names(urineVOC0506_qrl) <- paste0("q95_",str_sub(as.character(urineVOC0506_chems), 2, 7))
urineVOC0506_qrdf<-bind_rows(urineVOC0506_qrl, .id="chemical")
urineVOC0506_95q<- pivot_wider(urineVOC0506_qrdf, names_from = "chemical", values_from = "q95" )
urineVOC0506_95q$cycle<- "2005-2006"


urineVOC1112 <- nhanes_load_data("UVOC_G", "2011-2012", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, URX2MH, URX34M, URXAAM, URXATC, URXBMA, URXCYM, URXDHB, URXHP2, URXMAD, URXMB3)  %>%
  left_join(creat1112, by="SEQN") %>%
  mutate(X2MHcr=100*URX2MH/URXUCR,
         X34Mcr=100*URX34M/URXUCR,
         AAM_cr=100*URXAAM/URXUCR,
         ATC_cr=100*URXATC/URXUCR,
         BMA_cr=100*URXBMA/URXUCR,
         CYM_cr=100*URXCYM/URXUCR,
         DHB_cr=100*URXDHB/URXUCR,
         HP2_cr=100*URXHP2/URXUCR,
         MAD_cr=100*URXMAD/URXUCR,
         MB3_cr=100*URXMB3/URXUCR)

urineVOC1112_design <-svydesign(id = ~SDMVPSU,
                                strata = ~SDMVSTRA,
                                weights = ~WTSA2YR,
                                nest = TRUE,
                                data = urineVOC1112)

urineVOC1112_chems<-c(~URX2MH, ~URX34M, ~URXAAM, ~URXATC, ~URXBMA, ~URXCYM, ~URXDHB, ~URXHP2, ~URXMAD, ~URXMB3, ~X2MHcr, ~X34Mcr, ~AAM_cr, ~ATC_cr, ~BMA_cr, ~CYM_cr, ~DHB_cr, ~HP2_cr, ~MAD_cr, ~MB3_cr)

urineVOC1112_qrl<-map(urineVOC1112_chems, sum_q95, urineVOC1112_design)
names(urineVOC1112_qrl) <- paste0("q95_",str_sub(as.character(urineVOC1112_chems), 2, 7))
urineVOC1112_qrdf<-bind_rows(urineVOC1112_qrl, .id="chemical")
urineVOC1112_95q<- pivot_wider(urineVOC1112_qrdf, names_from = "chemical", values_from = "q95" )
urineVOC1112_95q$cycle<- "2011-2012"


urineVOC1314 <- nhanes_load_data("UVOC_H", "2013-2014", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, URX2MH, URX34M, URXAAM, URXATC, URXBMA, URXCYM, URXDHB, URXHP2, URXMAD, URXMB3)  %>%
  left_join(creat1314, by="SEQN") %>%
  mutate(X2MHcr=100*URX2MH/URXUCR,
         X34Mcr=100*URX34M/URXUCR,
         AAM_cr=100*URXAAM/URXUCR,
         ATC_cr=100*URXATC/URXUCR,
         BMA_cr=100*URXBMA/URXUCR,
         CYM_cr=100*URXCYM/URXUCR,
         DHB_cr=100*URXDHB/URXUCR,
         HP2_cr=100*URXHP2/URXUCR,
         MAD_cr=100*URXMAD/URXUCR,
         MB3_cr=100*URXMB3/URXUCR)

urineVOC1314_design <-svydesign(id = ~SDMVPSU,
                                strata = ~SDMVSTRA,
                                weights = ~WTSA2YR,
                                nest = TRUE,
                                data = urineVOC1314)

urineVOC1314_chems<-c(~URX2MH, ~URX34M, ~URXAAM, ~URXATC, ~URXBMA, ~URXCYM, ~URXDHB, ~URXHP2, ~URXMAD, ~URXMB3, ~X2MHcr, ~X34Mcr, ~AAM_cr, ~ATC_cr, ~BMA_cr, ~CYM_cr, ~DHB_cr, ~HP2_cr, ~MAD_cr, ~MB3_cr)

urineVOC1314_qrl<-map(urineVOC1314_chems, sum_q95, urineVOC1314_design)
names(urineVOC1314_qrl) <- paste0("q95_",str_sub(as.character(urineVOC1314_chems), 2, 7))
urineVOC1314_qrdf<-bind_rows(urineVOC1314_qrl, .id="chemical")
urineVOC1314_95q<- pivot_wider(urineVOC1314_qrdf, names_from = "chemical", values_from = "q95" )
urineVOC1314_95q$cycle<- "2013-2014"


urineVOC1516 <- nhanes_load_data("UVOC_I", "2015-2016", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, URX2MH, URX34M, URXAAM, URXATC, URXBMA, URXCYM, URXDHB, URXHP2, URXMAD, URXMB3)  %>%
  left_join(creat1516, by="SEQN") %>%
  mutate(X2MHcr=100*URX2MH/URXUCR,
         X34Mcr=100*URX34M/URXUCR,
         AAM_cr=100*URXAAM/URXUCR,
         ATC_cr=100*URXATC/URXUCR,
         BMA_cr=100*URXBMA/URXUCR,
         CYM_cr=100*URXCYM/URXUCR,
         DHB_cr=100*URXDHB/URXUCR,
         HP2_cr=100*URXHP2/URXUCR,
         MAD_cr=100*URXMAD/URXUCR,
         MB3_cr=100*URXMB3/URXUCR)

urineVOC1516_design <-svydesign(id = ~SDMVPSU,
                                strata = ~SDMVSTRA,
                                weights = ~WTSA2YR,
                                nest = TRUE,
                                data = urineVOC1516)

urineVOC1516_chems<-c(~URX2MH, ~URX34M, ~URXAAM, ~URXATC, ~URXBMA, ~URXCYM, ~URXDHB, ~URXHP2, ~URXMAD, ~URXMB3, ~X2MHcr, ~X34Mcr, ~AAM_cr, ~ATC_cr, ~BMA_cr, ~CYM_cr, ~DHB_cr, ~HP2_cr, ~MAD_cr, ~MB3_cr)

urineVOC1516_qrl<-map(urineVOC1516_chems, sum_q95, urineVOC1516_design)
names(urineVOC1516_qrl) <- paste0("q95_",str_sub(as.character(urineVOC1516_chems), 2, 7))
urineVOC1516_qrdf<-bind_rows(urineVOC1516_qrl, .id="chemical")
urineVOC1516_95q<- pivot_wider(urineVOC1516_qrdf, names_from = "chemical", values_from = "q95" )
urineVOC1516_95q$cycle<- "2015-2016"



# Combine the cycles of data together
urineVOC_95q <- bind_rows(urineVOC0506_95q,
                          urineVOC1112_95q,
                          urineVOC1314_95q,
                          urineVOC1516_95q)



# Blood VOCs
bloodVOC9900 <- nhanes_load_data("LAB04", "1999-2000", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSVOC2Y, LBXVTO, LBXVCF)

bloodVOC9900_design <-svydesign(id = ~SDMVPSU,
                                strata = ~SDMVSTRA,
                                weights = ~WTSVOC2Y,
                                nest = TRUE,
                                data = bloodVOC9900)

bloodVOC9900_chems<-c(~LBXVTO, ~LBXVCF)

bloodVOC9900_qrl<-map(bloodVOC9900_chems, sum_q95, bloodVOC9900_design)
names(bloodVOC9900_qrl) <- paste0("q95_",str_sub(as.character(bloodVOC9900_chems), 2, 7))
bloodVOC9900_qrdf<-bind_rows(bloodVOC9900_qrl, .id="chemical")
bloodVOC9900_95q<- pivot_wider(bloodVOC9900_qrdf, names_from = "chemical", values_from = "q95" )
bloodVOC9900_95q$cycle<- "1999-2000"


bloodVOC0102 <- nhanes_load_data("L04VOC_B", "2001-2002", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSVOC2Y, LBXVTO, LBXVCF)

bloodVOC0102_design <-svydesign(id = ~SDMVPSU,
                                strata = ~SDMVSTRA,
                                weights = ~WTSVOC2Y,
                                nest = TRUE,
                                data = bloodVOC0102)

bloodVOC0102_chems<-c(~LBXVTO, ~LBXVCF)

bloodVOC0102_qrl<-map(bloodVOC0102_chems, sum_q95, bloodVOC0102_design)
names(bloodVOC0102_qrl) <- paste0("q95_",str_sub(as.character(bloodVOC0102_chems), 2, 7))
bloodVOC0102_qrdf<-bind_rows(bloodVOC0102_qrl, .id="chemical")
bloodVOC0102_95q<- pivot_wider(bloodVOC0102_qrdf, names_from = "chemical", values_from = "q95" )
bloodVOC0102_95q$cycle<- "2001-2002"


bloodVOC0304 <- nhanes_load_data("L04VOC_C", "2003-2004", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSVOC2Y, LBXVTO, LBXVCF)

bloodVOC0304_design <-svydesign(id = ~SDMVPSU,
                                strata = ~SDMVSTRA,
                                weights = ~WTSVOC2Y,
                                nest = TRUE,
                                data = bloodVOC0304)

bloodVOC0304_chems<-c(~LBXVTO, ~LBXVCF)

bloodVOC0304_qrl<-map(bloodVOC0304_chems, sum_q95, bloodVOC0304_design)
names(bloodVOC0304_qrl) <- paste0("q95_",str_sub(as.character(bloodVOC0304_chems), 2, 7))
bloodVOC0304_qrdf<-bind_rows(bloodVOC0304_qrl, .id="chemical")
bloodVOC0304_95q<- pivot_wider(bloodVOC0304_qrdf, names_from = "chemical", values_from = "q95" )
bloodVOC0304_95q$cycle<- "2003-2004"


bloodVOC0506 <- nhanes_load_data("VOCWB_D", "2005-2006", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSVOC2Y, LBXVTO, LBXVCF)

bloodVOC0506_design <-svydesign(id = ~SDMVPSU,
                                strata = ~SDMVSTRA,
                                weights = ~WTSVOC2Y,
                                nest = TRUE,
                                data = bloodVOC0506)

bloodVOC0506_chems<-c(~LBXVTO, ~LBXVCF)

bloodVOC0506_qrl<-map(bloodVOC0506_chems, sum_q95, bloodVOC0506_design)
names(bloodVOC0506_qrl) <- paste0("q95_",str_sub(as.character(bloodVOC0506_chems), 2, 7))
bloodVOC0506_qrdf<-bind_rows(bloodVOC0506_qrl, .id="chemical")
bloodVOC0506_95q<- pivot_wider(bloodVOC0506_qrdf, names_from = "chemical", values_from = "q95" )
bloodVOC0506_95q$cycle<- "2005-2006"

# In 2007-2008 through 2011-2012 cycles, chloroform is in separate trihalomethanes file; same SEQNs
bloodVOC0708 <- nhanes_load_data("VOCWB_E", "2007-2008", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSVOC2Y, LBXVTO) %>%
  # there are 41 observations that are missing WTSVOC2Y; none of them have LBXVCF data; only 7 have LBXVTO data
  # I have to delete them becuase they don't have weights
  filter(is.na(WTSVOC2Y)==F)

bloodVOC0708_c <- nhanes_load_data("VOCMWB_E", "2007-2008", demographics = FALSE) %>%
  select(SEQN, LBXVCF)

bloodVOC0708 <- bloodVOC0708 %>%
  left_join(bloodVOC0708_c, by = "SEQN")

bloodVOC0708_design <-svydesign(id = ~SDMVPSU,
                                strata = ~SDMVSTRA,
                                weights = ~WTSVOC2Y,
                                nest = TRUE,
                                data = bloodVOC0708)

bloodVOC0708_chems<-c(~LBXVTO, ~LBXVCF)

bloodVOC0708_qrl<-map(bloodVOC0708_chems, sum_q95, bloodVOC0708_design)
names(bloodVOC0708_qrl) <- paste0("q95_",str_sub(as.character(bloodVOC0708_chems), 2, 7))
bloodVOC0708_qrdf<-bind_rows(bloodVOC0708_qrl, .id="chemical")
bloodVOC0708_95q<- pivot_wider(bloodVOC0708_qrdf, names_from = "chemical", values_from = "q95" )
bloodVOC0708_95q$cycle<- "2007-2008"


bloodVOC0910 <- nhanes_load_data("VOCWB_F", "2009-2010", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSVOC2Y, LBXVTO)

bloodVOC0910_c <- nhanes_load_data("VOCMWB_F", "2009-2010", demographics = FALSE) %>%
  select(SEQN, LBXVCF)

bloodVOC0910 <- bloodVOC0910 %>%
  left_join(bloodVOC0910_c, by = "SEQN")

bloodVOC0910_design <-svydesign(id = ~SDMVPSU,
                                strata = ~SDMVSTRA,
                                weights = ~WTSVOC2Y,
                                nest = TRUE,
                                data = bloodVOC0910)

bloodVOC0910_chems<-c(~LBXVTO, ~LBXVCF)

bloodVOC0910_qrl<-map(bloodVOC0910_chems, sum_q95, bloodVOC0910_design)
names(bloodVOC0910_qrl) <- paste0("q95_",str_sub(as.character(bloodVOC0910_chems), 2, 7))
bloodVOC0910_qrdf<-bind_rows(bloodVOC0910_qrl, .id="chemical")
bloodVOC0910_95q<- pivot_wider(bloodVOC0910_qrdf, names_from = "chemical", values_from = "q95" )
bloodVOC0910_95q$cycle<- "2009-2010"


# no toluene data in 2011-2012
bloodVOC1112 <- nhanes_load_data("VOCMWB_g", "2011-2012", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSVOC2Y, LBXVCF)

bloodVOC1112_design <-svydesign(id = ~SDMVPSU,
                                strata = ~SDMVSTRA,
                                weights = ~WTSVOC2Y,
                                nest = TRUE,
                                data = bloodVOC1112)

bloodVOC1112_chems<-c(~LBXVCF)

bloodVOC1112_qrl<-map(bloodVOC1112_chems, sum_q95, bloodVOC1112_design)
names(bloodVOC1112_qrl) <- paste0("q95_",str_sub(as.character(bloodVOC1112_chems), 2, 7))
bloodVOC1112_qrdf<-bind_rows(bloodVOC1112_qrl, .id="chemical")
bloodVOC1112_95q<- pivot_wider(bloodVOC1112_qrdf, names_from = "chemical", values_from = "q95" )
bloodVOC1112_95q$cycle<- "2011-2012"

#NOTE: for 2013-2014 and 2015-2016, chloroform data is in ng/mL, but in earlier cycles it is in picograms/mL;
# going to convert 2013-2014 and 2015-2016 data so that it's all in picograms/mL so that we match Exposure Report
# Need to do this also when we create the public data file
bloodVOC1314 <- nhanes_load_data("VOCWB_H", "2013-2014", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSVOC2Y, LBXVTO, LBXVCF) %>%
  mutate(LBXVCF=LBXVCF*1000) #multiply by 1000 to get from nanograms to picograms

bloodVOC1314_design <-svydesign(id = ~SDMVPSU,
                                strata = ~SDMVSTRA,
                                weights = ~WTSVOC2Y,
                                nest = TRUE,
                                data = bloodVOC1314)

bloodVOC1314_chems<-c(~LBXVTO, ~LBXVCF)

bloodVOC1314_qrl<-map(bloodVOC1314_chems, sum_q95, bloodVOC1314_design)
names(bloodVOC1314_qrl) <- paste0("q95_",str_sub(as.character(bloodVOC1314_chems), 2, 7))
bloodVOC1314_qrdf<-bind_rows(bloodVOC1314_qrl, .id="chemical")
bloodVOC1314_95q<- pivot_wider(bloodVOC1314_qrdf, names_from = "chemical", values_from = "q95" )
bloodVOC1314_95q$cycle<- "2013-2014"


bloodVOC1516 <- nhanes_load_data("VOCWB_I", "2015-2016", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSVOC2Y, LBXVTO, LBXVCF) %>%
  mutate(LBXVCF=LBXVCF*1000)

bloodVOC1516_design <-svydesign(id = ~SDMVPSU,
                                strata = ~SDMVSTRA,
                                weights = ~WTSVOC2Y,
                                nest = TRUE,
                                data = bloodVOC1516)

bloodVOC1516_chems<-c(~LBXVTO, ~LBXVCF)

bloodVOC1516_qrl<-map(bloodVOC1516_chems, sum_q95, bloodVOC1516_design)
names(bloodVOC1516_qrl) <- paste0("q95_",str_sub(as.character(bloodVOC1516_chems), 2, 7))
bloodVOC1516_qrdf<-bind_rows(bloodVOC1516_qrl, .id="chemical")
bloodVOC1516_95q<- pivot_wider(bloodVOC1516_qrdf, names_from = "chemical", values_from = "q95" )
bloodVOC1516_95q$cycle<- "2015-2016"


# Combine the cycles of data together
bloodVOC_95q <- bind_rows(bloodVOC9900_95q,
                          bloodVOC0102_95q,
                          bloodVOC0304_95q,
                          bloodVOC0506_95q,
                          bloodVOC0708_95q,
                          bloodVOC0910_95q,
                          bloodVOC1112_95q,
                          bloodVOC1314_95q,
                          bloodVOC1516_95q)



#PAHs
PAH0102 <- nhanes_load_data("PHPYPA_B", "2001-2002", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSPH2YR, URXP01, URXP02, URXP03, URXP04, URXP05, URXP06, URXP07, URXP10)%>%
  left_join(creat0102, by="SEQN") %>%
  mutate(URXP01=URXP01/1000, # convert P01 and P02 from ng/L to micrograms/L to match Exposure Report, the rest are reported in ng/L so don't need to convert
         URXP02=URXP02/1000) %>%
  mutate(P01_cr=100*URXP01/URXUCR,
         P02_cr=100*URXP02/URXUCR,
         P03_cr=100*URXP03/URXUCR,
         P04_cr=100*URXP04/URXUCR,
         P05_cr=100*URXP05/URXUCR,
         P06_cr=100*URXP06/URXUCR,
         P07_cr=100*URXP07/URXUCR,
         P10_cr=100*URXP10/URXUCR)

PAH0102_design <-svydesign(id = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           weights = ~WTSPH2YR,
                           nest = TRUE,
                           data = PAH0102)

PAH0102_chems<-c(~URXP01, ~URXP02, ~URXP03, ~URXP04, ~URXP05, ~URXP06, ~URXP07, ~URXP10, ~P01_cr, ~P02_cr, ~P03_cr, ~P04_cr, ~P05_cr, ~P06_cr, ~P07_cr, ~P10_cr)

PAH0102_qrl<-map(PAH0102_chems, sum_q95, PAH0102_design)
names(PAH0102_qrl) <- paste0("q95_",str_sub(as.character(PAH0102_chems), 2, 7))
PAH0102_qrdf<-bind_rows(PAH0102_qrl, .id="chemical")
PAH0102_95q<- pivot_wider(PAH0102_qrdf, names_from = "chemical", values_from = "q95" )
PAH0102_95q$cycle<- "2001-2002"



PAH0304 <- nhanes_load_data("L31PAH_C", "2003-2004", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, URXP01, URXP02, URXP03, URXP04, URXP05, URXP06, URXP07, URXP10, URXP17, URXP19) %>%
  left_join(creat0304, by="SEQN") %>%
  mutate(URXP01=URXP01/1000,
         URXP02=URXP02/1000) %>%
  mutate(P01_cr=100*URXP01/URXUCR,
         P02_cr=100*URXP02/URXUCR,
         P03_cr=100*URXP03/URXUCR,
         P04_cr=100*URXP04/URXUCR,
         P05_cr=100*URXP05/URXUCR,
         P06_cr=100*URXP06/URXUCR,
         P07_cr=100*URXP07/URXUCR,
         P10_cr=100*URXP10/URXUCR,
         P17_cr=100*URXP17/URXUCR,
         P19_cr=100*URXP19/URXUCR)

PAH0304_design <-svydesign(id = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           weights = ~WTSB2YR,
                           nest = TRUE,
                           data = PAH0304)

PAH0304_chems<-c(~URXP01, ~URXP02, ~URXP03, ~URXP04, ~URXP05, ~URXP06, ~URXP07, ~URXP10, ~URXP17, ~URXP19, ~P01_cr, ~P02_cr, ~P03_cr, ~P04_cr, ~P05_cr, ~P06_cr, ~P07_cr, ~P10_cr, ~P17_cr, ~P19_cr)

PAH0304_qrl<-map(PAH0304_chems, sum_q95, PAH0304_design)
names(PAH0304_qrl) <- paste0("q95_",str_sub(as.character(PAH0304_chems), 2, 7))
PAH0304_qrdf<-bind_rows(PAH0304_qrl, .id="chemical")
PAH0304_95q<- pivot_wider(PAH0304_qrdf, names_from = "chemical", values_from = "q95" )
PAH0304_95q$cycle<- "2003-2004"


PAH0506 <- nhanes_load_data("PAH_D", "2005-2006", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, URXP01, URXP02, URXP03, URXP04, URXP05, URXP06, URXP07, URXP10, URXP17, URXP19) %>%
  left_join(creat0506, by="SEQN") %>%
  mutate(URXP01=URXP01/1000,
         URXP02=URXP02/1000) %>%
  mutate(P01_cr=100*URXP01/URXUCR,
         P02_cr=100*URXP02/URXUCR,
         P03_cr=100*URXP03/URXUCR,
         P04_cr=100*URXP04/URXUCR,
         P05_cr=100*URXP05/URXUCR,
         P06_cr=100*URXP06/URXUCR,
         P07_cr=100*URXP07/URXUCR,
         P10_cr=100*URXP10/URXUCR,
         P17_cr=100*URXP17/URXUCR,
         P19_cr=100*URXP19/URXUCR)

PAH0506_design <-svydesign(id = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           weights = ~WTSB2YR,
                           nest = TRUE,
                           data = PAH0506)

PAH0506_chems<-c(~URXP01, ~URXP02, ~URXP03, ~URXP04, ~URXP05, ~URXP06, ~URXP07, ~URXP10, ~URXP17, ~URXP19, ~P01_cr, ~P02_cr, ~P03_cr, ~P04_cr, ~P05_cr, ~P06_cr, ~P07_cr, ~P10_cr, ~P17_cr, ~P19_cr)

PAH0506_qrl<-map(PAH0506_chems, sum_q95, PAH0506_design)
names(PAH0506_qrl) <- paste0("q95_",str_sub(as.character(PAH0506_chems), 2, 7))
PAH0506_qrdf<-bind_rows(PAH0506_qrl, .id="chemical")
PAH0506_95q<- pivot_wider(PAH0506_qrdf, names_from = "chemical", values_from = "q95" )
PAH0506_95q$cycle<- "2005-2006"


PAH0708 <- nhanes_load_data("PAH_E", "2007-2008", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, URXP01, URXP02, URXP03, URXP04, URXP05, URXP06, URXP07, URXP10, URXP17)  %>%
  left_join(creat0708, by="SEQN") %>%
  mutate(URXP01=URXP01/1000,
         URXP02=URXP02/1000) %>%
  mutate(P01_cr=100*URXP01/URXUCR,
         P02_cr=100*URXP02/URXUCR,
         P03_cr=100*URXP03/URXUCR,
         P04_cr=100*URXP04/URXUCR,
         P05_cr=100*URXP05/URXUCR,
         P06_cr=100*URXP06/URXUCR,
         P07_cr=100*URXP07/URXUCR,
         P10_cr=100*URXP10/URXUCR,
         P17_cr=100*URXP17/URXUCR)

PAH0708_design <-svydesign(id = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           weights = ~WTSB2YR,
                           nest = TRUE,
                           data = PAH0708)

PAH0708_chems<-c(~URXP01, ~URXP02, ~URXP03, ~URXP04, ~URXP05, ~URXP06, ~URXP07, ~URXP10, ~URXP17, ~P01_cr, ~P02_cr, ~P03_cr, ~P04_cr, ~P05_cr, ~P06_cr, ~P07_cr, ~P10_cr, ~P17_cr)

PAH0708_qrl<-map(PAH0708_chems, sum_q95, PAH0708_design)
names(PAH0708_qrl) <- paste0("q95_",str_sub(as.character(PAH0708_chems), 2, 7))
PAH0708_qrdf<-bind_rows(PAH0708_qrl, .id="chemical")
PAH0708_95q<- pivot_wider(PAH0708_qrdf, names_from = "chemical", values_from = "q95" )
PAH0708_95q$cycle<- "2007-2008"

PAH0910 <- nhanes_load_data("PAH_F", "2009-2010", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, URXP01, URXP02, URXP03, URXP04, URXP05, URXP06, URXP07, URXP10, URXP17)  %>%
  left_join(creat0910, by="SEQN") %>%
  mutate(URXP01=URXP01/1000,
         URXP02=URXP02/1000) %>%
  mutate(P01_cr=100*URXP01/URXUCR,
         P02_cr=100*URXP02/URXUCR,
         P03_cr=100*URXP03/URXUCR,
         P04_cr=100*URXP04/URXUCR,
         P05_cr=100*URXP05/URXUCR,
         P06_cr=100*URXP06/URXUCR,
         P07_cr=100*URXP07/URXUCR,
         P10_cr=100*URXP10/URXUCR,
         P17_cr=100*URXP17/URXUCR)

PAH0910_design <-svydesign(id = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           weights = ~WTSB2YR,
                           nest = TRUE,
                           data = PAH0910)

PAH0910_chems<-c(~URXP01, ~URXP02, ~URXP03, ~URXP04, ~URXP05, ~URXP06, ~URXP07, ~URXP10, ~URXP17, ~P01_cr, ~P02_cr, ~P03_cr, ~P04_cr, ~P05_cr, ~P06_cr, ~P07_cr, ~P10_cr, ~P17_cr)

PAH0910_qrl<-map(PAH0910_chems, sum_q95, PAH0910_design)
names(PAH0910_qrl) <- paste0("q95_",str_sub(as.character(PAH0910_chems), 2, 7))
PAH0910_qrdf<-bind_rows(PAH0910_qrl, .id="chemical")
PAH0910_95q<- pivot_wider(PAH0910_qrdf, names_from = "chemical", values_from = "q95" )
PAH0910_95q$cycle<- "2009-2010"


PAH1112 <- nhanes_load_data("PAH_G", "2011-2012", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, URXP01, URXP02, URXP03, URXP04, URXP05, URXP06, URXP07, URXP10, URXP17, URXP19)  %>%
  left_join(creat1112, by="SEQN") %>%
  mutate(URXP01=URXP01/1000,
         URXP02=URXP02/1000) %>%
  mutate(P01_cr=100*URXP01/URXUCR,
         P02_cr=100*URXP02/URXUCR,
         P03_cr=100*URXP03/URXUCR,
         P04_cr=100*URXP04/URXUCR,
         P05_cr=100*URXP05/URXUCR,
         P06_cr=100*URXP06/URXUCR,
         P07_cr=100*URXP07/URXUCR,
         P10_cr=100*URXP10/URXUCR,
         P17_cr=100*URXP17/URXUCR,
         P19_cr=100*URXP19/URXUCR) %>%
  filter(is.na(WTSA2YR)==F) #delete the 43 observations with missign WTSA2YR

PAH1112_design <-svydesign(id = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           weights = ~WTSA2YR,
                           nest = TRUE,
                           data = PAH1112)

PAH1112_chems<-c(~URXP01, ~URXP02, ~URXP03, ~URXP04, ~URXP05, ~URXP06, ~URXP07, ~URXP10, ~URXP17, ~URXP19, ~P01_cr, ~P02_cr, ~P03_cr, ~P04_cr, ~P05_cr, ~P06_cr, ~P07_cr, ~P10_cr, ~P17_cr, ~P19_cr)

PAH1112_qrl<-map(PAH1112_chems, sum_q95, PAH1112_design)
names(PAH1112_qrl) <- paste0("q95_",str_sub(as.character(PAH1112_chems), 2, 7))
PAH1112_qrdf<-bind_rows(PAH1112_qrl, .id="chemical")
PAH1112_95q<- pivot_wider(PAH1112_qrdf, names_from = "chemical", values_from = "q95" )
PAH1112_95q$cycle<- "2011-2012"

PAH1314 <- nhanes_load_data("PAH_H", "2013-2014", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, URXP01, URXP02, URXP03, URXP04, URXP06, URXP10)  %>%
  left_join(creat1314, by="SEQN") %>%
  mutate(URXP01=URXP01/1000,
         URXP02=URXP02/1000) %>%
  mutate(P01_cr=100*URXP01/URXUCR,
         P02_cr=100*URXP02/URXUCR,
         P03_cr=100*URXP03/URXUCR,
         P04_cr=100*URXP04/URXUCR,
         P06_cr=100*URXP06/URXUCR,
         P10_cr=100*URXP10/URXUCR)

PAH1314_design <-svydesign(id = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           weights = ~WTSA2YR,
                           nest = TRUE,
                           data = PAH1314)

PAH1314_chems<-c(~URXP01, ~URXP02, ~URXP03, ~URXP04, ~URXP06, ~URXP10, ~P01_cr, ~P02_cr, ~P03_cr, ~P04_cr, ~P06_cr, ~P10_cr)

PAH1314_qrl<-map(PAH1314_chems, sum_q95, PAH1314_design)
names(PAH1314_qrl) <- paste0("q95_",str_sub(as.character(PAH1314_chems), 2, 7))
PAH1314_qrdf<-bind_rows(PAH1314_qrl, .id="chemical")
PAH1314_95q<- pivot_wider(PAH1314_qrdf, names_from = "chemical", values_from = "q95" )
PAH1314_95q$cycle<- "2013-2014"

# Combine the cycles of data together
PAH_95q <- bind_rows(PAH0102_95q,
                     PAH0304_95q,
                     PAH0506_95q,
                     PAH0708_95q,
                     PAH0910_95q,
                     PAH1112_95q,
                     PAH1314_95q)


# PFAS
pfas0304 <- nhanes_load_data("L24PFC_C", "2003-2004", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, LBXMPAH, LBXPFDE, LBXPFHS, LBXPFNA, LBXPFOA, LBXPFOS)

pfas0304_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTSA2YR,
                            nest = TRUE,
                            data = pfas0304)

pfas0304_chems<-c(~LBXMPAH, ~LBXPFDE, ~LBXPFHS, ~LBXPFNA, ~LBXPFOA, ~LBXPFOS)

pfas0304_qrl<-map(pfas0304_chems, sum_q95, pfas0304_design)
names(pfas0304_qrl) <- paste0("q95_",str_sub(as.character(pfas0304_chems), 2, 8))
pfas0304_qrdf<-bind_rows(pfas0304_qrl, .id="chemical")
pfas0304_95q<- pivot_wider(pfas0304_qrdf, names_from = "chemical", values_from = "q95" )
pfas0304_95q$cycle<- "2003-2004"


pfas0506 <- nhanes_load_data("PFC_D", "2005-2006", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, LBXMPAH, LBXPFDE, LBXPFHS, LBXPFNA, LBXPFOA, LBXPFOS)

pfas0506_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTSA2YR,
                            nest = TRUE,
                            data = pfas0506)

pfas0506_chems<-c(~LBXMPAH, ~LBXPFDE, ~LBXPFHS, ~LBXPFNA, ~LBXPFOA, ~LBXPFOS)

pfas0506_qrl<-map(pfas0506_chems, sum_q95, pfas0506_design)
names(pfas0506_qrl) <- paste0("q95_",str_sub(as.character(pfas0506_chems), 2, 8))
pfas0506_qrdf<-bind_rows(pfas0506_qrl, .id="chemical")
pfas0506_95q<- pivot_wider(pfas0506_qrdf, names_from = "chemical", values_from = "q95" )
pfas0506_95q$cycle<- "2005-2006"


pfas0708 <- nhanes_load_data("PFC_E", "2007-2008", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSC2YR, LBXMPAH, LBXPFDE, LBXPFHS, LBXPFNA, LBXPFOA, LBXPFOS)

pfas0708_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTSC2YR,
                            nest = TRUE,
                            data = pfas0708)

pfas0708_chems<-c(~LBXMPAH, ~LBXPFDE, ~LBXPFHS, ~LBXPFNA, ~LBXPFOA, ~LBXPFOS)

pfas0708_qrl<-map(pfas0708_chems, sum_q95, pfas0708_design)
names(pfas0708_qrl) <- paste0("q95_",str_sub(as.character(pfas0708_chems), 2, 8))
pfas0708_qrdf<-bind_rows(pfas0708_qrl, .id="chemical")
pfas0708_95q<- pivot_wider(pfas0708_qrdf, names_from = "chemical", values_from = "q95" )
pfas0708_95q$cycle<- "2007-2008"


pfas0910 <- nhanes_load_data("PFC_F", "2009-2010", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSC2YR, LBXMPAH, LBXPFDE, LBXPFHS, LBXPFNA, LBXPFOA, LBXPFOS)

pfas0910_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTSC2YR,
                            nest = TRUE,
                            data = pfas0910)

pfas0910_chems<-c(~LBXMPAH, ~LBXPFDE, ~LBXPFHS, ~LBXPFNA, ~LBXPFOA, ~LBXPFOS)

pfas0910_qrl<-map(pfas0910_chems, sum_q95, pfas0910_design)
names(pfas0910_qrl) <- paste0("q95_",str_sub(as.character(pfas0910_chems), 2, 8))
pfas0910_qrdf<-bind_rows(pfas0910_qrl, .id="chemical")
pfas0910_95q<- pivot_wider(pfas0910_qrdf, names_from = "chemical", values_from = "q95" )
pfas0910_95q$cycle<- "2009-2010"

pfas1112 <- nhanes_load_data("PFC_G", "2011-2012", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, LBXMPAH, LBXPFDE, LBXPFHS, LBXPFNA, LBXPFOA, LBXPFOS) %>%
  filter(is.na(WTSA2YR)==F) #delete the 33 observations with missing weights and pfas data

pfas1112_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTSA2YR,
                            nest = TRUE,
                            data = pfas1112)

pfas1112_chems<-c(~LBXMPAH, ~LBXPFDE, ~LBXPFHS, ~LBXPFNA, ~LBXPFOA, ~LBXPFOS)

pfas1112_qrl<-map(pfas1112_chems, sum_q95, pfas1112_design)
names(pfas1112_qrl) <- paste0("q95_",str_sub(as.character(pfas1112_chems), 2, 8))
pfas1112_qrdf<-bind_rows(pfas1112_qrl, .id="chemical")
pfas1112_95q<- pivot_wider(pfas1112_qrdf, names_from = "chemical", values_from = "q95" )
pfas1112_95q$cycle<- "2011-2012"

pfas1314 <- nhanes_load_data("PFAS_H", "2013-2014", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, LBXMPAH, LBXPFDE, LBXPFHS, LBXPFNA)
# no LBXPFOA, LBXPFOS - to get these, have to sum the isomers from the supplemental

pfas1314s <- nhanes_load_data("SSPFAS_H", "2013-2014", demographics = FALSE) %>%
  select(SEQN, SSNPFOA, SSBPFOA, SSNPFOS, SSMPFOS )

pfas1314 <- pfas1314 %>%
  left_join(pfas1314s, by="SEQN") %>%
  mutate(LBXPFOA=SSNPFOA+SSBPFOA,
         LBXPFOS=SSNPFOS+SSMPFOS) %>%
  select(-SSNPFOA, -SSBPFOA, -SSNPFOS, -SSMPFOS)

pfas1314_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTSB2YR,
                            nest = TRUE,
                            data = pfas1314)

pfas1314_chems<-c(~LBXMPAH, ~LBXPFDE, ~LBXPFHS, ~LBXPFNA, ~LBXPFOA, ~LBXPFOS)

pfas1314_qrl<-map(pfas1314_chems, sum_q95, pfas1314_design)
names(pfas1314_qrl) <- paste0("q95_",str_sub(as.character(pfas1314_chems), 2, 8))
pfas1314_qrdf<-bind_rows(pfas1314_qrl, .id="chemical")
pfas1314_95q<- pivot_wider(pfas1314_qrdf, names_from = "chemical", values_from = "q95" )
pfas1314_95q$cycle<- "2013-2014"


pfas1516 <- nhanes_load_data("PFAS_I", "2015-2016", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSB2YR, LBXMPAH, LBXPFDE, LBXPFHS, LBXPFNA, LBXNFOA, LBXBFOA, LBXNFOS, LBXMFOS) %>%
  mutate(LBXPFOA=LBXNFOA+LBXBFOA,
         LBXPFOS=LBXNFOS+LBXMFOS) %>%
  select(-LBXNFOA, -LBXBFOA, -LBXNFOS, -LBXMFOS)

pfas1516_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTSB2YR,
                            nest = TRUE,
                            data = pfas1516)

pfas1516_chems<-c(~LBXMPAH, ~LBXPFDE, ~LBXPFHS, ~LBXPFNA, ~LBXPFOA, ~LBXPFOS)

pfas1516_qrl<-map(pfas1516_chems, sum_q95, pfas1516_design)
names(pfas1516_qrl) <- paste0("q95_",str_sub(as.character(pfas1516_chems), 2, 8))
pfas1516_qrdf<-bind_rows(pfas1516_qrl, .id="chemical")
pfas1516_95q<- pivot_wider(pfas1516_qrdf, names_from = "chemical", values_from = "q95" )
pfas1516_95q$cycle<- "2015-2016"


pfas_95q <- bind_rows(pfas0304_95q,
                      pfas0506_95q,
                      pfas0708_95q,
                      pfas0910_95q,
                      pfas1112_95q,
                      pfas1314_95q,
                      pfas1516_95q)


# Metals
# Lead and mercury (in blood); note that 1999-2012 uses MEC weights
lead9900 <- nhanes_load_data("LAB06", "1999-2000", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTMEC2YR, LBXBPB, LBXTHG)

lead9900_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTMEC2YR,
                            nest = TRUE,
                            data = lead9900)

lead9900_chems<-c(~LBXBPB, ~LBXTHG)

lead9900_qrl<-map(lead9900_chems, sum_q95, lead9900_design)
names(lead9900_qrl) <- paste0("q95_",str_sub(as.character(lead9900_chems), 2, 7))
lead9900_qrdf<-bind_rows(lead9900_qrl, .id="chemical")
lead9900_95q<- pivot_wider(lead9900_qrdf, names_from = "chemical", values_from = "q95" )
lead9900_95q$cycle<- "1999-2000"


lead0102 <- nhanes_load_data("L06_B", "2001-2002", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTMEC2YR, LBXBPB, LBXTHG)

lead0102_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTMEC2YR,
                            nest = TRUE,
                            data = lead0102)

lead0102_chems<-c(~LBXBPB, ~LBXTHG)

lead0102_qrl<-map(lead0102_chems, sum_q95, lead0102_design)
names(lead0102_qrl) <- paste0("q95_",str_sub(as.character(lead0102_chems), 2, 7))
lead0102_qrdf<-bind_rows(lead0102_qrl, .id="chemical")
lead0102_95q<- pivot_wider(lead0102_qrdf, names_from = "chemical", values_from = "q95" )
lead0102_95q$cycle<- "2001-2002"


lead0304 <- nhanes_load_data("L06BMT_C", "2003-2004", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTMEC2YR, LBXBPB, LBXTHG)

lead0304_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTMEC2YR,
                            nest = TRUE,
                            data = lead0304)

lead0304_chems<-c(~LBXBPB, ~LBXTHG)

lead0304_qrl<-map(lead0304_chems, sum_q95, lead0304_design)
names(lead0304_qrl) <- paste0("q95_",str_sub(as.character(lead0304_chems), 2, 7))
lead0304_qrdf<-bind_rows(lead0304_qrl, .id="chemical")
lead0304_95q<- pivot_wider(lead0304_qrdf, names_from = "chemical", values_from = "q95" )
lead0304_95q$cycle<- "2003-2004"


lead0506 <- nhanes_load_data("PBCD_D", "2005-2006", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTMEC2YR, LBXBPB, LBXTHG)

lead0506_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTMEC2YR,
                            nest = TRUE,
                            data = lead0506)

lead0506_chems<-c(~LBXBPB, ~LBXTHG)

lead0506_qrl<-map(lead0506_chems, sum_q95, lead0506_design)
names(lead0506_qrl) <- paste0("q95_",str_sub(as.character(lead0506_chems), 2, 7))
lead0506_qrdf<-bind_rows(lead0506_qrl, .id="chemical")
lead0506_95q<- pivot_wider(lead0506_qrdf, names_from = "chemical", values_from = "q95" )
lead0506_95q$cycle<- "2005-2006"


lead0708 <- nhanes_load_data("PBCD_E", "2007-2008", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTMEC2YR, LBXBPB, LBXTHG)

lead0708_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTMEC2YR,
                            nest = TRUE,
                            data = lead0708)

lead0708_chems<-c(~LBXBPB, ~LBXTHG)

lead0708_qrl<-map(lead0708_chems, sum_q95, lead0708_design)
names(lead0708_qrl) <- paste0("q95_",str_sub(as.character(lead0708_chems), 2, 7))
lead0708_qrdf<-bind_rows(lead0708_qrl, .id="chemical")
lead0708_95q<- pivot_wider(lead0708_qrdf, names_from = "chemical", values_from = "q95" )
lead0708_95q$cycle<- "2007-2008"


lead0910 <- nhanes_load_data("PBCD_F", "2009-2010", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTMEC2YR, LBXBPB, LBXTHG)

lead0910_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTMEC2YR,
                            nest = TRUE,
                            data = lead0910)

lead0910_chems<-c(~LBXBPB, ~LBXTHG)

lead0910_qrl<-map(lead0910_chems, sum_q95, lead0910_design)
names(lead0910_qrl) <- paste0("q95_",str_sub(as.character(lead0910_chems), 2, 7))
lead0910_qrdf<-bind_rows(lead0910_qrl, .id="chemical")
lead0910_95q<- pivot_wider(lead0910_qrdf, names_from = "chemical", values_from = "q95" )
lead0910_95q$cycle<- "2009-2010"

lead1112 <- nhanes_load_data("PBCD_G", "2011-2012", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTMEC2YR, LBXBPB, LBXTHG)

lead1112_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTMEC2YR,
                            nest = TRUE,
                            data = lead1112)

lead1112_chems<-c(~LBXBPB, ~LBXTHG)

lead1112_qrl<-map(lead1112_chems, sum_q95, lead1112_design)
names(lead1112_qrl) <- paste0("q95_",str_sub(as.character(lead1112_chems), 2, 7))
lead1112_qrdf<-bind_rows(lead1112_qrl, .id="chemical")
lead1112_95q<- pivot_wider(lead1112_qrdf, names_from = "chemical", values_from = "q95" )
lead1112_95q$cycle<- "2011-2012"


lead1314 <- nhanes_load_data("PBCD_H", "2013-2014", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSH2YR, LBXBPB, LBXTHG)

lead1314_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTSH2YR,
                            nest = TRUE,
                            data = lead1314)

lead1314_chems<-c(~LBXBPB, ~LBXTHG)

lead1314_qrl<-map(lead1314_chems, sum_q95, lead1314_design)
names(lead1314_qrl) <- paste0("q95_",str_sub(as.character(lead1314_chems), 2, 7))
lead1314_qrdf<-bind_rows(lead1314_qrl, .id="chemical")
lead1314_95q<- pivot_wider(lead1314_qrdf, names_from = "chemical", values_from = "q95" )
lead1314_95q$cycle<- "2013-2014"


lead1516 <- nhanes_load_data("PBCD_I", "2015-2016", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSH2YR, LBXBPB, LBXTHG)

lead1516_design <-svydesign(id = ~SDMVPSU,
                            strata = ~SDMVSTRA,
                            weights = ~WTSH2YR,
                            nest = TRUE,
                            data = lead1516)

lead1516_chems<-c(~LBXBPB, ~LBXTHG)

lead1516_qrl<-map(lead1516_chems, sum_q95, lead1516_design)
names(lead1516_qrl) <- paste0("q95_",str_sub(as.character(lead1516_chems), 2, 7))
lead1516_qrdf<-bind_rows(lead1516_qrl, .id="chemical")
lead1516_95q<- pivot_wider(lead1516_qrdf, names_from = "chemical", values_from = "q95" )
lead1516_95q$cycle<- "2015-2016"

lead_mercury_95q <- bind_rows(lead9900_95q,
                              lead0102_95q,
                              lead0304_95q,
                              lead0506_95q,
                              lead0708_95q,
                              lead0910_95q,
                              lead1112_95q,
                              lead1314_95q,
                              lead1516_95q)


#Cadmium
cad9900 <- nhanes_load_data("LAB06HM", "1999-2000", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSHM2YR, URDUCD) %>%
  rename(URXUCD=URDUCD) %>% #correct variable name
  left_join(creat9900, by="SEQN") %>%
  mutate(UCD_cr=100*URXUCD/URXUCR)

cad9900_design <-svydesign(id = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           weights = ~WTSHM2YR,
                           nest = TRUE,
                           data = cad9900)

cad9900_chems<-c(~URXUCD, ~UCD_cr)

cad9900_qrl<-map(cad9900_chems, sum_q95, cad9900_design)
names(cad9900_qrl) <- paste0("q95_",str_sub(as.character(cad9900_chems), 2, 7))
cad9900_qrdf<-bind_rows(cad9900_qrl, .id="chemical")
cad9900_95q<- pivot_wider(cad9900_qrdf, names_from = "chemical", values_from = "q95" )
cad9900_95q$cycle<- "1999-2000"


cad0102 <- nhanes_load_data("L06HM_B", "2001-2002", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSHM2YR, URDUCD) %>%
  rename(URXUCD=URDUCD) %>% #correct variable name
  left_join(creat0102, by="SEQN") %>%
  mutate(UCD_cr=100*URXUCD/URXUCR)

cad0102_design <-svydesign(id = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           weights = ~WTSHM2YR,
                           nest = TRUE,
                           data = cad0102)

cad0102_chems<-c(~URXUCD, ~UCD_cr)

cad0102_qrl<-map(cad0102_chems, sum_q95, cad0102_design)
names(cad0102_qrl) <- paste0("q95_",str_sub(as.character(cad0102_chems), 2, 7))
cad0102_qrdf<-bind_rows(cad0102_qrl, .id="chemical")
cad0102_95q<- pivot_wider(cad0102_qrdf, names_from = "chemical", values_from = "q95" )
cad0102_95q$cycle<- "2001-2002"


cad0304 <- nhanes_load_data("L06HM_C", "2003-2004", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, URXUCD) %>%
  left_join(creat0304, by="SEQN") %>%
  mutate(UCD_cr=100*URXUCD/URXUCR)

cad0304_design <-svydesign(id = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           weights = ~WTSA2YR,
                           nest = TRUE,
                           data = cad0304)

cad0304_chems<-c(~URXUCD, ~UCD_cr)

cad0304_qrl<-map(cad0304_chems, sum_q95, cad0304_design)
names(cad0304_qrl) <- paste0("q95_",str_sub(as.character(cad0304_chems), 2, 7))
cad0304_qrdf<-bind_rows(cad0304_qrl, .id="chemical")
cad0304_95q<- pivot_wider(cad0304_qrdf, names_from = "chemical", values_from = "q95" )
cad0304_95q$cycle<- "2003-2004"


cad0506 <- nhanes_load_data("UHM_D", "2005-2006", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, URXUCD) %>%
  left_join(creat0506, by="SEQN") %>%
  mutate(UCD_cr=100*URXUCD/URXUCR)

cad0506_design <-svydesign(id = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           weights = ~WTSA2YR,
                           nest = TRUE,
                           data = cad0506)

cad0506_chems<-c(~URXUCD, ~UCD_cr)

cad0506_qrl<-map(cad0506_chems, sum_q95, cad0506_design)
names(cad0506_qrl) <- paste0("q95_",str_sub(as.character(cad0506_chems), 2, 7))
cad0506_qrdf<-bind_rows(cad0506_qrl, .id="chemical")
cad0506_95q<- pivot_wider(cad0506_qrdf, names_from = "chemical", values_from = "q95" )
cad0506_95q$cycle<- "2005-2006"


cad0708 <- nhanes_load_data("UHM_E", "2007-2008", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, URXUCD) %>%
  left_join(creat0708, by="SEQN") %>%
  mutate(UCD_cr=100*URXUCD/URXUCR)

cad0708_design <-svydesign(id = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           weights = ~WTSA2YR,
                           nest = TRUE,
                           data = cad0708)

cad0708_chems<-c(~URXUCD, ~UCD_cr)

cad0708_qrl<-map(cad0708_chems, sum_q95, cad0708_design)
names(cad0708_qrl) <- paste0("q95_",str_sub(as.character(cad0708_chems), 2, 7))
cad0708_qrdf<-bind_rows(cad0708_qrl, .id="chemical")
cad0708_95q<- pivot_wider(cad0708_qrdf, names_from = "chemical", values_from = "q95" )
cad0708_95q$cycle<- "2007-2008"

cad0910 <- nhanes_load_data("UHM_F", "2009-2010", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, URXUCD) %>%
  left_join(creat0910, by="SEQN") %>%
  mutate(UCD_cr=100*URXUCD/URXUCR)

cad0910_design <-svydesign(id = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           weights = ~WTSA2YR,
                           nest = TRUE,
                           data = cad0910)

cad0910_chems<-c(~URXUCD, ~UCD_cr)

cad0910_qrl<-map(cad0910_chems, sum_q95, cad0910_design)
names(cad0910_qrl) <- paste0("q95_",str_sub(as.character(cad0910_chems), 2, 7))
cad0910_qrdf<-bind_rows(cad0910_qrl, .id="chemical")
cad0910_95q<- pivot_wider(cad0910_qrdf, names_from = "chemical", values_from = "q95" )
cad0910_95q$cycle<- "2009-2010"


cad1112 <- nhanes_load_data("UHM_G", "2011-2012", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, URXUCD) %>%
  left_join(creat1112, by="SEQN") %>%
  mutate(UCD_cr=100*URXUCD/URXUCR) %>%
  filter(is.na(WTSA2YR)==F)  #remove the 43 observations with missing weights and data

cad1112_design <-svydesign(id = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           weights = ~WTSA2YR,
                           nest = TRUE,
                           data = cad1112)

cad1112_chems<-c(~URXUCD, ~UCD_cr)

cad1112_qrl<-map(cad1112_chems, sum_q95, cad1112_design)
names(cad1112_qrl) <- paste0("q95_",str_sub(as.character(cad1112_chems), 2, 7))
cad1112_qrdf<-bind_rows(cad1112_qrl, .id="chemical")
cad1112_95q<- pivot_wider(cad1112_qrdf, names_from = "chemical", values_from = "q95" )
cad1112_95q$cycle<- "2011-2012"


cad1314 <- nhanes_load_data("UM_H", "2013-2014", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, URXUCD) %>%
  left_join(creat1314, by="SEQN") %>%
  mutate(UCD_cr=100*URXUCD/URXUCR)

cad1314_design <-svydesign(id = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           weights = ~WTSA2YR,
                           nest = TRUE,
                           data = cad1314)

cad1314_chems<-c(~URXUCD, ~UCD_cr)

cad1314_qrl<-map(cad1314_chems, sum_q95, cad1314_design)
names(cad1314_qrl) <- paste0("q95_",str_sub(as.character(cad1314_chems), 2, 7))
cad1314_qrdf<-bind_rows(cad1314_qrl, .id="chemical")
cad1314_95q<- pivot_wider(cad1314_qrdf, names_from = "chemical", values_from = "q95" )
cad1314_95q$cycle<- "2013-2014"


cad1516 <- nhanes_load_data("UM_I", "2015-2016", demographics = TRUE) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTSA2YR, URXUCD) %>%
  left_join(creat1516, by="SEQN") %>%
  mutate(UCD_cr=100*URXUCD/URXUCR)

cad1516_design <-svydesign(id = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           weights = ~WTSA2YR,
                           nest = TRUE,
                           data = cad1516)

cad1516_chems<-c(~URXUCD, ~UCD_cr)

cad1516_qrl<-map(cad1516_chems, sum_q95, cad1516_design)
names(cad1516_qrl) <- paste0("q95_",str_sub(as.character(cad1516_chems), 2, 7))
cad1516_qrdf<-bind_rows(cad1516_qrl, .id="chemical")
cad1516_95q<- pivot_wider(cad1516_qrdf, names_from = "chemical", values_from = "q95" )
cad1516_95q$cycle<- "2015-2016"


cadmium_95q <- bind_rows(cad9900_95q,
                         cad0102_95q,
                         cad0304_95q,
                         cad0506_95q,
                         cad0708_95q,
                         cad0910_95q,
                         cad1112_95q,
                         cad1314_95q,
                         cad1516_95q)


# Merge the cut-offs for the different chemical groups together by cycle
cut95 <- phthalates_95q %>%
  left_join(phenols_95q, by="cycle") %>%
  left_join(urineVOC_95q, by="cycle") %>%
  left_join(bloodVOC_95q, by="cycle") %>%
  left_join(PAH_95q, by="cycle") %>%
  left_join(pfas_95q, by="cycle") %>%
  left_join(lead_mercury_95q, by="cycle") %>%
  left_join(cadmium_95q, by="cycle")

# Save cut-offs so we can merge them in with the public dataset
write_rds(cut95, "data/cut95.rds")
