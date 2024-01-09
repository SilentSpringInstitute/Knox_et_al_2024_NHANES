# Make figure 1 for the paper

# WRITTEN IN R VERSION: R version 4.0.3 (2020-10-10)

library(tidyverse)
library(survey)
library(patchwork)
library(sysfonts)
library(showtext)
library(svglite)

# Set the working directory
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)
setwd('..') 
setwd('..') 

options(stringsAsFactors = FALSE)
options(scipen = 999)

# Load fonts
font_add(family="<calibri>", regular="C:/Windows/Fonts/calibri.ttf")
showtext_auto()


##########################################################################################
##########################################################################################

### Load in the Data

##########################################################################################
# Load in the quantile regression results
table12 <- read_csv("RDC Output/tables/tables/Table_12.csv") 

table12 <- table12 %>%
  mutate(chemical=str_trim(str_sub(chemical_quantile, 1, 7)),
         quant=str_sub(chemical_quantile, -4, -1)) %>%
  mutate(creatinine_indic = case_when(is.na(coef_ln_UCR)==F ~ "yes",
                                      TRUE ~ "no"),
         cotinine_indic = case_when(is.na(coef_ln_COT)==F ~ "yes",
                                    TRUE ~ "no"))


# Look at just the P65 specs
quants <- table12 %>%
  filter(chemical %in% c("ln_MZP", "ln_MBP", "ln_MHH", "ln_CNP", "ln_COP", "ln_BPH", "ln_AAM", "ln_ATC", "ln_BMA", "ln_VCF", "ln_VTO", "ln_P01", "ln_P02")) %>%
  filter(is.na(coef_CA)==F) %>%
  filter(is.na(coef_time)==T)

quants <- quants %>%
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
    chemical=="ln_P01" ~ coef_P65_URXP01,
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
      chemical=="ln_P01" ~ se_P65_URXP01,
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
      chemical=="ln_P01" ~ coef_P65_URXP01_CA,
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
      chemical=="ln_P01" ~ se_P65_URXP01_CA,
      chemical=="ln_P02" ~ se_P65_URXP02_CA))

quants <- quants %>%
  select(chemical, quant, creatinine_indic, cotinine_indic, coef_P65, se_P65, coef_P65_CA, se_P65_CA, coef_CA, se_CA, cov_CA_P65, `coef_(Intercept)`, `se_(Intercept)`)


# Select just the specifications we are interested in
quants <- quants %>%
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
  filter(incl=="yes") 

# Create the point estimate and standard error for the effect for CA post-listing
quants <- quants %>%
  mutate(CA_post_effect=coef_P65+coef_P65_CA,
         CA_post_effect_se=sqrt((se_P65^2)+(se_P65_CA^2)+2*cov_CA_P65))

# Now create 95% confidence intervals
quants <- quants %>%
  mutate(CA_post_effect_CI_low = CA_post_effect-1.96*CA_post_effect_se,
         CA_post_effect_CI_high = CA_post_effect+1.96*CA_post_effect_se,
         CA_pre_effect_CI_low = coef_CA-1.96*se_CA,
         CA_pre_effect_CI_high = coef_CA+1.96*se_CA,
         Non_post_effect_CI_low = coef_P65-1.96*se_P65,
         Non_post_effect_CI_high = coef_P65+1.96*se_P65)


# Clean up what we need for the plot
quants <- quants %>%
  select(chemical, quant, coef_P65, Non_post_effect_CI_low, Non_post_effect_CI_high, CA_post_effect, CA_post_effect_CI_low, CA_post_effect_CI_high) %>%
  rename(Non_post_effect=coef_P65)

quants <- quants %>%
  pivot_longer(cols=c(Non_post_effect, Non_post_effect_CI_low, Non_post_effect_CI_high, CA_post_effect, CA_post_effect_CI_low, CA_post_effect_CI_high), names_to="pop", values_to="value") %>%
  mutate(loc=str_sub(pop, 1, 3),
         est_type=str_sub(pop, -7, -1)) %>%
  mutate(loc=case_when(loc=="CA_" ~ "CA",
                       TRUE ~ loc),
         est_type=case_when(est_type=="_effect" ~ "post_effect",
                            est_type=="_CI_low" ~ "CI_low",
                            TRUE ~ est_type)) %>%
  select(-pop)


quants <- quants %>%
  pivot_wider(id_cols=c(chemical, quant, loc), names_from=est_type, values_from=value)


# Have to get everything in the same units - most in ng/mL
# But chloroform in picograms/mL -> divide by 1000
# And naphthalene in micrograms/L -> same as ng/mL so all set
quants <- quants %>%
  mutate(post_effect_new=case_when(chemical=="ln_VCF" ~ post_effect/1000,
                                   TRUE ~ post_effect),
         CI_low_new=case_when(chemical=="ln_VCF" ~ CI_low/1000,
                              TRUE ~ CI_low),
         CI_high_new=case_when(chemical=="ln_VCF" ~ CI_high/1000,
                              TRUE ~ CI_high))

prettynames <- as.data.frame(matrix(c(
  "ln_AAM", "AAMA (acrylamide)", 9,
  "ln_ATC", "ATCA (cyanide)", 10,
  "ln_BPH", "BPA", 6,
  "ln_CNP", "MCNP (DIDP)", 3,
  "ln_COP", "MCOP(DINP)", 4,
  "ln_MBP", "MnBP (DnBP)", 5,
  "ln_MHH", "MEHHP (DEHP)", 2,
  "ln_MZP", "MBzP (BBP)", 1,
  "ln_P02", "2-naphthol (napthalene)", 11,
  "ln_VCF", "chloroform", 7,
  "ln_VTO", "toluene", 8
), ncol=3, byrow=TRUE))
names(prettynames)<- c("chemical", "prettyname", "chemorder")

quants <- quants %>%
  left_join(prettynames, by="chemical")

quants <- quants %>%
  mutate(finalorder=as.numeric(chemorder)+ as.numeric(quant))

quants <- quants %>%
  mutate(prettyquant = case_when(quant=="0.25" ~ "25th",
                                 quant=="0.75" ~ "75th",
                                 quant=="0.95" ~ "95th")) %>%
  mutate(chem_quant=paste0(prettyname,": ", prettyquant))


##########################################################################################
##########################################################################################

### Make the figure

##########################################################################################


ggplot(quants, aes(x=reorder(chem_quant, -finalorder), y=post_effect_new, color=chemical, group=loc)) +
  geom_point(position=position_dodge(.6), size=.5) +
  geom_errorbar(aes(ymin=CI_low_new, ymax=CI_high_new, linetype=loc), width=.8, position=position_dodge(.6), size=.5) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  geom_hline(yintercept=0, color="black") +
  ylab("Change in Concentration\nPre to Post P65 Listing (ng/mL)") +
  xlab("") +
  ggtitle("Figure 1") +
  coord_flip() +
  theme_bw() +
  guides(color=FALSE) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        axis.title.x = element_text(size = 6, family="<calibri>"),
        axis.title.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"),
        legend.text = element_text(size=6, family="<calibri>"),
        legend.position=c(0.9, 0.9), 
        legend.title=element_blank())
#ggsave(filename='tables and figures/final paper figures/figure1.pdf', width = 7.5, height = 7.5, units = "in", dpi=600)

#export the raw figure data for the supplemental
fig1_data <- quants %>%
  select(chem_quant, loc, post_effect_new, CI_low_new, CI_high_new)
#write_csv(fig1_data, "tables and figures/figure_data_for_supplemental/fig1_data.csv")
