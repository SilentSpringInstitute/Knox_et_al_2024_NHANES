# Make figures 2-5 for the paper. 
# Also exports underlying data for figures for inclusion in the supplemental excel file.
# Note: these figures are based on public data

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

# Read in the public data
public_data <- read_rds("data/public_data_for_figures.rds")

##########################################################################################
##########################################################################################

### Public Data Figures for Chemicals Listed Prior to Start of Biomonitoring Data

##########################################################################################

dichlorophenol_2_5_public <-
ggplot(filter(public_data, chemical=="X14Dcr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 1000), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2003-2004","2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("03-\n04","05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("2,5-dichlorophenol (p-DCB)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


acrylonitrile_public <-
  ggplot(filter(public_data, chemical=="CYM_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 500), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("CYMA (acrylonitrile)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


butadience_1_3_public_metab1 <-
  ggplot(filter(public_data, chemical=="DHB_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 1000), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("DHBMA (1,3-butadiene)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


butadience_1_3_public_metab2 <-
  ggplot(filter(public_data, chemical=="MB3_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 100), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("MHBMA3 (1,3-butadiene)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


propylene_oxide_public <-
  ggplot(filter(public_data, chemical=="HP2_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 500), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("2HPMA (propylene oxide)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


xylene_public <-
  ggplot(filter(public_data, chemical=="X34Mcr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 2000), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("3MHA+4MHA (xylene)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  



pyrene_public <-
  ggplot(filter(public_data, chemical=="P10_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 1000), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2001-2002", "2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012","2013-2014"), labels=c("01-\n02", "03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12","13-\n14")) +
  ggtitle("1-hydroxypyrene (pyrene))") +
  xlab("") +
  ylab("concentration (ng/g of creatinine)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


fluorene_public <-
  ggplot(filter(public_data, chemical=="P04_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 2500), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2001-2002", "2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012","2013-2014"), labels=c("01-\n02", "03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12","13-\n14")) +
  ggtitle("2-hydroxyfluorene (fluorene)") +
  xlab("") +
  ylab("concentration (ng/g of creatinine)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  



phenanthrene_public_metab1 <-
  ggplot(filter(public_data, chemical=="P05_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 500), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2001-2002", "2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012"), labels=c("01-\n02", "03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12")) +
  ggtitle("3-hydroxyphenanthrene (phenanthrene)") +
  xlab("") +
  ylab("concentration (ng/g of creatinine)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


phenanthrene_public_metab2 <-
  ggplot(filter(public_data, chemical=="P06_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 1000), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2001-2002", "2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012", "2013-2014"), labels=c("01-\n02", "03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12", "13-\n14")) +
  ggtitle("1-hydroxyphenanthrene (phenanthrene)") +
  xlab("") +
  ylab("concentration (ng/g of creatinine)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


lead_public <-
  ggplot(filter(public_data, chemical=="LBXBPB")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 10), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("1999-2000", "2001-2002", "2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012", "2013-2014", "2015-2016"), labels=c("99-\n00", "01-\n02", "03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12", "13-\n14", "15-\n16")) +
  ggtitle("lead") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/dL)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


cadmium_public <-
  ggplot(filter(public_data, chemical=="UCD_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 2), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("1999-2000", "2001-2002", "2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012", "2013-2014", "2015-2016"), labels=c("99-\n00", "01-\n02", "03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12", "13-\n14", "15-\n16")) +
  ggtitle("cadmium") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


mercury_public <-
  ggplot(filter(public_data, chemical=="LBXTHG")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 10), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("1999-2000", "2001-2002", "2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012", "2013-2014", "2015-2016"), labels=c("99-\n00", "01-\n02", "03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12", "13-\n14", "15-\n16")) +
  ggtitle("mercury") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/L)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  



#summary chart (non-diesel)
dichlorophenol_2_5_public + butadience_1_3_public_metab1 + butadience_1_3_public_metab2 + acrylonitrile_public + propylene_oxide_public + cadmium_public + lead_public + mercury_public +
  plot_annotation(title="Figure 3(a)") & theme(plot.title = element_text(hjust = 0.5, size=6,  family="<calibri>")) 
#ggsave(filename='tables and figures/final paper figures/figure3(a).pdf', width = 7.5, height = 7.5, units = "in", dpi=600)

fig3a_data <- public_data %>%
  filter(chemical %in% c("X14Dcr", "DHB_cr", "MB3_cr", "CYM_cr", "HP2_cr", "UCD_cr", "LBXBPB", "LBXTHG"))
#write_csv(fig3a_data, "tables and figures/figure_data_for_supplemental/fig3a_data.csv")


# summary chart (diesel)
xylene_public + fluorene_public + phenanthrene_public_metab1 + phenanthrene_public_metab2 + pyrene_public +
  plot_annotation(title="Figure 3(b)") & theme(plot.title = element_text(hjust = 0.5, size=6,  family="<calibri>")) 
#ggsave(filename='tables and figures/final paper figures/figure3(b).pdf', width = 7.5, height = 7.5, units = "in", dpi=600)

fig3b_data <- public_data %>%
  filter(chemical %in% c("X34Mcr","P04_cr", "P05_cr", "P06_cr", "P10_cr"))
#write_csv(fig3b_data, "tables and figures/figure_data_for_supplemental/fig3b_data.csv")

#########################################################################################
##########################################################################################

### Public Data Figures for Chemicals Listed After the End of Biomonitoring Data

##########################################################################################

styrene_public <-
  ggplot(filter(public_data, chemical=="MAD_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 500), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("mandelic acid (styrene)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


pfoa_public <-
  ggplot(filter(public_data, chemical=="LBXPFOA")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 20), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("PFOA") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/L)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


pfos_public <-
  ggplot(filter(public_data, chemical=="LBXPFOS")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 100), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("PFOS") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/L)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


##########################################################################################
##########################################################################################

### Public Data Figures for Closely Related Chemicals

##########################################################################################

mpah_public <-
  ggplot(filter(public_data, chemical=="LBXMPAH")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 5), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("N-MeFOSAA") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/L)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


pfde_public <-
  ggplot(filter(public_data, chemical=="LBXPFDE")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 5), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("PFDA") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/L)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


pfhs_public <-
  ggplot(filter(public_data, chemical=="LBXPFHS")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 10), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("PFHxS") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/L)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


pfna_public <-
  ggplot(filter(public_data, chemical=="LBXPFNA")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 5), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("PFNA") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/L)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


di_isobutyl_phthalate_public <-
  ggplot(filter(public_data, chemical=="MIB_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 100), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2001-2002", "2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("01-\n02","03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("MiBP (DiBP)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


di_ethyl_phthalate_public <-
ggplot(filter(public_data, chemical=="MEP_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 2000), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("1999-2000", "2001-2002", "2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("99-\n00", "01-\n02", "03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("MEP (DEP)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  

benzophenone_3_public <-
  ggplot(filter(public_data, chemical=="BP3_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 2500), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("benzophenone-3") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


triclosan_public <-
  ggplot(filter(public_data, chemical=="TRS_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 1000), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("triclosan") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  

dichlorophenol_2_4_public <-
  ggplot(filter(public_data, chemical=="DCB_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 50), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2003-2004", "2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("03-\n04", "05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("2,4-dichlorophenol") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


methyl_paraben_public <-
  ggplot(filter(public_data, chemical=="MPB_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 1000), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("methyl paraben") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


propyl_paraben_public <-
  ggplot(filter(public_data, chemical=="PPB_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 1000), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  ggtitle("propyl paraben") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


bps_public <-
  ggplot(filter(public_data, chemical=="BPS_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 10), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2013-2014","2015-2016"), labels=c("13-\n14","15-\n16")) +
  ggtitle("BPS") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


di_ethyl_phthalate_public + di_isobutyl_phthalate_public + dichlorophenol_2_4_public + benzophenone_3_public + bps_public + methyl_paraben_public + propyl_paraben_public + triclosan_public + styrene_public + mpah_public + pfde_public + pfhs_public + pfna_public + pfoa_public + pfos_public +
  plot_annotation(title="Figure 4") & theme(plot.title = element_text(hjust = 0.5, size=6,  family="<calibri>")) 
#ggsave(filename='tables and figures/final paper figures/figure4.pdf', width = 7.5, height = 7.5, units = "in", dpi=600)

fig4_data <- public_data %>%
  filter(chemical %in% c("MEP_cr", "MIB_cr", "DCB_cr", "BP3_cr", "BPS_cr", "MPB_cr", "PPB_cr", "TRS_cr", "MAD_cr", "LBXMPAH", "LBXPFDE", "LBXPFHS", "LBXPFNA", "LBXPFOA", "LBXPFOS"))
#write_csv(fig4_data, "tables and figures/figure_data_for_supplemental/fig4_data.csv")


##########################################################################################
##########################################################################################

### Public Data Figures for Chemicals Listed During Biomonitoring Period (figure 2)

##########################################################################################

### Toluene ###
toluene_public <-
  ggplot(filter(public_data, chemical=="LBXVTO")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", breaks = c(0.02, 0.05, 0.14, 0.37, 1.00),labels = c(0.02, 0.05, 0.14, 0.37, 1.00), limits = c(NA, 2)) +
  scale_x_discrete(limits=c("1999-2000", "2001-2002", "2003-2004","2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("99-\n00", "01-\n02", "03-\n04","05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  geom_vline(xintercept = 5.55, colour="pink", linetype="dashed", size=.5) +
  ggtitle("toluene") +
  xlab("") +
  ylab("concentration (ng/mL)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


### Benzylbutyl Phthalate ###
benzylbutyl_phthalate_public <-
  ggplot(filter(public_data, chemical=="MZP_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 100), breaks = c(1.0, 2.7, 7.4, 20.1, 54.6), labels = c(1.0, 2.7, 7.4, 20.1, 54.6)) +
  scale_x_discrete(limits=c("1999-2000", "2001-2002", "2003-2004","2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("99-\n00", "01-\n02", "03-\n04","05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  geom_vline(xintercept = 3.55, colour="pink", linetype="dashed", size=.5) +
  #annotate("text" ,x=2, y=95, label="Pre-Listing", size=2, family="<calibri>") +
  #annotate("text", x=7, y=95, label="Post-Listing", size=2, family="<calibri>") +
  ggtitle("MBzP (BBP)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  



### Naphthalene ###
naphthalene_public <-
  ggplot(filter(public_data, chemical=="P02_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 100), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2001-2002", "2003-2004","2005-2006","2007-2008","2009-2010","2011-2012","2013-2014"), labels=c("01-\n02", "03-\n04","05-\n06","07-\n08","09-\n10","11-\n12","13-\n14")) +
  geom_vline(xintercept = 1, colour="pink", linetype="dashed", size=.5) +
  ggtitle("2-naphthol (naphthalene)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


### di(2-ethylhexyl) phthalate ###
di_2_ethylhexyl_phthalate_public <-
  ggplot(filter(public_data, chemical=="MHH_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 250), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2001-2002", "2003-2004","2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("01-\n02", "03-\n04","05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  geom_vline(xintercept = 1.55, colour="pink", linetype="dashed", size=.5) +
  ggtitle("MEHHP (DEHP)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


### Di-n-butyl phthalate ###
di_n_butyl_phthalate_public <-
  ggplot(filter(public_data, chemical=="MBP_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 100), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("1999-2000", "2001-2002", "2003-2004","2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("99-\n00", "01-\n02", "03-\n04","05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  geom_vline(xintercept = 3.55, colour="pink", linetype="dashed", size=.5) +
  ggtitle("MnBP (DnBP)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


### di-isodecyl phthalate ###
di_isodecyl_phthalate_public <-
  ggplot(filter(public_data, chemical=="CNP_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 25), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  geom_vline(xintercept = 1.55, colour="pink", linetype="dashed", size=.5) +
  ggtitle("MCNP (DIDP)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


### Di-isononyl Phthalate ###
di_isononyl_phthalate_public <-
  ggplot(filter(public_data, chemical=="COP_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 200), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  geom_vline(xintercept = 4.55, colour="pink", linetype="dashed", size=.5) +
  ggtitle("MCOP (DINP)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


### bisphenol A ###
bpa_public <-
  ggplot(filter(public_data, chemical=="BPH_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 20), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2003-2004","2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("03-\n04","05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  geom_vline(xintercept = 5.55, colour="pink", linetype="dashed", size=.5) +
  ggtitle("BPA") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


### Chloroform ###
chloroform_public <-
  ggplot(filter(public_data, chemical=="LBXVCF")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 200), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("1999-2000", "2001-2002", "2003-2004","2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016"), labels=c("99-\n00", "01-\n02", "03-\n04","05-\n06","07-\n08","09-\n10","11-\n12","13-\n14","15-\n16")) +
  geom_vline(xintercept = 5.55, colour="pink", linetype="dashed", size=.5) +
  ggtitle("chloroform") +
  xlab("") +
  ylab("concentration (picograms/mL)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


### Acrylamide ###
acrylamide_public <-
  ggplot(filter(public_data, chemical=="AAM_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 300), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2005-2006","2011-2012","2013-2014","2015-2016"), labels=c("05-\n06","11-\n12","13-\n14","15-\n16")) +
  geom_vline(xintercept = 1.55, colour="pink", linetype="dashed", size=.5) +
  ggtitle("AAMA (acrylamide)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


### Cyanide ###
# presentation version
cyanide_public <-
  ggplot(filter(public_data, chemical=="ATC_cr")) +
  geom_boxplot(aes(x=cycle, ymin=p5,lower=p25, middle=p50, upper=p75, ymax=p95), 
               stat = "identity", fill="#d9d9d9") +
  scale_y_continuous(trans = "log", limits = c(NA, 1000), breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
  scale_x_discrete(limits=c("2005-2006","2011-2012","2013-2014","2015-2016"), labels=c("05-\n06","11-\n12","13-\n14","15-\n16")) +
  geom_vline(xintercept = 2.55, colour="pink", linetype="dashed", size=.5) +
  ggtitle("ATCA (cyanide)") +
  xlab("") +
  ylab(expression(paste("concentration (", mu, "g/g of creatinine)"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title = element_text(size = 6, family="<calibri>"),
        axis.text.x=element_text(hjust = 0.5, vjust = 0.5, size = 6, family="<calibri>"),
        axis.text.y = element_text(size = 6, family="<calibri>"),
        plot.title = element_text(hjust = 0.5, size=6, family="<calibri>"))  


# Pull all the public data figures for the diffs-in-diffs chems into one figure
benzylbutyl_phthalate_public + di_2_ethylhexyl_phthalate_public + di_isodecyl_phthalate_public + di_isononyl_phthalate_public + di_n_butyl_phthalate_public +  bpa_public + chloroform_public + toluene_public + acrylamide_public + cyanide_public + naphthalene_public +
  plot_annotation(title="Figure 2") & theme(plot.title = element_text(hjust = 0.5, size=6,  family="<calibri>")) 
#ggsave(filename='tables and figures/final paper figures/figure2.pdf', width = 7.5, height = 7.5, units = "in", dpi=600)

fig2_data <- public_data %>%
  filter(chemical %in% c("MZP_cr", "MHH_cr", "CNP_cr", "COP_cr", "MBP_cr", "BPH_cr", "LBXVCF", "LBXVTO", "AAM_cr", "ATC_cr", "P02_cr"))
#write_csv(fig2_data, "tables and figures/figure_data_for_supplemental/fig2_data.csv")
