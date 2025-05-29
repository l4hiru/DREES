# Baromètre DREES

#0) Packages

library(arrow)
library(haven)       
library(dplyr)        
library(tidyverse)    
library(summarytools) 
library(stargazer)    
library(plm)

#I) Dataset 

data <- read.csv2("Base 2000 - 2013/Csv/barodrees_unif00_13v8.csv") # Unified dataset 2000 - 2013

table(data$DEPT, data$ANNEE) # Missing for 2007 - 2012 waves

#II) Variables 

#A) Outcome variables

# In the future, given your level of resources,  would you be prepared to CONTRIBUTE MORE to maintain the level of benefits?

data <- data %>%
  mutate(
    pmore_health_insurance = ifelse(Q91_1 == 5, NA, 5 - Q91_1), # Health Insurance 
    pmore_pension          = ifelse(Q91_2 == 5, NA, 5 - Q91_2), # Pension allowances
    pmore_family           = ifelse(Q91_3 == 5, NA, 5 - Q91_3), # Family allowances
    pmore_unemployed       = ifelse(Q91_4 == 5, NA, 5 - Q91_4), # Unemployment benefits
    pmore_disabled         = ifelse(Q91_5 == 5, NA, 5 - Q91_5), # Help for disabled (no obs between 2000 - 2002)
    pmore_dependent        = ifelse(Q91_6 == 5, NA, 5 - Q91_6)  # Help for dependent elderly people (no obs between 2000 - 2002)
  )                                                             
                                                                # 1: Not at all vs. 5: Fully Agree

# How much is the RSA (revenu de solidarité active) for a single person who does not work (per month in euros)?

data$rsa_estimate <- as.numeric(data$Q32_BR)

freq(data$rsa_estimate) # 20% N.A 