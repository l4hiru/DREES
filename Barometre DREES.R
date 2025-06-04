# Baromètre DREES

#0) Packages

library(arrow)
library(haven)       
library(dplyr)        
library(tidyverse)    
library(summarytools) 
library(stargazer)    
library(plm)
library(sandwich)

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

freq(data$rsa_estimate) # 20% N.A (need to check if missing years !)

# Today, the RSA for a single person who does not work is around XXXX euros a month. Which of these opinions do you agree with most?

freq(data$Q35)

data <- data %>%
  mutate(
    increase_rsa = case_when(
      Q35 == 1 ~ 3,
      Q35 == 3 ~ 2,
      Q35 == 2 ~ 1,  
      Q35 == 4 ~ NA_real_)
  )

freq(data$increase_rsa) #1: Decrease / 2: Maintain / 3: Increase


#B) Explanatory variables

# There are too many immigrant workers 

freq(data$Q13_2)

data <- data %>% mutate(xenophobia = ifelse(Q13_2 == 3, NA, 3 - Q13_2))

data <- data %>%
  mutate(Xenophobia = ifelse(xenophobia == 2, 1,
                      ifelse(!is.na(xenophobia), 0, NA)))

freq(data$Xenophobia)

# Shift-share IV / à la Bartik

#C) Control variables 

# Gender 

freq(data$SEXE)

data$Women <- ifelse(data$SEXE == 2, 1, 0)

# Age

freq(data$AGE_BR)

data$Age <- as.numeric(data$AGE_BR)

# Marital Status

freq(data$SITFAM)

data <- data %>%
  mutate(Married = case_when(
    SITFAM == 1 ~ 1,
    SITFAM %in% c(2, 3, 4) ~ 0,
  ))

freq(data$Married)

# Diploma 

freq(data$Q111)

data <- data %>%
  mutate(Diploma = case_when(
    Q111 %in% c(1, 2) ~ "Low",
    Q111 %in% c(3, 4) ~ "Medium",
    Q111 %in% c(5, 6, 7, 8) ~ "High",
    TRUE ~ NA_character_
  ),
  Diploma = factor(Diploma, levels = c("Low", "Medium", "High")))

freq(data$Diploma)

# Occupation

freq(data$PPI)

data <- data %>%
  mutate(Occupation = case_when(
    PPI %in% c(1) ~ "Farmer",
    PPI %in% c(2) ~ "Craftmen",
    PPI %in% c(3) ~ "Executive",
    PPI %in% c(4) ~ "PI",
    PPI %in% c(5) ~ "Employee",
    PPI %in% c(6) ~ "Worker",
    PPI %in% c(7) ~ "Unemployed",
    PPI %in% c(8) ~ "Pensioner",
    PPI %in% c(9) ~ "Inactive",
    PPI %in% c(10) ~ "Inactive",
    TRUE ~ NA_character_
  ),
  Occupation = relevel(factor(Occupation), ref = "Worker"))

freq(data$Occupation)

# Employment

freq(data$STATUTPPIbis)

data <- data %>%
  mutate(Employment = case_when(
    STATUTPPIbis == 1 ~ "Public",
    STATUTPPIbis == 2 ~ "Private",
    STATUTPPIbis == 3 ~ "Independent",
    STATUTPPIbis == 4 ~ "Employer",
    TRUE ~ NA_character_
  )) %>%
  mutate(Employment = factor(Employment, levels = c("Public", "Private", "Independent", "Employer")))

freq(data$Employment) #45% de N.A ! 

data <- data %>%
  mutate(
    Public      = ifelse(STATUTPPIbis == 1, 1, 0),
    Private     = ifelse(STATUTPPIbis == 2, 1, 0),
    Independent = ifelse(STATUTPPIbis == 3, 1, 0),
    Boss        = ifelse(STATUTPPIbis == 4, 1, 0)
  )

freq(data$Public)

# Unioner

data <- data %>%
  mutate(Unioner = case_when(
    Q103_1 == 1 ~ 1,
    Q103_1 == 2 ~ 0,
  ))

# Income Brackets

freq(data$Q110BIS)

data <- data %>%
  mutate(IncomeBrackets = ifelse(Q110BIS == 8, NA, Q110BIS)) %>%
  mutate(IncomeBrackets = factor(IncomeBrackets,
                                 levels = 1:7,
                                 labels = c(
                                   "Moins de 1000€",
                                   "1000–1499€",
                                   "1400–1900€",
                                   "1900–2400€",
                                   "2400–3800€",
                                   "3800–5300€",
                                   "5300€ et plus"))) %>%
  mutate(IncomeBrackets = relevel(IncomeBrackets, ref = "Moins de 1000€"))

freq(data$IncomeBrackets)

# Year 

data$Year <- as.factor(data$ANNEE)

freq(data$Year)

#III) Regression Analysis

ols <- lm(pmore_health_insurance ~ Xenophobia + Women + Age + Married + Diploma + Occupation 
          + Public + Private + Independent + Boss + IncomeBrackets + Unioner + Year, data = data)
ols2 <- lm(pmore_pension ~ Xenophobia + Women + Age + Married + Diploma + Occupation 
  + Public + Private + Independent + Boss + IncomeBrackets + Unioner + Year, data = data)
ols3 <- lm(pmore_family  ~ Xenophobia + Women + Age + Married + Diploma + Occupation 
  + Public + Private + Independent + Boss + IncomeBrackets + Unioner + Year, data = data)
ols4 <- lm(pmore_unemployed  ~ Xenophobia + Women + Age + Married + Diploma + Occupation 
  + Public + Private + Independent + Boss + IncomeBrackets + Unioner + Year, data = data)
ols5 <- lm(pmore_disabled  ~ Xenophobia + Women + Age + Married + Diploma + Occupation 
  + Public + Private + Independent + Boss + IncomeBrackets + Unioner + Year, data = data)
ols6 <- lm(pmore_dependent ~ Xenophobia + Women + Age + Married + Diploma + Occupation 
  + Public + Private + Independent + Boss + IncomeBrackets + Unioner + Year, data = data)

ols7 <- lm(increase_rsa ~ Xenophobia + Women + Age + Married + Diploma + Occupation 
  + Public + Private + Independent + Boss + IncomeBrackets + Unioner + Year, data = data)


se_ols <- sqrt(diag(vcovHC(ols, type = "HC1")))
se_ols2 <- sqrt(diag(vcovHC(ols2, type = "HC1")))
se_ols3 <- sqrt(diag(vcovHC(ols3, type = "HC1")))
se_ols4 <- sqrt(diag(vcovHC(ols4, type = "HC1")))
se_ols5 <- sqrt(diag(vcovHC(ols5, type = "HC1")))
se_ols6 <- sqrt(diag(vcovHC(ols6, type = "HC1")))

se_ols7 <- sqrt(diag(vcovHC(ols7, type = "HC1")))
se_ols8 <- sqrt(diag(vcovHC(ols8, type = "HC1")))

stargazer(
  ols, ols2, ols3, ols4, ols5, ols6,
  se = list(se_ols, se_ols2, se_ols3, se_ols4, se_ols5, se_ols6),
  type = "text"
)

stargazer(
  ols7,
  se = list(se_ols7),
  type = "text"
)
