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
library(MASS)
library(erer)
library(DescTools)
library(flexmix)

#I) Dataset 

# data <- read.csv2("Base 2000 - 2013/Csv/barodrees_unif00_13v8.csv") # Unified dataset 2000 - 2013

data <- read_dta("2023/barometre2000_2023_diff.dta") # Unified dataset 2000 - 2023

#II) Variables 

#A) Outcome variables

# Would you accept a reduction ... in exchange for a reduction in your taxes or payroll taxes ?

data <- data %>%
  mutate(
    pless_health_insurance = ifelse(ps13_ab_1 %in% c(5, 999999999), NA, 5 - ps13_ab_1), # Health Insurance 
    pless_pension          = ifelse(ps13_ab_2 %in% c(5, 999999999), NA, 5 - ps13_ab_2), # Pension allowances
    pless_family           = ifelse(ps13_ab_3 %in% c(5, 999999999), NA, 5 - ps13_ab_3), # Family allowances
    pless_unemployed       = ifelse(ps13_ab_4 %in% c(5, 999999999), NA, 5 - ps13_ab_4), # Unemployment benefits
    pless_disabled         = ifelse(ps13_ab_5 %in% c(5, 999999999), NA, 5 - ps13_ab_5), # Help for disabled (no obs between 2000 - 2002) / 2004 according DREES 2013
    pless_dependent        = ifelse(ps13_ab_6 %in% c(5, 999999999), NA, 5 - ps13_ab_6),  # Help for dependent elderly people (no obs between 2000 - 2002) / 2004 according DREES 2013
    pless_housing          = ifelse(ps13_ab_7 %in% c(5, 999999999), NA, 5 - ps13_ab_7), # Housing benefits (since 2015 accoring to DREES 2023)
  )                                                             
                                                                # 1: Not at all vs. 4: Fully Agree

# Today, the RSA for a single person who does not work is around XXXX euros a month. Which of these opinions do you agree with most?

freq(data$pe09)

data <- data %>%
  mutate(
    increase_rsa = case_when(
      pe09 == 1 ~ 3,
      pe09 == 3 ~ 2,
      pe09 == 2 ~ 1,  
      pe09 == 999999999 ~ NA_real_)
  )

freq(data$increase_rsa)

# France devotes around a third of its national income to financing social protection. Do you consider this to be ... ?

freq(data$ps03)

data <- data %>%
  mutate(
    toomuchTforPS = case_when(
      ps03 == 1 ~ 3,
      ps03 == 2 ~ 2,
      ps03 == 3 ~ 1,  
      ps03 == 999999999 ~ NA_real_)
  )

freq(data$toomuchTforPS)

#B) Explanatory variables

freq(data$og08_1)

data <- data %>%
  mutate(
    xenophobia = case_when(
      annee <= 2013 & og08_1 %in% c(2, 3) ~ og08_1,
      annee >= 2014 & og08_1 %in% c(1, 2) ~ 2,
      annee >= 2014 & og08_1 %in% c(3, 4) ~ 3,
      og08_1 == 999999999 | is.na(og08_1) ~ NA_real_,
      TRUE ~ NA_real_
    ))                            # 4-points Likert scale is only since 2014 and onwards

data$xenophobia <- ifelse(data$xenophobia == 2, 1, 0)

freq(data$xenophobia)

#C) Control variables

# Gender

freq(data$sdsexe)

data$women <- ifelse(data$sdsexe == 2, 1, 0)

# Age

freq(data$sdage)

data$age <- as.numeric(data$sdage)

# Married

freq(data$sdmatri)

data$married <- ifelse(data$sdmatri == 1, 1, 0) # Include also civil union and cohabitation 

# Diploma 

freq(data$sddipl)

data <- data %>%
  mutate(diploma = case_when(
    sddipl %in% c(1, 2) ~ "low",
    sddipl %in% c(3, 4) ~ "medium",
    sddipl %in% c(5, 6, 7, 8) ~ "high",
    TRUE ~ NA_character_
  ),
  diploma = factor(diploma, levels = c("low", "medium", "high")))

freq(data$diploma)

# Occupation 

freq(data$sdpcs10)

data <- data %>%
  mutate(occupation = case_when(
    sdpcs10 %in% c(1) ~ "farmer",
    sdpcs10 %in% c(2) ~ "craftmen",
    sdpcs10 %in% c(3) ~ "executive",
    sdpcs10 %in% c(4) ~ "pi",
    sdpcs10 %in% c(5) ~ "employee",
    sdpcs10 %in% c(6) ~ "worker",
    sdpcs10 %in% c(7) ~ "unemployed",
    sdpcs10 %in% c(8) ~ "pensioner",
    sdpcs10 %in% c(9) ~ "inactive",
    sdpcs10 %in% c(10) ~ "inactive",
    TRUE ~ NA_character_
  ),
  occupation = relevel(factor(occupation), ref = "worker"))

freq(data$occupation)

# Employment

# Unioner

freq(data$sdassynd_1)

data <- data %>%
  mutate(unioner = case_when(
    sdassynd_1 == 1 ~ 1,
    sdassynd_1 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

freq(data$unioner)

# Income Brackets 

freq(data$sdrevtr)

table(data$sdrevtr, data$annee)

data <- data %>%
  mutate(income = ifelse(sdrevtr == 8, NA, sdrevtr)) %>%
  mutate(income = factor(income,
                                 levels = 1:7,
                                 labels = c(
                                   "moins de 1000€",
                                   "1000–1400€",
                                   "1400–1900€",
                                   "1900–2400€",
                                   "2400–3800€",
                                   "3800–5300€",
                                   "5300€ et plus"))) %>%
  mutate(income = relevel(income, ref = "moins de 1000€"))

freq(data$income)

# Year 

data$year <- as.factor(data$annee)

freq(data$year)



ols_unemployed <- lm(pless_unemployed ~ Xenophobia, data = data)
ols_health_insurance <- lm(pless_health_insurance ~ Xenophobia, data = data)
ols_family <- lm(pless_family ~ Xenophobia, data = data)
ols_disabled <- lm(pless_disabled ~ Xenophobia, data = data)
ols_dependent <- lm(pless_dependent ~ Xenophobia, data = data)
ols_housing <- lm(pless_housing ~ Xenophobia, data = data)
ols_pension <- lm(pless_pension ~ Xenophobia, data = data)


stargazer(ols_pension, type = "text")

# -------------------- 1st UNIFIED DATASET (2000 - 2013) --------------------

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

#III) Regression Analysis$

# A) Linear Regression Models

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

# B) Ordered Logit Models

data <- data %>%
  mutate(
    pmore_health_insurance_ordered = factor(pmore_health_insurance,
                                            levels = c(1, 2, 3, 4),
                                            ordered = TRUE),
    pmore_pension_ordered = factor(pmore_pension,
                                   levels = c(1, 2, 3, 4),
                                   ordered = TRUE),
    pmore_family_ordered = factor(pmore_family,
                                  levels = c(1, 2, 3, 4),
                                  ordered = TRUE),
    pmore_unemployed_ordered = factor(pmore_unemployed,
                                      levels = c(1, 2, 3, 4),
                                      ordered = TRUE),
    increase_rsa_ordered = factor(increase_rsa,
                                      levels = c(1, 2, 3),
                                      ordered = TRUE)
  )

logit_ord1 <- polr(pmore_health_insurance_ordered ~ Xenophobia + Women + Age + Married + Diploma + Occupation 
  + Public + Private + Independent + Boss + IncomeBrackets + Unioner + Year, data = data, method = "logistic")

logit_ord2 <- polr(pmore_pension_ordered ~ Xenophobia + Women + Age + Married + Diploma + Occupation 
  + Public + Private + Independent + Boss + IncomeBrackets + Unioner + Year, data = data, method = "logistic")

logit_ord3 <- polr(pmore_family_ordered ~ Xenophobia + Women + Age + Married + Diploma + Occupation 
  + Public + Private + Independent + Boss + IncomeBrackets + Unioner + Year, data = data, method = "logistic")

logit_ord4 <- polr(pmore_unemployed_ordered ~ Xenophobia + Women + Age + Married + Diploma + Occupation 
  + Public + Private + Independent + Boss + IncomeBrackets + Unioner + Year, data = data, method = "logistic")

logit_ord5 <- polr(increase_rsa_ordered ~ Xenophobia + Women + Age + Married + Diploma + Occupation 
  + Public + Private + Independent + Boss + IncomeBrackets + Unioner + Year, data = data, method = "logistic")

stargazer(logit_ord4, type = "text")
summary(data$pmore_pension)

x1 <- ocME(logit_ord1)
x1$out
stargazer(x1$out$ME.1, type = "text")

x2 <- ocME(logit_ord2)
x2$out
stargazer(x2$out$ME.1, type = "text")

x3 <- ocME(logit_ord3)
x3$out
stargazer(x3$out$ME.1, type = "text")

x4 <- ocME(logit_ord4)
x4$out
stargazer(x4$out$ME.1, type = "text")

x5 <- ocME(logit_ord5)
x5$out
stargazer(x5$out$ME.1, type = "text")

stargazer(
  x1$out$ME.1,
  x2$out$ME.1,
  x3$out$ME.1,
  x4$out$ME.1,
  type = "text"
)

PseudoR2(logit_ord1, which = "CoxSnell")
PseudoR2(logit_ord2, which = "CoxSnell")
PseudoR2(logit_ord3, which = "CoxSnell")
PseudoR2(logit_ord4, which = "CoxSnell")
PseudoR2(logit_ord5, which = "CoxSnell")









#C) Finite Mixture Regression Models (Attempts)

data_clean <- data[, c("pmore_unemployed", "Xenophobia", "Women", "Age", "Married", "Diploma",
                "Occupation", "Public", "Private", "Independent", "Boss",
                "IncomeBrackets", "Unioner", "Year")]  

data_clean <- na.omit(data_clean)

blindmodel <- stepFlexmix(pmore_unemployed ~ Xenophobia,
  k = c(1, 2, 3, 4, 5),
  nrep = 10,
  data = data_clean,
  control = list(iter.max = 500))

blindmodel
plot(blindmodel) 

bestmodel <- getModel(blindmodel, which = 2) 
summary(bestmodel) 
parameters(bestmodel)
summary(refit(bestmodel)) 

summary(data_clean$Xenophobia)
tapply(data_clean$Xenophobia, data_clean$pmore_unemployed, var)

data_clean$Xeno_std <- scale(data_clean$Xenophobia)
summary(data_clean$Xeno_std)
bestmodel <- stepFlexmix(pmore_unemployed ~ Xeno_std, data = data_clean, 
                         control = list(iter.max = 500), k = 4, nrep = 10)

blindmodel <- stepFlexmix(pmore_unemployed ~ Xeno_std,
  k = c(1, 2, 3, 4, 5),
  nrep = 10,
  data = data_clean,
  control = list(iter.max = 500))


car::vif(lm(pmore_health_insurance ~ Xenophobia + Women + Age + Married + Diploma + 
  Occupation + Public + Private + Independent + Boss + IncomeBrackets + Unioner + 
  Year, data = data_clean))

library(mixtools)

fit <- regmixEM(y = data_clean$pmore_health_insurance,
  x = data_clean[, c("Xenophobia", "Women", "Age", "Married", 
              "Diploma", "Occupation", "IncomeBrackets", 
              "Unioner", "Year")],
  k = 2)


install.packages("RobMixReg")

library(RobMixReg)

m1 <- lm(pmore_unemployed ~ Xenophobia, data = data_clean)


mixfit(m1, ncomp = 2)

mixfit()



fit <- MLM(
  formula = pmore_unemployed ~ Xenophobia + Women + Age + Married + Diploma +  
            Occupation + IncomeBrackets + Unioner + Year,
  nc = 2,       # Number of components
  x = x, 
  y = y, 
)

y <- data_clean$pmore_unemployed
x <- data_clean[, c("Xenophobia", "Women", "Age", "Married", "Diploma", 
                    "Occupation", "Public", "Private", "Independent", "Boss",
                    "IncomeBrackets", "Unioner", "Year")]

 data_clean$Married <- as.factor(data_clean$Married)
data_clean$Occupation <- as.factor(data_clean$Occupation)
data_clean$Public <- as.factor(data_clean$Public)
data_clean$Private <- as.factor(data_clean$Private)
data_clean$Independent <- as.factor(data_clean$Independent)
data_clean$Boss <- as.factor(data_clean$Boss)
data_clean$IncomeBrackets <- as.factor(data_clean$IncomeBrackets)
data_clean$Unioner <- as.factor(data_clean$Unioner)
data_clean$Year <- as.factor(data_clean$Year)
