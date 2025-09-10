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
library(stringr)
library(scales)
library(fixest)

# I) Dataset 

# A) DREES Surveys 

data00_13 <- read.csv2("Base 2000 - 2013/Csv/barodrees_unif00_13v8.csv") # Unified dataset 2000 - 2013 with departemental location

data <- read_dta("2023/barometre2000_2023_diff.dta") # Unified dataset 2000 - 2023

data00_16 <- read_dta("2016/baro_drees_unif0016_queteletv1.dta") # Unified dataset 2000 - 2016

# B) Departemental controls 

immi_data <- read_parquet("C:/Users/srimling/Documents/Positron/CREDOC/immi_data.parquet")
age_data <- read_parquet("C:/Users/srimling/Documents/Positron/CREDOC/age_data_dep.parquet")
csp_data <- read_parquet("C:/Users/srimling/Documents/Positron/CREDOC/csp_data_dep.parquet")
diploma_data <- read_parquet("C:/Users/srimling/Documents/Positron/CREDOC/diploma_data_dep.parquet")
income_data <- read_parquet("C:/Users/srimling/Documents/Positron/CREDOC/income_data_dep.parquet")
data_region <- read.csv("C:/Users/srimling/Documents/Positron/CREDOC/departements-region.csv", sep = ",", stringsAsFactors = FALSE)
female_data <- read_parquet("C:/Users/srimling/Documents/Positron/CREDOC/dep_female_data.parquet")
pop_data <- read_parquet("C:/Users/srimling/Documents/Positron/CREDOC/dep_pop_data.parquet")           # Weighting variable 

#II) Variables 

#A) Outcome variables

# Would you accept a reduction ... in exchange for a reduction in your taxes or payroll taxes ?

freq(data$ps13_ab_1)

data <- data %>%
  mutate(
    pless_health_insurance = ifelse(ps13_ab_1 %in% c(5, 999999999), NA, 5 - ps13_ab_1), # Health Insurance 
    pless_pension          = ifelse(ps13_ab_2 %in% c(5, 999999999), NA, 5 - ps13_ab_2), # Pension allowances
    pless_family           = ifelse(ps13_ab_3 %in% c(5, 999999999), NA, 5 - ps13_ab_3), # Family allowances
    pless_unemployed       = ifelse(ps13_ab_4 %in% c(5, 999999999), NA, 5 - ps13_ab_4), # Unemployment benefits
    pless_disabled         = ifelse(ps13_ab_5 %in% c(5, 999999999), NA, 5 - ps13_ab_5), # Help for disabled (since 2004)
    pless_dependent        = ifelse(ps13_ab_6 %in% c(5, 999999999), NA, 5 - ps13_ab_6),  # Help for dependent elderly people (since 2004)
    pless_housing          = ifelse(ps13_ab_7 %in% c(5, 999999999), NA, 5 - ps13_ab_7), # Housing benefits (since 2015)
  )                                                             
                                                                # 1: Not at all vs. 4: Fully Agree

freq(data$pless_health_insurance) # 2017 - 2023 
table(data$pless_health_insurance, data$annee) # 2017 - 2023 

freq(data00_16$ps13_1) # Include 2000 - 2016 waves

data00_16 <- data00_16 %>%
  mutate(
    pless_health_insurance = ifelse(ps13_1 %in% c(5, 6), NA, 5 - ps13_1), # Health Insurance 
    pless_pension          = ifelse(ps13_2 %in% c(5, 6), NA, 5 - ps13_2), # Pension allowances
    pless_family           = ifelse(ps13_3 %in% c(5, 6), NA, 5 - ps13_3), # Family allowances
    pless_unemployed       = ifelse(ps13_4 %in% c(5, 6), NA, 5 - ps13_4), # Unemployment benefits
    pless_disabled         = ifelse(ps13_5 %in% c(5, 6), NA, 5 - ps13_5), # Help for disabled (since 2004)
    pless_dependent        = ifelse(ps13_6 %in% c(5, 6), NA, 5 - ps13_6),  # Help for dependent elderly people (since 2004)
    pless_housing          = ifelse(ps13_7 %in% c(5, 6), NA, 5 - ps13_7), # Housing benefits (since 2015)
  )            

freq(data00_16$pless_health_insurance)


data00_16$ident <- as.factor(data00_16$ident)
data00_16$annee <- as.factor(data00_16$annee)

data$ident <- as.factor(data$ident)
data$annee <- as.factor(data$annee)

head(data$ident, 10)
head(data00_16$ident, 10)

data00_16 <- data00_16 %>%
  mutate(ident_harmonized = paste0("ID_", ident))

data <- data %>%
  left_join(
    data00_16 %>% dplyr::select(ident_harmonized, annee, starts_with("pless_")),
    by = c("ident" = "ident_harmonized", "annee" = "annee"),
    suffix = c("", "_update")
  ) %>%
  mutate(
    pless_health_insurance = case_when(
      !is.na(pless_health_insurance_update) ~ pless_health_insurance_update,
      TRUE ~ pless_health_insurance
    ),
    pless_pension = case_when(
      !is.na(pless_pension_update) ~ pless_pension_update,
      TRUE ~ pless_pension
    ),
    pless_family = case_when(
      !is.na(pless_family_update) ~ pless_family_update,
      TRUE ~ pless_family
    ),
    pless_unemployed = case_when(
      !is.na(pless_unemployed_update) ~ pless_unemployed_update,
      TRUE ~ pless_unemployed
    ),
    pless_disabled = case_when(
      !is.na(pless_disabled_update) ~ pless_disabled_update,
      TRUE ~ pless_disabled
    ),
    pless_dependent = case_when(
      !is.na(pless_dependent_update) ~ pless_dependent_update,
      TRUE ~ pless_dependent
    ),
    pless_housing = case_when(
      !is.na(pless_housing_update) ~ pless_housing_update,
      TRUE ~ pless_housing
    )
  ) %>%
  dplyr::select(-ends_with("_update"))

table(data$pless_health_insurance, data$annee) # Merging seems to have worked but high NAs rate (additional checking needed)

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

# Exclusive benefits: 1) Yes i.e only to those who pay payroll taxes (Bismarck) 2) No i.e only poorest one (Beveridge) 3) To everyone without distinction (Universal) cf. Esping-Andersen typology

freq(data$ps01_1) # Health care insurance
freq(data$ps01_2) # Pensions
freq(data$ps01_3) # Family
freq(data$ps01_4) # Unemployment

table(data$ps01_1, data$annee)
table(data$ps01_2, data$annee)
table(data$ps01_3, data$annee)
table(data$ps01_4, data$annee) # Huge increase for conditional unemployment benefits overtime


library(dplyr)

data <- data %>%
  mutate(
    # Calculer le score brut seulement si les 4 variables sont non manquantes
    raw_score = ifelse(
      !is.na(ps01_1) & !is.na(ps01_2) & !is.na(ps01_3) & !is.na(ps01_4),
      ifelse(ps01_1 == 1, 1, 0) +
      ifelse(ps01_2 == 1, 1, 0) +
      ifelse(ps01_3 == 1, 1, 0) +
      ifelse(ps01_4 == 1, 1, 0),
      NA_real_
    )
  ) %>%
  # Standardiser sur l'ensemble des données complètes
  mutate(
    exclusive_benefits = as.numeric(scale(raw_score))
  ) %>%
  dplyr::select(-raw_score)

# Vérifier les fréquences
freq(data$exclusive_benefits)


ggplot(data, aes(x = annee, y = exclusive_benefits)) +
  stat_summary(fun = "mean", geom = "point", size = 3, color = "steelblue") +
  labs(
    title = "Évolution des bénéfices exclusifs au cours du temps",
    x = "Année",
    y = "Score moyen des bénéfices exclusifs (0-100)"
  ) +
  theme_minimal()

data <- data %>%
  mutate(ws_health_insurance = case_when(
    ps01_1 == 1 ~ "Bismarck",
    ps01_1 == 2 ~ "Beveridge",
    ps01_1 == 3 ~ "Universal",
    ps01_1 == 4 ~ "Mixed",
    ps01_1 == 999999999 ~ NA_character_,
    TRUE ~ NA_character_
  ))

freq(data$ws_health_insurance)

ggplot(
  data %>% 
    filter(!is.na(ws_health_insurance)) %>%
    group_by(annee, ws_health_insurance) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(annee) %>%
    mutate(pct = n / sum(n)),
  aes(x = as.factor(annee), y = pct, color = ws_health_insurance, group = ws_health_insurance)
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Évolution des préférences pour les systèmes d’assurance maladie",
    x = "Année",
    y = "Pourcentage",
    color = "Type de système"
  ) +
  theme_minimal()

data <- data %>%
  mutate(ws_pension = case_when(
    ps01_2 == 1 ~ "Bismarck",
    ps01_2 == 2 ~ "Beveridge",
    ps01_2 == 3 ~ "Universal",
    ps01_2 == 4 ~ "Mixed",
    ps01_2 == 999999999 ~ NA_character_,
    TRUE ~ NA_character_
  ))

freq(data$ws_pension)

ggplot(
  data %>% 
    filter(!is.na(ws_pension)) %>%
    group_by(annee, ws_pension) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(annee) %>%
    mutate(pct = n / sum(n)),
  aes(x = as.factor(annee), y = pct, color = ws_pension, group = ws_pension)
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Évolution des préférences pour les système de retraite",
    x = "Année",
    y = "Pourcentage",
    color = "Type de système"
  ) +
  theme_minimal()

data <- data %>%
  mutate(ws_family = case_when(
    ps01_3 == 1 ~ "Bismarck",
    ps01_3 == 2 ~ "Beveridge",
    ps01_3 == 3 ~ "Universal",
    ps01_3 == 4 ~ "Mixed",
    ps01_3 == 999999999 ~ NA_character_,
    TRUE ~ NA_character_
  ))

freq(data$ws_family)

ggplot(
  data %>% 
    filter(!is.na(ws_family)) %>%
    group_by(annee, ws_family) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(annee) %>%
    mutate(pct = n / sum(n)),
  aes(x = as.factor(annee), y = pct, color = ws_family, group = ws_family)
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Évolution des préférences pour les prestations familiales",
    x = "Année",
    y = "Pourcentage",
    color = "Type de système"
  ) +
  theme_minimal()


data <- data %>%
  mutate(ws_unemployment = case_when(
    ps01_4 == 1 ~ "Bismarck",
    ps01_4 == 2 ~ "Beveridge",
    ps01_4 == 3 ~ "Universal",
    ps01_4 == 4 ~ "Mixed",
    ps01_4 == 999999999 ~ NA_character_,
    TRUE ~ NA_character_
  ))

ggplot(
  data %>% 
    filter(!is.na(ws_unemployment)) %>%
    group_by(annee, ws_unemployment) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(annee) %>%
    mutate(pct = n / sum(n)),
  aes(x = as.factor(annee), y = pct, color = ws_unemployment, group = ws_unemployment)
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Évolution des préférences pour les prestations familiales",
    x = "Année",
    y = "Pourcentage",
    color = "Type de système"
  ) +
  theme_minimal()

freq(data$ws_unemployment)

#B) Explanatory variables

freq(data$og08_1)

data <- data %>%   
  mutate(     
    xenophobia = case_when(       
      as.numeric(as.character(annee)) <= 2013 & og08_1 %in% c(2, 3) ~ og08_1,       
      as.numeric(as.character(annee)) >= 2014 & og08_1 %in% c(1, 2) ~ 2,       
      as.numeric(as.character(annee)) >= 2014 & og08_1 %in% c(3, 4) ~ 3,       
      og08_1 == 999999999 | is.na(og08_1) ~ NA_real_,       
      TRUE ~ NA_real_     
    )
  )                        # 4-points Likert scale is only since 2014 and onwards

data$xenophobia <- ifelse(data$xenophobia == 2, 1, 0)

freq(data$xenophobia)

table(data$xenophobia, data$annee)

ggplot(
  data %>%
    filter(!is.na(xenophobia)) %>%
    group_by(annee, xenophobia) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(annee) %>%
    mutate(pct = n / sum(n)),
  aes(x = as.factor(annee), y = pct, color = as.factor(xenophobia), group = xenophobia)
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(
    values = c("0" = "#1f77b4", "1" = "#d62728"),
    labels = c("0" = "Non xénophobe", "1" = "Xénophobe"),
    name = "Attitude"
  ) +
  labs(
    title = "Évolution de la xénophobie déclarée par année",
    x = "Année",
    y = "Pourcentage"
  ) +
  theme_minimal()

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

#III) Regression Analysis

# A) Linear Regression Models

data_reg <- data %>%
  dplyr::select(
    pless_health_insurance,
    pless_pension,
    pless_family,
    pless_unemployed,
    pless_disabled,
    pless_dependent,
    pless_housing,
    increasersa,
    increase_rsa,
    toomuchTforPS,
    xenophobia,
    women,
    age,
    married,
    diploma,
    occupation,
    unioner,
    income,
    year, 
    poids
  )

ols_1 <- lm(pless_unemployed ~ xenophobia + women + age + married +
                                        diploma + occupation + unioner + income + year, data = data_reg)
ols_2 <- lm(pless_health_insurance ~ xenophobia + women + age + married +
                                        diploma + occupation + unioner + income + year, data = data_reg)
ols_3 <- lm(pless_family ~ xenophobia + women + age + married +
                                        diploma + occupation + unioner + income + year, data = data_reg)
ols_4 <- lm(pless_disabled ~ xenophobia + women + age + married +
                                        diploma + occupation + unioner + income + year, data = data_reg)
ols_5 <- lm(pless_dependent ~ xenophobia + women + age + married +
                                        diploma + occupation + unioner + income + year, data = data_reg)
ols_6 <- lm(pless_housing ~ xenophobia + women + age + married +
                                        diploma + occupation + unioner + income + year, data = data_reg)
ols_7 <- lm(pless_pension ~ xenophobia + women + age + married +
                                        diploma + occupation + unioner + income + year, data = data_reg)

se_unemployed <- sqrt(diag(vcovHC(ols_1, type = "HC1")))
se_health_insurance <- sqrt(diag(vcovHC(ols_2, type = "HC1")))
se_family <- sqrt(diag(vcovHC(ols_3, type = "HC1")))
se_disabled <- sqrt(diag(vcovHC(ols_4, type = "HC1")))
se_dependent <- sqrt(diag(vcovHC(ols_5, type = "HC1")))
se_housing <- sqrt(diag(vcovHC(ols_6, type = "HC1")))
se_pension <- sqrt(diag(vcovHC(ols_7, type = "HC1")))

stargazer(
  ols_1, ols_2, ols_3, 
  ols_4, ols_5, ols_6, ols_7,
  se = list(se_unemployed, se_health_insurance, se_family, 
            se_disabled, se_dependent, se_housing, se_pension),
  type = "text",
  column.labels = c("Unemployed", "Health Insurance", "Family", 
                   "Disabled", "Dependent", "Housing", "Pension"),
  model.numbers = FALSE
)

# around 41k observations with regression analysis on 2000 - 2013. Now around 55k to 60k which looks weird !








# -------------------- 1st UNIFIED DATASET (2000 - 2013) --------------------

# In the future, given your level of resources,  would you be prepared to CONTRIBUTE MORE to maintain the level of benefits?

data00_13 <- data00_13 %>%
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

freq(data00_13$SEXE)

data00_13$Women <- ifelse(data00_13$SEXE == 2, 1, 0)

# Age

freq(data00_13$AGE_BR)

data00_13$Age <- as.numeric(data00_13$AGE_BR)

# Marital Status

freq(data00_13$SITFAM)

data00_13 <- data00_13 %>%
  mutate(Married = case_when(
    SITFAM == 1 ~ 1,
    SITFAM %in% c(2, 3, 4) ~ 0,
  ))

freq(data00_13$Married)

# Diploma 

freq(data00_13$Q111)

data00_13 <- data00_13 %>%
  mutate(Diploma = case_when(
    Q111 %in% c(1, 2) ~ "Low",
    Q111 %in% c(3, 4) ~ "Medium",
    Q111 %in% c(5, 6, 7, 8) ~ "High",
    TRUE ~ NA_character_
  ),
  Diploma = factor(Diploma, levels = c("Low", "Medium", "High")))

freq(data00_13$Diploma)

# Occupation

freq(data00_13$PPI)

data00_13 <- data00_13 %>%
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

freq(data00_13$Occupation)

# Employment

freq(data00_13$STATUTPPIbis)

data00_13 <- data00_13 %>%
  mutate(Employment = case_when(
    STATUTPPIbis == 1 ~ "Public",
    STATUTPPIbis == 2 ~ "Private",
    STATUTPPIbis == 3 ~ "Independent",
    STATUTPPIbis == 4 ~ "Employer",
    TRUE ~ NA_character_
  )) %>%
  mutate(Employment = factor(Employment, levels = c("Public", "Private", "Independent", "Employer")))

freq(data00_13$Employment) #45% de N.A ! 

data00_13 <- data00_13 %>%
  mutate(
    Public      = ifelse(STATUTPPIbis == 1, 1, 0),
    Private     = ifelse(STATUTPPIbis == 2, 1, 0),
    Independent = ifelse(STATUTPPIbis == 3, 1, 0),
    Boss        = ifelse(STATUTPPIbis == 4, 1, 0)
  )

freq(data00_13$Public)

# Unioner

data00_13 <- data00_13 %>%
  mutate(Unioner = case_when(
    Q103_1 == 1 ~ 1,
    Q103_1 == 2 ~ 0,
  ))

# Income Brackets

freq(data00_13$Q110BIS)

data00_13 <- data00_13 %>%
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

freq(data00_13$IncomeBrackets)

# Year 

data00_13$Year <- as.factor(data00_13$ANNEE)

freq(data00_13$Year)

# Departement

table(data00_13$DEPT, data00_13$Year) # Only data from 2000 to 2006 (with a break in 2003) and 2013

data00_13 <- subset(data00_13, Year %in% c(2000:2006, 2013))
data00_13$Year <- droplevels(data00_13$Year)

table(data00_13$DEPT, data00_13$Year)

data00_13$Departement <- as.factor(data00_13$DEPT)
data00_13$Departement <- sprintf("%02d", as.numeric(as.character(data00_13$Departement)))

freq(data00_13$Departement)


# D) Merging with control variables

data00_13 <- data00_13 %>%
  dplyr::select(
    IDENT, pmore_health_insurance, pmore_pension, pmore_family, pmore_unemployed,
    pmore_disabled, pmore_dependent, pmore_disabled, Women, Age, Married, Diploma,
    Occupation, Unioner, IncomeBrackets, Year, Departement
  )


immi_data$dep <- as.factor(immi_data$dep)
age_data$dep <- as.factor(age_data$dep)
csp_data$dep <- as.factor(csp_data$dep)
diploma_data$dep <- as.factor(diploma_data$dep)
income_data$dep <- as.factor(income_data$dep)

# Immigration 

immi_data <- immi_data %>%               # conversion from wide to long panel format
  pivot_longer(
    cols = starts_with("petranger"),
    names_to = "Year",
    names_prefix = "petranger",
    values_to = "ShareImmi"
  ) %>%
  mutate(
    Year = as.factor(Year)
  )

data00_13 <- data00_13 %>%
  mutate(Year = as.integer(as.character(Year))) %>%
  left_join(
    immi_data %>%
      mutate(
        Year = as.integer(as.character(Year)),
        Year_lag = Year + 1                            # t-1 to have a 1 year lagged value
      ) %>%
      dplyr::select(dep, Year_lag, ShareImmi),
    by = c("Departement" = "dep", "Year" = "Year_lag")
  ) %>%
  mutate(Year = as.factor(Year))

# Occupational share

data00_13 <- data00_13 %>%
  mutate(Year = as.integer(as.character(Year))) %>%
  left_join(
    csp_data %>%
      mutate(
        Year = as.integer(as.character(Year)),
        Year_lag = Year + 1
      ) %>%
      dplyr::select(dep, Year_lag, part_agri, part_indp, part_cadr, part_pint, part_empl, part_ouvr, part_chom),
    by = c("Departement" = "dep", "Year" = "Year_lag")
  )

# Income 

data00_13 <- data00_13 %>%
  mutate(Year = as.integer(as.character(Year))) %>%
  left_join(
    income_data %>%
      mutate(
        Year = as.integer(as.character(Year)),
        Year_lag = Year + 1
      ) %>%
      dplyr::select(dep, Year_lag, RevMoy),
    by = c("Departement" = "dep", "Year" = "Year_lag")
  ) %>%
  mutate(Year = as.factor(Year))  

# High dipp 

data00_13 <- data00_13 %>%
  mutate(Year = as.integer(as.character(Year))) %>%
  left_join(
    diploma_data %>%
      mutate(
        Year = as.integer(as.character(Year)),
        Year_lag = Year + 1
      ) %>%
      dplyr::select(dep, Year_lag, partbac),
    by = c("Departement" = "dep", "Year" = "Year_lag")
  )

data00_13 <- data00_13 %>%
  mutate(Year = as.integer(as.character(Year))) %>%
  left_join(
    female_data %>%
      mutate(
        Year = as.integer(as.character(Year)),
        Year_lag = Year + 1
      ) %>%
      dplyr::select(dep, Year_lag, FemaleShare),
    by = c("Departement" = "dep", "Year" = "Year_lag")
  ) %>%
  mutate(Year = as.factor(Year))

### Analysis 

colnames(data00_13)

data00_13 <- data00_13 %>%
  mutate(across(ShareImmi:FemaleShare, ~ .x / 100))

freq(data00_13$pmore_unemployed)

unemployment <- feols(pmore_unemployed ~ Women + Age + Married + Diploma + IncomeBrackets
                      + Unioner + ShareImmi + part_agri + part_indp + part_cadr + part_empl
                      + part_pint + part_ouvr + part_chom + partbac + FemaleShare + log(RevMoy)
                      + factor(Year) + factor(Departement), data = data00_13)

health <- feols(pmore_health_insurance ~ Women + Age + Married + Diploma + IncomeBrackets
                      + Unioner + ShareImmi + part_agri + part_indp + part_cadr + part_empl
                      + part_pint + part_ouvr + part_chom + partbac + FemaleShare + log(RevMoy)
                      + factor(Year) + factor(Departement), data = data00_13)

pension <- feols(pmore_pension ~ Women + Age + Married + Diploma + IncomeBrackets
                      + Unioner + ShareImmi + part_agri + part_indp + part_cadr + part_empl
                      + part_pint + part_ouvr + part_chom + partbac + FemaleShare + log(RevMoy)
                      + factor(Year) + factor(Departement), data = data00_13)

family <- feols(pmore_family ~ Women + Age + Married + Diploma + IncomeBrackets
                      + Unioner + ShareImmi + part_agri + part_indp + part_cadr + part_empl
                      + part_pint + part_ouvr + part_chom + partbac + FemaleShare + log(RevMoy)
                      + factor(Year) + factor(Departement), data = data00_13)

etable(health, pension, family, unemployment, keep = "ShareImmi")

























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
