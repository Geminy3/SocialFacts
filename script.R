library(openxlsx)
library(dplyr)
library(graphPAF) 
library(averisk) 
library(MASS) 
library(gtsummary)
library(gt)
library(ggplot2)
library(margins)
library(ggeffects)
library(modelbased)
library(rcompanion)
library(stringr)
library(car)
#IMPORT
test <- read.csv2("./2023_24/En cours/Julie Thomas - Danse & Genre/Etudes stat/lil-1620.csv/Csv/enpps2020_indiv_fpr.csv")
#tmp$weightsENPPS <- test$POIDS_CATIWIPapier
tmp <- read.xlsx("~/Documents/0 - Projet/2025_26/En cours/PAF/PAF_article_25juin2025.xlsx", sheet = "Feuil1")
#tmp$weights <- 1/tmp$weights ## ?

# NETTOYAGE TABLEAU
tmp %>%
  #filter(MB_PUB_OU_PRI != 'nc') %>%
  mutate(
    #MB_PUB_OU_PRI = factor(ifelse(MB_PUB_OU_PRI == "non", 0, 1), levels = c(0, 1)),
    MB_PUB_OU_PRI = factor(
      ifelse(MB_PUB_OU_PRI %in% c("non", "nc"), "Non", "Oui"), 
      levels = c("Non", "Oui")
      ),
    POSSOC.plus = case_when(
      POSSOC.plus == 1 ~ "Ouvrier/employé",
      POSSOC.plus == 2 ~ "Prof. intermédiaire", 
      POSSOC.plus == 3 ~ "Cadre et Prof. Intel et Sup"
    ),
    POSSOC.plus = factor(POSSOC.plus, levels = c("Ouvrier/employé", "Prof. intermédiaire", "Cadre et Prof. Intel et Sup")), 
    AGE.jeune = case_when(
      AGE.jeune == 1 ~ "65 +",
      AGE.jeune == 2 ~ "50-64", 
      AGE.jeune == 3 ~ "40-49", 
      AGE.jeune == 4 ~ "25-39", 
      AGE.jeune == 5 ~ "15-24", 
    ),
    #AGE.jeune = as.character(ifelse(AGE.jeune != 1, 1, 0)),
    AGE = factor(AGE.jeune, levels = c("65 +", "50-64", "40-49", "25-39", "15-24")),
    SEXE = factor(ifelse(SEXE.hom == 1, "Homme", "Femme"), levels = c("Homme", "Femme")),
    TUU.urbain = case_when(
      TUU.urbain == 1 ~ "Rural", 
      TUU.urbain == 2 ~ "2.000 à 99.999 hab", 
      TUU.urbain == 3 ~ "100.000 hab et +"
    ),
    TUU = factor(TUU.urbain, levels = c("Rural", "2.000 à 99.999 hab", "100.000 hab et +")), # Voir les niveaux de la TUU
    NIVIE.ok = case_when(
      NIVIE.ok == 1 ~ 'Difficile',
      NIVIE.ok == 2 ~ 'Juste',
      NIVIE.ok == 3 ~ "Ça va & à l'aise"
    ),
    NIVIE = factor(NIVIE.ok, levels = c('Difficile', 'Juste', "Ça va & à l'aise")),
    DIP.sup = case_when(
      DIP.sup == 1 ~ "Sous le bac",
      DIP.sup == 2 ~ "Bac & équiv",
      DIP.sup == 3 ~ "Bac+2",
      DIP.sup == 4 ~ "Licence et +"
    ),
    DIP = factor(DIP.sup, levels = c("Sous le bac", "Bac & équiv", "Bac+2", "Licence et +")),
    SANTE.bon = case_when(
      SANTE.bon == 1 ~ "Mauvaise et très mauvaise",
      SANTE.bon == 2 ~ "Assez bonne",
      SANTE.bon == 3 ~ "Bonne et très bonne",
    ),
    SANTE = factor(SANTE.bon, 
                       levels = c("Mauvaise et très mauvaise", "Assez bonne", "Bonne et très bonne")), 
    IMMI.fr = case_when(
      IMMI.fr == 1 ~ "Nat. étranger",
      IMMI.fr == 2 ~ "Nat. fr acquise",
      IMMI.fr == 3 ~ "Nat. fr naissance",
    ),
    IMMI = as.factor(IMMI.fr),
    MERsp = as.factor(MERsp),
    PERsp = as.factor(PERsp)
  ) %>%
  select(-SEXE.hom, -AGE.jeune, -TUU.urbain, -NIVIE.ok, -DIP.sup, -IMMI.fr, -SANTE.bon, -IMMI) %>%
  rename(PRATI_CLUB = MB_PUB_OU_PRI) %>%
  #rename(AGE = AGE.jeune) %>%
  #rename(NIVEAU_VIE = NIVIE.ok) %>%
  #rename(DIPLOME = DIP.sup) %>%
  rename(POS.SOCIALE = POSSOC.plus) -> tmp

source("~/Documents/0 - Projet/2025_26/En cours/PAF/utils.R")

# TRI A PLAT
tmp %>%
  tbl_summary(
    include = POS.SOCIALE, 
    digits = list(
      all_categorical() ~ c(0, 1)
    )
  ) %>%
  add_ci(
    style_fun = everything() ~ purrr::partial(style_percent, digit = 1)
  ) %>%
  #add_p() %>%
  as_gt() %>%
  tab_header(title = "Distribution des classes sociales dans l'échantillon", 
             subtitle = "Avec intervalle de confiance") %>%
  tab_footnote(footnote = "Source : ENPPS 2020")
  

# TABLEAU DE CONTINGENCE
tmp %>%
  tbl_summary(
    by = POS.SOCIALE,
    include = PRATI_CLUB, 
    digits = list(
      all_categorical() ~ c(0, 1)
    )
  ) %>%
  add_ci(
    style_fun = everything() ~ purrr::partial(style_percent, digit = 1)
  ) %>%
  add_p() %>%
  as_gt() %>%
  tab_header(title = "Pratique d'un sport en club selon la classe sociale", 
             subtitle = "Avec intervalle de confiance") %>%
  tab_footnote(footnote = "Source : ENPPS 2020")

tmp %>%
  tbl_summary(
    by = POS.SOCIALE,
    include = NIVIE, 
    digits = list(
      all_categorical() ~ c(0, 1)
    )
  ) %>%
  add_ci(
    style_fun = everything() ~ purrr::partial(style_percent, digit = 2)
  ) %>%
  add_p() %>%
  as_gt() %>%
  tab_header(title = "Niveau de vie perçu en fonction de la position sociale", 
             subtitle = "Avec intervalle de confiance") %>%
  tab_footnote(footnote = "Source : ENPPS 2020")

# Khi 2 / V de Cramer

name_var <- names(tmp %>% select(-PRATI_CLUB, -weights, -ski_alp))
get_cramer_tab(var = name_var, metric = 'cramer', data = tmp)

# ODDS.RATIO

odds <- glm(PRATI_CLUB ~ POS.SOCIALE, family = binomial(logit), data = tmp)
odds %>%
  tbl_regression(exponentiate = T) %>%
  as_gt() %>%
  tab_header(title = "Pratique d'un sport en club selon la classe sociale", 
             subtitle = "Odds.ratio") %>%
  tab_footnote(footnote = "Source : ENPPS 2020")

reg <- glm(PRATI_CLUB ~ POS.SOCIALE+AGE+SEXE+TUU+NIVIE+DIP+SANTE+MERsp+PERsp,  
           family = binomial(logit), 
           weights = weights, ## On utilise ici les poids pour pondérer les résultats. 
           data = tmp)

summary(reg)

vif(reg)
step.model <- reg %>% stepAIC(trace = F)
summary(step.model)

odds_tab <- tbl_regression(step.model, exponentiate = T) %>%
  as_gt() %>%
  tab_header(title = "Pratique d'un sport en club selon les variables du modèle",
            subtitle = "Odds.ratio") %>%
  tab_footnote(footnote = "Source : ENPPS 2020")

odds_tab %>%
  gtsave(filename = "~/Desktop/PAF/Graph&Tab/Odds_ratio_club_modele.png")

# AME
marg <- margins(step.model)
summary(marg) 

marg <- step.model %>%
  gtsummary::tbl_regression(
    tidy_fun = broom.helpers::tidy_avg_slopes,
    estimate_fun = scales::label_percent(
      accuracy = 0.1,
      style_positive = "plus"
      )
  ) %>%
  as_gt() %>%
  gt::tab_header(title = "Average Marginal Effect") %>%
  gt::tab_footnote(footnote = "Source : ENPPS 2020")

marg

marg %>%
  gtsave(filename = "~/Desktop/PAF/Graph&Tab/AME_club.png")

result <- margins(step.model, variables = c("POS.SOCIALE"), 
                  at = list(TUU = c("Rural", "2.000 à 99.999 hab", "100.000 hab et +")))
res <- summary(result) %>%
  mutate(
    POS.SOCIALE = str_remove(factor, pattern = '^POS.SOCIALE'),
    across(.cols = c("AME", 'SE', "z", "lower", "upper"), 
           .fns = function(x) round(x, 2)
           ),
    p = ifelse(p < 0.0001, "< 0.0001", round(p, 4))
    ) %>%
  relocate(POS.SOCIALE) %>%
  select(-factor) %>%
  gt::gt() %>%
  gt::tab_header(title = "Effets marginaux", 
                 subtitle = "Sur la TUU, et la position sociale") %>%
  gt::tab_footnote(footnote = "Source : ENPPS 2020")

res %>%
  gtsave("~/Desktop/PAF/Graph&Tab/AME_club_POSSOC_TUU.png")

# AAF 

data <- Get_AF(step.model, 
               name_var = c('POS.SOCIALE', 'AGE', 'SEXE', 'TUU', 
                                        'NIVIE', 'DIP', 'SANTE', 'MERsp', 
                                        'PERsp'), 
               vars_dep = 'PRATI_CLUB', 
               nbootstrap = 50, 
               data = tmp)

AAF.res <- data$res %>% filter(position %in% c("Average", "Joint")) %>%
  arrange(desc(est)) %>%
  mutate(
    across(.cols = c("est", 'norm_lower', 'norm_upper'), 
           .fns = function(x) round(x, 2)),
    across(.cols = c("bias", 'debiased_est'), 
           .fns = function(x) round(x, 4))
  ) %>%
  gt::gt() %>%
  tab_header(title = "Average Attributable Fraction", 
             subtitle = "sur la pratique d'un sport en club") %>%
  tab_footnote(footnote = "Source : ENPPS 2020")

AAF.res

AAF.res %>%
  gtsave(filename = "~/Desktop/PAF/Graph&Tab/AAF_club.png")

### 200 bootstraps

data200bstp <- Get_AF(step.model, 
               name_var = c('POS.SOCIALE', 'AGE', 'SEXE', 'TUU', 
                            'NIVIE', 'DIP', 'SANTE', 'IMMI', 'MERsp', 
                            'PERsp'), 
               vars_dep = 'PRATI_CLUB', 
               nbootstrap = 200,
               data = tmp)

AAF200.res <- data200bstp$res %>% filter(position %in% c("Average", "Joint")) %>%
  arrange(desc(est)) %>%
  mutate(
    across(.cols = c("est", 'norm_lower', 'norm_upper'), 
           .fns = function(x) round(x, 2)),
    across(.cols = c("bias", 'debiased_est'), 
           .fns = function(x) round(x, 4))
  ) %>%
  gt::gt() %>%
  tab_header(title = "Average Attributable Fraction", 
             subtitle = "sur la pratique d'un sport en club") %>%
  tab_footnote(footnote = "Source : ENPPS 2020")

AAF200.res

AAF200.res %>%
  gtsave(filename = "~/Desktop/PAF/Graph&Tab/AAF_club_200bootstrap.png")

## Specific PAF
test <- read.xlsx("~/Desktop/PAF/PAF_article_25juin2025.xlsx", sheet = "Feuil1")
test <- test %>%
  mutate(
    PRATI_CLUB = ifelse(MB_PUB_OU_PRI == "non", 0, 1), 
    POSSOC.plus = as.factor(POSSOC.plus)
  ) %>%
  select(-MB_PUB_OU_PRI)

str(test)

reg <- glm(PRATI_CLUB ~ POSSOC.plus+AGE.jeune+SEXE.hom+TUU.urbain+NIVIE.ok+DIP.sup+SANTE.bon+MERsp+PERsp,  
           family = binomial(logit), 
           weights = weights, ## On utilise ici les poids pour pondérer les résultats. 
           data = test)

res.PAF <- graphPAF::PAF_calc_discrete(model = reg, riskfactor = "POSSOC.plus", refval = 1, 
                            data = test, ci = T, boot_rep = 50)

# BIG TAB
source("~/Desktop/PAF/utils.R")
tab.recap <- export_tab(step.model, var_ref = 'PRATI_CLUB', res_AAF = data$res, data = tmp)
tab.recap
tab.recap %>%
  gtsave(filename = "~/Desktop/PAF/Graph&Tab/TAB_RECAP.png", expand = 10)