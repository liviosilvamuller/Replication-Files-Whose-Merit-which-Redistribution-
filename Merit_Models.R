# Title: Whose Merit, Which Redistribution? Models
# This script contains all code necessary to replicate the models.
# Author: Lívio Silva-Muller
# Date: March 2026

# 0 Packages, help functions, and data------------------------------------------

library(tidyverse)
library(scales)
library(sandwich)
library(stargazer)
library(officer)
library(flextable)

merit_dat <- readRDS("merit_dat.rds")

# 1 Models Merits -> Redistribution-------------------------------------------

br <- merit_dat %>%
  dplyr::select(hardworking_elite, effortless_poor, sector, racial_identity, 
                giving_cash2, taking_cash2, political_identity, country, participant_sex)%>%
  filter(country == "Brazil")

sa <- merit_dat  %>%
  dplyr::select(hardworking_elite, effortless_poor, sector, racial_identity, 
                giving_cash2, taking_cash2, political_identity, country, participant_sex)%>%
  filter(country == "South Africa")

# Lean (without controls)
B_a <- lm(giving_cash2 ~ effortless_poor + hardworking_elite, data = br)
B_b <- lm(taking_cash2 ~ effortless_poor + hardworking_elite, data = br)
S_a <- lm(giving_cash2 ~ effortless_poor + hardworking_elite, data = sa)
S_b <- lm(taking_cash2 ~ effortless_poor + hardworking_elite, data = sa)

# Full (with controls)
B_c <- lm(giving_cash2 ~ effortless_poor + hardworking_elite +
            sector + racial_identity + political_identity + participant_sex,
          data = br)

B_d <- lm(taking_cash2 ~ effortless_poor + hardworking_elite +
            sector + racial_identity + political_identity + participant_sex,
          data = br)

S_c <- lm(giving_cash2 ~ effortless_poor + hardworking_elite +
            sector + racial_identity + political_identity + participant_sex,
          data = sa)

S_d <- lm(taking_cash2 ~ effortless_poor + hardworking_elite +
            sector + racial_identity + political_identity + participant_sex,
          data = sa)


robust_se_B_a <- sqrt(diag(vcovHC(B_a, type = "HC1")))
robust_se_B_b <- sqrt(diag(vcovHC(B_b, type = "HC1")))
robust_se_S_a <- sqrt(diag(vcovHC(S_a, type = "HC1")))
robust_se_S_b <- sqrt(diag(vcovHC(S_b, type = "HC1")))

robust_se_B_c <- sqrt(diag(vcovHC(B_c, type = "HC1")))
robust_se_B_d <- sqrt(diag(vcovHC(B_d, type = "HC1")))
robust_se_S_c <- sqrt(diag(vcovHC(S_c, type = "HC1")))
robust_se_S_d <- sqrt(diag(vcovHC(S_d, type = "HC1")))

stargazer(
  B_a, B_b, S_a, S_b,
  type = "html",
  title = "Preferences for transfers and taxation — lean models",
  se = list(robust_se_B_a, robust_se_B_b,
            robust_se_S_a, robust_se_S_b),
  covariate.labels = c("Effortless Poor",
                       "Hard-working Elites"),
  column.labels = c("Brazil", "Brazil",
                    "South Africa", "South Africa"),
  column.separate = c(2,2),
  omit.stat = c("f","ser"),
  keep.stat = c("n","rsq"),
  model.names = FALSE,
  no.space = TRUE,
  out = "table_lean_models.doc"
)

stargazer(
  B_c, B_d, S_c, S_d,
  type = "html",
  title = "Preferences for transfers and taxation with controls",
  se = list(robust_se_B_c, robust_se_B_d,
            robust_se_S_c, robust_se_S_d),
  column.labels = c("Brazil", "Brazil",
                    "South Africa", "South Africa"),
  column.separate = c(2,2),
  omit.stat = c("f","ser"),
  keep.stat = c("n","adj.rsq"),
  model.names = FALSE,
  no.space = TRUE,
  out = "table_full_models.doc"
)

# 2 Models Controls -> Merit----------------------------------------------------

s_m1 <- lm(effortless_poor~ political_identity+racial_identity+sector+participant_sex, data=sa)
b_m1 <- lm(effortless_poor~ political_identity+racial_identity+sector+participant_sex, data=br)
s_m2 <- lm(hardworking_elite~ political_identity+racial_identity+sector+participant_sex, data=sa)
b_m2 <- lm(hardworking_elite~ political_identity+racial_identity+sector+participant_sex, data=br)

robust_se_S_m1 <- sqrt(diag(vcovHC(s_m1, type = "HC1")))
robust_se_B_m1 <- sqrt(diag(vcovHC(b_m1, type = "HC1")))
robust_se_S_m2 <- sqrt(diag(vcovHC(s_m2, type = "HC1")))
robust_se_B_m2 <- sqrt(diag(vcovHC(b_m2, type = "HC1")))

stargazer(
  b_m1, b_m2, s_m1, s_m2,                    # Brazil then South Africa
  type = "html",
  title = "Predicting Merit Beliefs",
  se = list(robust_se_B_m1, robust_se_B_m2,
            robust_se_S_m1, robust_se_S_m2),
  column.labels = c("Brazil", "Brazil", "South Africa", "South Africa"),
  column.separate = c(2,2),
  keep.stat = c("n","adj.rsq"),
  omit.stat = c("f","ser"),
  digits = 3,
  align = TRUE,
  model.names = FALSE,
  no.space = TRUE,
  star.cutoffs = c(0.1, 0.05, 0.01),
  out = "merit_predictors.doc"
)
