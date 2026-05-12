# Title: Whose Merit, Which Redistribution? Appendix
# This script contains all code necessary to replicate the appendix.
# Author: Lívio Silva-Muller
# Date: March 2026

# 0 Packages, help functions, and data------------------------------------------

library(tidyverse)
library(scales)
library(psych)
library(misty)
library(sandwich)
library(stargazer)
library(patchwork)
library(pwr)
library(modelsummary)
library(tibble)
library(car)
library(broom)
library(officer)
library(flextable)
library(lmtest)
library(ggcorrplot)

merit_dat <- readRDS("merit_dat.rds")

br <- merit_dat %>%
  filter(country == "Brazil")

sa <- merit_dat %>%
  filter(country == "South Africa")

# Figures A1 & A2 Correlation between Items---------------------------------------------------

# Variables creating each index (renamed for clarity)
giving_items <- c(
  basic_income_d = "dist_binc_1", basic_income_v = "dist_binc_2",
  cash_transfer_d = "dist_ct__1", cash_transfer_v = "dist_ct__2",
  pension_d = "dist_reti_1", pension_v = "dist_reti_2"
)

taking_items <- c(
  profit_tax_d = "dist_tax2_1", profit_tax_v = "dist_tax2_2",
  wealth_tax_d = "dist_tax4_1", wealth_tax_v = "dist_tax4_2",
  capital_tax_d = "dist_tax3_1", capital_tax_v = "dist_tax3_2",
  income_tax_d = "dist_tax1_1", income_tax_v = "dist_tax1_2"
)

# Function to compute tetrachoric correlations
plot_corr_no_title <- function(data, vars, facet_subtitle) {
  dat_sel <- data %>% select(all_of(vars)) %>% drop_na()
  names(dat_sel) <- names(vars)
  
  cor_mat <- tetrachoric(dat_sel)$rho
  
  ggcorrplot(cor_mat,
             method = "circle",
             type = "lower",
             lab = TRUE,
             lab_size = 3,
             colors = c("#d73027", "white", "#1a9850"),
             ggtheme = theme_minimal()) +
    labs(subtitle = facet_subtitle) +
    theme(
      panel.background = element_rect("white", "black", 0.5, "solid"),
      panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
      axis.text = element_text(color = "black", size = 10),
      plot.subtitle = element_text(color = "black", size = 12, face = "bold"),
      legend.position = "top",  # legend above
      legend.title = element_blank(),
      legend.text = element_text(size = 10)
    )
}

# Brazil
br <- merit_dat %>% filter(country == "Brazil")
br_giving <- plot_corr_no_title(br, giving_items, "Giving Income Items")
br_taking <- plot_corr_no_title(br, taking_items, "Taking Income Items")

brazil_panel <- (br_giving / br_taking) +
  plot_layout(guides = "collect") &  # shared legend
  plot_annotation(
    title = "Correlation Matrices for Brazil",
    subtitle = "_d indicates desirability and _v indicates viability",
    caption = "Correlations are tetrachoric, appropriate for binary (dummy) variables.",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, face = "plain"),
      plot.caption = element_text(size = 10, face = "italic")
    )
  )

# South Africa
sa <- merit_dat %>% filter(country == "South Africa")
sa_giving <- plot_corr_no_title(sa, giving_items, "Giving Income Items")
sa_taking <- plot_corr_no_title(sa, taking_items, "Taking Income Items")

sa_panel <- (sa_giving / sa_taking) +
  plot_layout(guides = "collect") &
  plot_annotation(
    title = "Correlation Matrices for South Africa",
    subtitle = "_d indicates desirability and _v indicates viability",
    caption = "Correlations are tetrachoric, appropriate for binary (dummy) variables.",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, face = "plain"),
      plot.caption = element_text(size = 10, face = "italic")))

# Tables A3 & A4  Models separated by viability and desirability-----------------------

B_des_g <- lm(giving_des2 ~ hardworking_elite + effortless_poor + sector + racial_identity +
                political_identity + participant_sex, data = br)
B_via_g <- lm(giving_via2 ~ hardworking_elite + effortless_poor + sector + racial_identity +
                political_identity + participant_sex, data = br)
B_des_t <- lm(taking_des2 ~ hardworking_elite + effortless_poor + sector + racial_identity +
                political_identity + participant_sex, data = br)
B_via_t <- lm(taking_via2 ~ hardworking_elite + effortless_poor + sector + racial_identity +
                political_identity + participant_sex, data = br)
S_des_g <- lm(giving_des2 ~ hardworking_elite + effortless_poor + sector + racial_identity +
                political_identity + participant_sex, data = sa)
S_via_g <- lm(giving_via2 ~ hardworking_elite + effortless_poor + sector + racial_identity +
                political_identity + participant_sex, data = sa)
S_des_t <- lm(taking_des2 ~ hardworking_elite + effortless_poor + sector + racial_identity +
                political_identity + participant_sex, data = sa)
S_via_t <- lm(taking_via2 ~ hardworking_elite + effortless_poor + sector + racial_identity +
                political_identity + participant_sex, data = sa)

robust_se_B_des_g <- sqrt(diag(vcovHC(B_des_g, type = "HC1")))
robust_se_B_via_g <- sqrt(diag(vcovHC(B_via_g, type = "HC1")))
robust_se_B_des_t <- sqrt(diag(vcovHC(B_des_t, type = "HC1")))
robust_se_B_via_t <- sqrt(diag(vcovHC(B_via_t, type = "HC1")))
robust_se_S_des_g <- sqrt(diag(vcovHC(S_des_g, type = "HC1")))
robust_se_S_via_g <- sqrt(diag(vcovHC(S_via_g, type = "HC1")))
robust_se_S_des_t <- sqrt(diag(vcovHC(S_des_t, type = "HC1")))
robust_se_S_via_t <- sqrt(diag(vcovHC(S_via_t, type = "HC1")))

stargazer(
  B_des_g, B_via_g, B_des_t, B_via_t,
  type = "html",
  title = "Brazil: Desirability vs Viability (Full Models)",
  se = list(
    robust_se_B_des_g,
    robust_se_B_via_g,
    robust_se_B_des_t,
    robust_se_B_via_t
  ),
  column.labels = c("Giving (Des)", "Giving (Via)", 
                    "Taking (Des)", "Taking (Via)"),
  omit.stat = c("f", "ser"),
  star.cutoffs = c(0.1, 0.05, 0.01),
  model.names = FALSE,
  no.space = TRUE,
  out="br_models_separated.doc")

stargazer(
  S_des_g, S_via_g, S_des_t, S_via_t,
  type = "html",
  title = "South Africa: Desirability vs Viability (Full Models)",
  se = list(
    robust_se_S_des_g,
    robust_se_S_via_g,
    robust_se_S_des_t,
    robust_se_S_via_t
  ),
  column.labels = c("Giving (Des)", "Giving (Via)", 
                    "Taking (Des)", "Taking (Via)"),
  omit.stat = c("f", "ser"),
  star.cutoffs = c(0.1, 0.05, 0.01),
  model.names = FALSE,
  no.space = TRUE,
  out="sa_models_separated.doc")

# A5 Descriptive Statistics-----------------------------------------------------

dat <- merit_dat %>%
  filter(country %in% c("Brazil","South Africa")) %>%
  mutate(
    participant_sex = ifelse(participant_sex == 1, "Female",
                             ifelse(participant_sex == 0, "Male", NA)),
    racial_identity = ifelse(racial_identity == 1, "White",
                   ifelse(racial_identity == 0, "Not White", NA)),
    participant_sex = factor(participant_sex, levels = c("Male","Female")),
    racial_identity = factor(racial_identity, levels = c("White","Not White"))
  ) %>%
  select(country, giving_cash2, taking_cash2, hardworking_elite, effortless_poor,
         political_identity, sector, racial_identity, participant_sex)

ft <- datasummary_balance(~ country, data = dat,
                          fmt = '%.3f', output = "flextable")

save_as_docx("Descriptives by Country — Main Model Vars" = ft,
             path = "Descriptives_byCountry.docx")
# A6 Model Diagnostics---------------------------------------------------------

B_a <- lm(giving_cash2~ hardworking_elite+effortless_poor, data=br)
B_b <- lm(taking_cash2~ hardworking_elite+effortless_poor, data=br)
B_c <- lm(giving_cash2~ hardworking_elite+effortless_poor+sector+racial_identity+
            political_identity+participant_sex, data=br)
B_d <- lm(taking_cash2~ hardworking_elite+effortless_poor+sector+racial_identity+
            political_identity+participant_sex, data=br)
S_a <- lm(giving_cash2~ hardworking_elite+effortless_poor, data=sa)
S_b <- lm(taking_cash2~ hardworking_elite+effortless_poor, data=sa)
S_c <- lm(giving_cash2~ hardworking_elite+effortless_poor+sector+racial_identity+
            political_identity+participant_sex, data=sa)
S_d <- lm(taking_cash2~ hardworking_elite+effortless_poor+sector+racial_identity+
            political_identity+participant_sex, data=sa)
s_m1 <- lm(effortless_poor~ political_identity+racial_identity+sector+participant_sex, data=sa)
b_m1 <- lm(effortless_poor~ political_identity+racial_identity+sector+participant_sex, data=br)
s_m2 <- lm(hardworking_elite~ political_identity+racial_identity+sector+participant_sex, data=sa)
b_m2 <- lm(hardworking_elite~ political_identity+racial_identity+sector+participant_sex, data=br)

models <- list(
  "SA: Giving (lean)"  = S_a,
  "SA: Taking (lean)"  = S_b,
  "SA: Giving (full)"  = S_c,
  "SA: Taking (full)"  = S_d,
  "BR: Giving (lean)"  = B_a,
  "BR: Taking (lean)"  = B_b,
  "BR: Giving (full)"  = B_c,
  "BR: Taking (full)"  = B_d,
  "SA: Poor Merit"  = s_m1,
  "BR: Poor Merit"  = b_m1,
  "SA: Elite Merit" = s_m2,
  "BR: Elite Merit" = b_m2)

safe_vif_max <- function(model) {
  out <- tryCatch({
    v <- car::vif(model)
    # car::vif returns either a vector or a matrix depending on terms
    if (is.matrix(v)) {
      max(v[, 1], na.rm = TRUE)
    } else {
      max(v, na.rm = TRUE)
    }
  }, error = function(e) NA_real_)
  out
}

# 3) Core diagnostics extractor
get_diagnostics <- function(model) {
  
  # Always use a non-conflicting name, and force numeric
  Nobs <- as.integer(stats::nobs(model))
  
  bp_p <- tryCatch(lmtest::bptest(model)$p.value, error = function(e) NA_real_)
  
  vif_max <- tryCatch({
    v <- car::vif(model)
    if (is.matrix(v)) max(v[, 1], na.rm = TRUE) else max(v, na.rm = TRUE)
  }, error = function(e) NA_real_)
  
  shapiro_p <- NA_real_
  # Shapiro only runs for 3..5000 AND only if Nobs is valid
  if (!is.na(Nobs) && is.finite(Nobs) && Nobs >= 3L && Nobs <= 5000L) {
    shapiro_p <- tryCatch(stats::shapiro.test(stats::residuals(model))$p.value,
                          error = function(e) NA_real_)
  }
  
  cooks_max <- max(stats::cooks.distance(model), na.rm = TRUE)
  
  tibble::tibble(
    N = Nobs,
    BP_Test_p = round(bp_p, 3),
    VIF_Max = round(vif_max, 3),
    Shapiro_p = ifelse(is.na(shapiro_p), NA, round(shapiro_p, 3)),
    Max_CooksD = round(cooks_max, 3)
  )
}

# 4) Build a diagnostics table for all models + simple flags
diag_table <- imap_dfr(models, ~ get_diagnostics(.x) %>% mutate(Model = .y)) %>%
  relocate(Model) %>%
  mutate(
    Flag_Heterosk = ifelse(!is.na(BP_Test_p) & BP_Test_p < 0.05, "yes", "no"),
    Flag_Collinear = ifelse(!is.na(VIF_Max) & VIF_Max >= 10, "yes", "no"),
    Flag_Influence = ifelse(!is.na(Max_CooksD) & Max_CooksD > (4 / N), "yes", "no")
  )

# 5) Export diagnostics table to Word
diag_flex <- flextable(diag_table) %>%
  autofit()

read_docx() %>%
  body_add_par("OLS Model Diagnostics", style = "heading 1") %>%
  body_add_flextable(diag_flex) %>%
  print(target = "Appendix_Table_Diagnostics.docx")

# # 6) Diagnostic plots per model (4-panel)
# plot_diagnostics <- function(model, model_name) {
#   df1 <- data.frame(fitted = fitted(model), resid = resid(model))
#   p1 <- ggplot(df1, aes(fitted, resid)) +
#     geom_point(alpha = 0.6) +
#     geom_hline(yintercept = 0, linetype = "dashed") +
#     labs(title = "Residuals vs Fitted", subtitle = model_name) +
#     theme_minimal()
#   
#   df2 <- data.frame(sample = resid(model))
#   p2 <- ggplot(df2, aes(sample = sample)) +
#     stat_qq() +
#     stat_qq_line() +
#     labs(title = "Normal Q-Q", subtitle = model_name) +
#     theme_minimal()
#   
#   df3 <- data.frame(fitted = fitted(model),
#                     sqrt_std_resid = sqrt(abs(rstandard(model))))
#   p3 <- ggplot(df3, aes(fitted, sqrt_std_resid)) +
#     geom_point(alpha = 0.6) +
#     geom_smooth(se = FALSE) +
#     labs(title = "Scale-Location", subtitle = model_name) +
#     theme_minimal()
#   
#   df4 <- data.frame(obs = seq_len(nobs(model)), cook = cooks.distance(model))
#   p4 <- ggplot(df4, aes(obs, cook)) +
#     geom_col(alpha = 0.6) +
#     labs(title = "Cook's Distance", subtitle = model_name) +
#     theme_minimal()
#   
#   (p1 | p2) / (p3 | p4)
# }
# 
# pdf("Appendix_Model_Diagnostics.pdf", width = 12, height = 10)
# walk(names(models), ~ print(plot_diagnostics(models[[.x]], .x)))
# dev.off()

# A7 SUR------------------------------------------------------------------------

library(systemfit)

min_giv <- giving_cash2 ~ hardworking_elite + effortless_poor
min_tak <- taking_cash2 ~ hardworking_elite + effortless_poor

br_sur_min <- systemfit(
  list(giv = min_giv,
       tak = min_tak),
  method = "SUR",
  data = br
)

sa_sur_min <- systemfit(
  list(giv = min_giv,
       tak = min_tak),
  method = "SUR",
  data = sa
)

# FULL SUR (joint index, giving + taking + controls)
eq_full_giv <- giving_cash2 ~ hardworking_elite + effortless_poor + sector + racial_identity +
  political_identity + participant_sex
eq_full_tak <- taking_cash2 ~ hardworking_elite + effortless_poor + sector + racial_identity +
  political_identity + participant_sex

br_sur_full <- systemfit(
  list(giv = eq_full_giv,
       tak = eq_full_tak),
  method = "SUR",
  data = br)

sa_sur_full <- systemfit(
  list(giv = eq_full_giv,
       tak = eq_full_tak),
  method = "SUR",
  data = sa)


make_coefs <- function(sur_min, sur_full, country_label) {
  
  ### --- Minimal SUR ---
  min_tidy <- tidy(sur_min, conf.int = TRUE)
  
  min_coefs <- min_tidy %>%
    mutate(
      dv = case_when(
        startsWith(term, "giv_") ~ "Giving (joint index)",
        startsWith(term, "tak_") ~ "Taking (joint index)",
        TRUE ~ NA_character_
      ),
      term_clean = sub("^(giv_|tak_)", "", term),
      
      # clean labels for *all* variables
      predictor = dplyr::recode(term_clean,
                                "hardworking_elite"      = "Hardworking Elites",
                                "effortless_poor"       = "Effortless Poor",
                                "sector"           = "Sector",
                                "white"            = "White",
                                "political_identity"         = "Ideology",
                                "participant_sex"  = "Sex",
                                .default = term_clean    # fallback for any variable name not listed
      )
    ) %>%
    transmute(
      predictor,
      dv,
      spec     = "Minimal",
      estimate = estimate,
      se       = std.error,
      ci_low   = conf.low,
      ci_high  = conf.high
    )
  
  
  ### --- Full SUR ---
  full_tidy <- tidy(sur_full, conf.int = TRUE)
  
  full_coefs <- full_tidy %>%
    mutate(
      dv = case_when(
        startsWith(term, "giv_") ~ "Giving (joint index)",
        startsWith(term, "tak_") ~ "Taking (joint index)",
        TRUE ~ NA_character_
      ),
      term_clean = sub("^(giv_|tak_)", "", term),
      
      predictor = dplyr::recode(term_clean,
                                "hardworking_elite"      = "Hard-working Elites",
                                "effortless_poor"       = "Effortless Poor",
                                "sector"           = "Sector",
                                "white"            = "White",
                                "political_identity"         = "Ideology",
                                "participant_sex"  = "Sex",
                                .default = term_clean
      )
    ) %>%
    transmute(
      predictor,
      dv,
      spec     = "Full",
      estimate = estimate,
      se       = std.error,
      ci_low   = conf.low,
      ci_high  = conf.high
    )
  
  bind_rows(min_coefs, full_coefs) %>%
    mutate(country = country_label)
}

br_coefs <- make_coefs(br_sur_min, br_sur_full, country_label = "Brazil")
sa_coefs <- make_coefs(sa_sur_min, sa_sur_full, country_label = "South Africa")

sur_table <- bind_rows(br_coefs, sa_coefs) %>%
  filter(spec == "Full") %>%              
  mutate(
    # two-sided p-value from z = estimate / se
    p_val = 2 * pnorm(-abs(estimate / se)),
    
    # significance stars
    stars = case_when(
      p_val < 0.001 ~ "***",
      p_val < 0.01  ~ "**",
      p_val < 0.05  ~ "*",
      p_val < 0.1  ~ "+",
      TRUE ~ ""
    ),
    
    # rounded p-value + stars
    p_label = paste0(formatC(p_val, digits = 3, format = "f"), " ", stars),
    
    country = factor(country, levels = c("Brazil", "South Africa")),
    dv = factor(
      dv,
      levels = c("Giving (joint index)", "Taking (joint index)"),
      labels = c("Giving Income", "Taking Income")
    )
  )


