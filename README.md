# Replication Package

## Whose Merit, which Redistribution? Elites, Taxes, and Transfers in Brazil and South Africa

**Authors:** Livio Silva-Muller, Graziella Moraes Silva, Matias López  
**Journal:** *Social Forces*  
**Manuscript ID:** Forthcoming, 2026

---

## Overview

This replication files contains all code and data necessary to reproduce the figures, tables, and appendix materials reported in the manuscript. The analyses are implemented in R and organized across three scripts.

---

## Data

| File | Description |
|------|-------------|
| `merit_dat.rds` | Processed survey dataset. Starting point for all analyses. Contains 318 elite respondents from Brazil (n = 163) and South Africa (n = 155), surveyed between 2021 and 2023. |

---

## Scripts

Run the scripts in the following order:

### 1. `Merit_Figures.R`
Reproduces all figures in the main manuscript.

| Output | Description |
|--------|-------------|
| `Figure1.png` | Scatterplot of elite vs. poor merit perceptions by country (Figure 1) |
| `Figure2.png` | Mean merit perceptions across elite sectors with 95% CIs (Figure 2) |
| `Figure3.png` | Racial composition of elite sectors by country (Figure 3) |

**Key packages:** `tidyverse`

---

### 2. `Merit_Models.R`
Reproduces all regression tables in the main manuscript.

| Output | Description |
|--------|-------------|
| `table_lean_models.doc` | OLS models without controls — giving and taking income (Table 2) |
| `table_full_models.doc` | OLS models with controls — giving and taking income (Table 3) |
| `merit_predictors.doc` | OLS models predicting merit perceptions from controls (Table 4) |

**Key packages:** `tidyverse`, `sandwich`, `stargazer`

---

### 3. `Merit_Appendix.R`
Reproduces all supplementary tables and figures.

| Output | Description |
|--------|-------------|
| `brazil_panel` | Tetrachoric correlation matrices for policy indices — Brazil (Figure A2) |
| `sa_panel` | Tetrachoric correlation matrices for policy indices — South Africa (Figure A3) |
| `br_models_separated.doc` | Brazil models with desirability and viability separated (Table A4) |
| `sa_models_separated.doc` | South Africa models with desirability and viability separated (Table A5) |
| `Descriptives_byCountry.docx` | Descriptive statistics by country (Table A6) |
| `Appendix_Table_Diagnostics.docx` | OLS model diagnostics including BP test, VIF, Shapiro, and Cook's D (Table A7) |
| SUR results | Seemingly Unrelated Regression estimates (Tables A8–A9) |

**Key packages:** `tidyverse`, `psych`, `ggcorrplot`, `patchwork`, `sandwich`, `stargazer`, `modelsummary`, `flextable`, `officer`, `car`, `broom`, `lmtest`, `systemfit`, `pwr`, `misty`

---

## Software and Package Versions

```
R version 4.2.2 (2022-10-31)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS 26.3.1

Key packages:
  tidyverse     2.0.0
  ggplot2       4.0.1
  dplyr         1.1.0
  tidyr         1.3.0
  scales        1.4.0
  psych         2.2.9
  ggcorrplot    0.1.4.1
  patchwork     1.1.3
  sandwich      3.0-2
  stargazer     5.2.3
  modelsummary  1.3.0
  flextable     0.9.10
  officer       0.7.0
  car           3.1-1
  broom         1.0.4
  lmtest        0.9-40
  systemfit     (see sessionInfo)
  pwr           1.3-0
  misty         0.4.11
```

---

## Contact

Livio Silva-Muller — livio_silvamueller@fas.harvard.edu
