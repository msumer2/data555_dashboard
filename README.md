# CARE DREAMS — Malawi Evaluation Dashboard
**Author:** Mia Sumera  
**Program:** CARE International, Malawi — DREAMS (Diversifying Resilience Approaches through Market Systems in Emergency)
**Program Period:** October 2024 (Baseline) – January 2026 (Endline)
**Target Districts:** Chiradzulu, Phalombe, Thyolo

---

## Dashboard

**View the dashboard here:** https://msumer2.github.io/data555_dashboard/DREAMS_Dashboard.html

---

## About

This dashboard presents endline findings from the CARE DREAMS program evaluation, examining whether the addition of a Supplementary Nutrition Assistance (SNA) top-up to Unconditional Food Assistance (UFA) improved nutrition outcomes among women of reproductive age (WRA, 15–49) and households with children aged 6–23 months in southern Malawi.

---

## Outcomes

| Outcome | Description | Model |
|---------|-------------|-------|
| FCS | Food Consumption Score | Linear regression |
| MDD-W | Minimum Dietary Diversity — Women 15–49 | Logistic regression |
| MDD-C | Minimum Dietary Diversity — Children 6–23m | Firth penalized logistic regression |

---

## Data

Data were collected by CARE Malawi's MEAL team via KoboToolbox at baseline (October 2024) and endline (January 2026) across three districts in southern Malawi. The dataset is **confidential** and not publicly available.

---

## Real-World Impact

These findings provide rigorous endline evidence that integrating a Supplementary Nutrition Assistance (SNA) top-up with Unconditional Food Assistance (UFA) meaningfully improves household food security and dietary diversity in southern Malawi. Results directly inform USAID/BHA investment decisions and CARE Malawi's targeting strategy for future nutrition-sensitive programming for women of reproductive age, pregnant and lactating women, and children under two.

---

## How to Run
> **Note:** Source data are confidential and not included in this repository.
> To reproduce the dashboard, contact CARE Malawi's MEAL team for data access.

1. Clone the repository
2. Run scripts in order:

```r
source("code/00_data_prep.R")
source("code/01_regression.R")
source("code/02_summary.R")
source("code/03_figures.R")
source("code/04_map.R")
source("code/05_assist_table.R")
source("code/06_reg_table.R")
```
4. Knit `DREAMS_Dashboard.Rmd`

---

## Source Code

https://github.com/msumer2/data555_dashboard