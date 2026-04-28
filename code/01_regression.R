library(tidyverse)
library(here)
library(broom)
library(logistf)

here::i_am("code/01_regression.R")

# Load Data
samples <- readRDS(here("output/samples.rds"))

# Helper Functions
fmt_p <- function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))

display <- function(x) { # format labels for dashboard display
  x %>%
    gsub("^intervention$","Intervention (UFA+SNA vs UFA only)", .) %>%
    gsub("^age$", "Age", .) %>%
    gsub("^marital_status", "Marital Status: ", .) %>%
    gsub("^education", "Education: ", .) %>%
    gsub("^hh_status", "Household Status: ", .) %>%
    gsub("^hh_size$", "Household Size", .) %>%
    gsub("^sex_hh_headMale$", "Sex of HH Head: Male (ref = Female)", .) %>%
    gsub("^district", "District: ", .) %>%
    gsub("^\\(Intercept\\)$", "Intercept", .)
}

# —————————————————————————————————————————————————

# FCS — Adjusted Linear Regression
m_fcs <- lm(
  fcs_score ~ intervention + age + marital_status + education +
    hh_status + hh_size + sex_hh_head + district,
  data = samples$fcs_data
)

# Intervention (UFA+SNA)
fcs_int <- tidy(m_fcs) %>%
  filter(term == "intervention") %>%
  mutate(
    ci_lower = estimate - 1.96 * std.error,
    ci_upper = estimate + 1.96 * std.error
  )

cat(sprintf("  FCS β = %.2f [%.2f, %.2f], p = %s\n",
            fcs_int$estimate, fcs_int$ci_lower, fcs_int$ci_upper,
            fmt_p(fcs_int$p.value)))

# Full Table
fcs_tbl <- as.data.frame(summary(m_fcs)$coefficients) %>%
  rownames_to_column("term") %>%
  rename(estimate = Estimate, std.error = `Std. Error`,
         statistic = `t value`, p.value = `Pr(>|t|)`) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Term = display(term),
    `β` = round(estimate, 2),
    SE = round(std.error, 2),
    t = round(statistic, 2),
    `p-value` = fmt_p(p.value),
    sig = p.value < 0.05
  ) %>%
  select(Term, `β`, SE, t, `p-value`, sig)

# —————————————————————————————————————————————————

# MDD-W — Adjusted Logistic Regression (female-only subgroup, sex_hh_head excluded)
m_mddw <- glm(
  mdd_w_cat ~ intervention + age + marital_status + education +
    hh_status + hh_size + district,
  data = samples$mddw_data,
  family = binomial(link = "logit")
)

# Intervention (UFA+SNA)
mddw_int <- tidy(m_mddw) %>%
  filter(term == "intervention") %>%
  mutate(
    OR = exp(estimate),
    ci_lower = exp(estimate - 1.96 * std.error),
    ci_upper = exp(estimate + 1.96 * std.error)
  )

cat(sprintf("  MDD-W OR = %.2f [%.2f, %.2f], p = %s\n",
            mddw_int$OR, mddw_int$ci_lower, mddw_int$ci_upper,
            fmt_p(mddw_int$p.value)))

# Full Table
mddw_tbl <- tidy(m_mddw) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Term = display(term),
    OR  = round(exp(estimate), 2),
    CI_lower = round(exp(estimate - 1.96 * std.error), 2),
    CI_upper = round(exp(estimate + 1.96 * std.error), 2),
    `95% CI` = paste0(CI_lower, ", ", CI_upper),
    `p-value` = fmt_p(p.value),
    sig = p.value < 0.05
  ) %>%
  select(Term, OR, `95% CI`, `p-value`, sig)

# —————————————————————————————————————————————————

# MDD-C — Firth Penalized Logistic Regression (small sample & sparse cells)
m_mddc <- logistf(
  mdd_c_cat ~ intervention + age + marital_status + education +
    hh_status + hh_size + sex_hh_head + district,
  data = samples$mddc_data
)

# Coefficients
mddc_coefs <- data.frame(
  term = names(coef(m_mddc)),
  estimate = coef(m_mddc),
  std.error = sqrt(diag(vcov(m_mddc))),
  p.value = m_mddc$prob,
  row.names = NULL
)

# Intervention (UFA+SNA)
mddc_int <- mddc_coefs %>%
  filter(term == "intervention") %>%
  mutate(
    OR = exp(estimate),
    ci_lower = exp(estimate - 1.96 * std.error),
    ci_upper = exp(estimate + 1.96 * std.error)
  )

cat(sprintf("  MDD-C OR = %.2f [%.2f, %.2f], p = %s\n",
            mddc_int$OR, mddc_int$ci_lower, mddc_int$ci_upper,
            fmt_p(mddc_int$p.value)))

# Full Table
mddc_tbl <- mddc_coefs %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Term = display(term),
    OR = round(exp(estimate), 2),
    CI_lower = round(exp(estimate - 1.96 * std.error), 2),
    CI_upper = round(exp(estimate + 1.96 * std.error), 2),
    `95% CI`  = paste0(CI_lower, ", ", CI_upper),
    `p-value` = fmt_p(p.value),
    sig = p.value < 0.05
  ) %>%
  select(Term, OR, `95% CI`, `p-value`, sig)

# —————————————————————————————————————————————————

# Save RDS
saveRDS(
  list(
    # model objects 
    m_fcs    = m_fcs,
    m_mddw   = m_mddw,
    m_mddc   = m_mddc,
    # intervention summaries
    fcs_int  = fcs_int,
    mddw_int = mddw_int,
    mddc_int = mddc_int,
    # full tables
    fcs_tbl  = fcs_tbl,
    mddw_tbl = mddw_tbl,
    mddc_tbl = mddc_tbl,
    # shared helper
    fmt_p    = fmt_p
  ),
  here("output/regressions.rds")
)