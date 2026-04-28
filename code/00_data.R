library(tidyverse)
library(here)

# Prep Function 
recode <- function(df) {
  df %>%
    mutate(
      district = factor(district, levels = c(1, 2, 3),
                        labels = c("Chiradzulu", "Phalombe", "Thyolo")),
      
      sex = factor(sex, levels = c(1, 2),
                   labels = c("Female", "Male")),
      
      marital_status = factor(marital_status,
                              levels = c(1, 2, 3, 4, 5),
                              labels = c("Single", "Married", "Separated",
                                         "Divorced", "Widowed")),
      marital_status = relevel(marital_status, ref = "Married"),
      
      education = factor(education,
                         levels = c(1, 2, 3, 4, 5, 6),
                         labels = c("None", "Std 1-4", "Std 5-8",
                                    "Form 1-4 and above", "Form 1-4 and above",
                                    "Form 1-4 and above")),
      education = relevel(education, ref = "None"),
      
      hh_status = case_when(
        hh_status == 1 ~ "Adult female and adult male",
        hh_status == 2 ~ "Adult female, no adult male",
        hh_status %in% c(3, 4) ~ "Other",
        TRUE ~ NA_character_
      ),
      hh_status = factor(hh_status,
                         levels = c("Adult female and adult male",
                                    "Adult female, no adult male",
                                    "Other")),
      hh_status = relevel(hh_status, ref = "Adult female and adult male"),
      
      sex_hh_head = factor(sex, levels = c("Female", "Male"))
    )
}

# Load Data
baseline <- read.csv("code/bl_26.csv") %>% recode()

endline <- read.csv("code/el_26.csv") %>%
  recode() %>%
  mutate(
    intervention = case_when(
      food_assist == 1 & uct_assist == 1 & nutrition_assist == 0 ~ 0,  # UFA only
      food_assist == 1 & uct_assist == 1 & nutrition_assist == 1 ~ 1,  # UFA + SNA
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(intervention))

el_ufa <- endline %>% filter(intervention == 0)
el_sna <- endline %>% filter(intervention == 1)

# —————————————————————————————————————————————————

# Analytic Sample Subgroups

# MDD-W: female respondents aged 15-49 (WRA)
mddw_bl  <- baseline %>% filter(sex == "Female", age >= 15, age <= 49, !is.na(mdd_w_score))
mddw_ufa <- el_ufa   %>% filter(sex == "Female", age >= 15, age <= 49, !is.na(mdd_w_score))
mddw_sna <- el_sna   %>% filter(sex == "Female", age >= 15, age <= 49, !is.na(mdd_w_score))

# MDD-C: households with child 6-23 months (mdd_c_score non-null by design)
mddc_bl  <- baseline %>% filter(!is.na(mdd_c_score))
mddc_ufa <- el_ufa   %>% filter(!is.na(mdd_c_score))
mddc_sna <- el_sna   %>% filter(!is.na(mdd_c_score))

# Overall FCS: WRA (15-49) OR household with child 6-23m
overall_sample <- function(df) {
  df %>% filter(
    (sex == "Female" & age >= 15 & age <= 49) | !is.na(mdd_c_score)
  )
}

fcs_bl  <- overall_sample(baseline)
fcs_ufa <- overall_sample(el_ufa)
fcs_sna <- overall_sample(el_sna)

fcs_data  <- bind_rows(fcs_ufa,  fcs_sna)  %>% droplevels()
mddw_data <- bind_rows(mddw_ufa, mddw_sna) %>% droplevels()
mddc_data <- bind_rows(mddc_ufa, mddc_sna) %>% droplevels()

# —————————————————————————————————————————————————

# Sample Size Summary
cat("Baseline: total =", nrow(baseline),
    "| FCS restricted =", nrow(fcs_bl), "\n")
cat("UFA only: total =", nrow(el_ufa),
    "| FCS restricted =", nrow(fcs_ufa), "\n")
cat("UFA+SNA: total =", nrow(el_sna),
    "| FCS restricted =", nrow(fcs_sna), "\n")
cat("MDD-W: BL =", nrow(mddw_bl),
    "| UFA =", nrow(mddw_ufa),
    "| SNA =", nrow(mddw_sna), "\n")
cat("MDD-C: BL =", nrow(mddc_bl),
    "| UFA =", nrow(mddc_ufa),
    "| SNA =", nrow(mddc_sna), "\n")

# —————————————————————————————————————————————————

# Save RDS 
saveRDS(
  list(
    baseline  = baseline,
    endline   = endline,
    el_ufa    = el_ufa,
    el_sna    = el_sna,
    fcs_bl    = fcs_bl,
    fcs_ufa   = fcs_ufa,
    fcs_sna   = fcs_sna,
    mddw_bl   = mddw_bl,
    mddw_ufa  = mddw_ufa,
    mddw_sna  = mddw_sna,
    mddc_bl   = mddc_bl,
    mddc_ufa  = mddc_ufa,
    mddc_sna  = mddc_sna,
    fcs_data  = fcs_data,
    mddw_data = mddw_data,
    mddc_data = mddc_data
  ),
  here("output/samples.rds")
)

