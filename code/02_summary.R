library(tidyverse)
library(here)

here::i_am("code/02_summary.R")

samples <- readRDS(here("output/samples.rds"))

# FCS (%) —————————————————————————————————————————————————
fcs_cat_pcts <- function(df, group_label) {
  fcs <- df$fcs_score[!is.na(df$fcs_score)]
  n <- length(fcs)
  tibble(
    Cat = group_label,
    n = n,
    Poor = round(sum(fcs <= 21) / n * 100, 1),
    Borderline = round(sum(fcs > 21 & fcs <= 35) / n * 100, 1),
    Acceptable = round(sum(fcs > 35) / n * 100, 1)
  )
}

cat_wide <- bind_rows(
  fcs_cat_pcts(samples$fcs_bl, "Baseline"),
  fcs_cat_pcts(samples$fcs_ufa, "UFA only"),
  fcs_cat_pcts(samples$fcs_sna, "UFA+SNA")
) %>%
  mutate(Cat = factor(Cat, levels = c("Baseline", "UFA only", "UFA+SNA")))

cat_long <- cat_wide %>%
  pivot_longer(
    cols = c(Poor, Borderline, Acceptable),
    names_to  = "Category",
    values_to = "Pct"
  ) %>%
  mutate(Category = factor(Category, levels = c("Poor", "Borderline", "Acceptable"))) %>%
  left_join(cat_wide %>% select(Cat, n_total = n), by = "Cat")

# FCS District Means ——————————————————————————————————————————
dist_fcs <- bind_rows(
  samples$fcs_bl %>%
    group_by(district) %>%
    summarise(mean_fcs = round(mean(fcs_score, na.rm = TRUE), 1), .groups = "drop") %>%
    mutate(Cat = "Baseline"),
  samples$fcs_ufa %>%
    group_by(district) %>%
    summarise(mean_fcs = round(mean(fcs_score, na.rm = TRUE), 1), .groups = "drop") %>%
    mutate(Cat = "UFA only"),
  samples$fcs_sna %>%
    group_by(district) %>%
    summarise(mean_fcs = round(mean(fcs_score, na.rm = TRUE), 1), .groups = "drop") %>%
    mutate(Cat = "UFA+SNA")
) %>%
  mutate(Cat = factor(Cat, levels = c("Baseline", "UFA only", "UFA+SNA")))


# MDD Prevalance —————————————————————————————————————————————
mdd_prev <- bind_rows(
  # MDD-W
  samples$mddw_ufa %>%
    summarise(
      outcome = "MDD-W",
      group = "UFA only",
      n = n(),
      prev = round(mean(mdd_w_cat, na.rm = TRUE) * 100, 1)
    ),
  samples$mddw_sna %>%
    summarise(
      outcome = "MDD-W",
      group = "UFA+SNA",
      n = n(),
      prev = round(mean(mdd_w_cat, na.rm = TRUE) * 100, 1)
    ),
  # MDD-C
  samples$mddc_ufa %>%
    summarise(
      outcome = "MDD-C",
      group = "UFA only",
      n = n(),
      prev = round(mean(mdd_c_cat, na.rm = TRUE) * 100, 1)
    ),
  samples$mddc_sna %>%
    summarise(
      outcome = "MDD-C",
      group = "UFA+SNA",
      n = n(),
      prev = round(mean(mdd_c_cat, na.rm = TRUE) * 100, 1)
    )
)

# Save ————————————————————————————————————————————————
saveRDS(
  list(
    cat_wide = cat_wide,
    cat_long = cat_long,
    dist_fcs = dist_fcs,
    mdd_prev = mdd_prev
  ),
  here("output/summary_data.rds")
)

