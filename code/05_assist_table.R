library(here)
library(tidyverse)
library(highcharter)
library(DT)

here::i_am("code/05_assist_table.R")

samples <- readRDS(here("output/samples.rds"))

# Assistance ———————————————————————————————————————
assist_summary <- samples$endline %>%
  summarise(
    Food = sum(food_assist == 1, na.rm = TRUE),
    Agriculture = sum(ag_assist == 1, na.rm = TRUE),
    Nutrition = sum(nutrition_assist == 1, na.rm = TRUE),
    Protection = sum(protection_assist == 1, na.rm = TRUE),
    CCT = sum(cct_assist == 1, na.rm = TRUE),
    UCT = sum(uct_assist == 1, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "type", values_to = "n") %>%
  mutate(pct = round(n / nrow(samples$endline) * 100, 1))

# Table —————————————————————————————————————————————
assist_table <- assist_summary %>%
  arrange(desc(n)) %>%
  mutate(pct = paste0(pct, "%")) %>%
  rename(
    `Assistance Type` = type,
    `Households (n)` = n,
    `% Receiving` = pct
  ) %>%
  datatable(
    rownames = FALSE,
    caption  = htmltools::tags$caption(
      style = "caption-side: bottom; text-align: left; font-size: 11px; color: #666;",
      "CCT = Conditional Cash Transfer (cash for work). UCT = Unconditional Cash Transfer."
    ),
    options = list(
      dom = "t",
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    ),
    class = "compact stripe hover"
  )

# Save RDS ———————————————————————————————————————————

saveRDS(assist_table, here("output/assist_table.rds"))

