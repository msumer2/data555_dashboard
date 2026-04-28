library(tidyverse)
library(here)
library(DT)
library(highcharter)

here::i_am("code/03_figures.R")

samples <- readRDS(here("output/samples.rds"))
regressions  <- readRDS(here("output/regressions.rds"))
sum_data <- readRDS(here("output/summary_data.rds"))

fmt_p <- regressions$fmt_p

# Assistance Bar Chart —————————————————————————————————————————————
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
assist_bar <- assist_summary %>%
  arrange(desc(n)) %>%
  mutate(
    pct = round(n / nrow(samples$endline) * 100, 1),
    tooltip = paste0(type, "<br>n = ", n, " (", pct, "%)")
  ) %>%
  hchart("bar", hcaes(x = type, y = pct)) %>%
  hc_colors("#C45E0A") %>%
  hc_xAxis(title = list(text = " ")) %>%
  hc_yAxis(
    title = list(text = "% of Households"),
    labels = list(format = "{value}%"),
    max = 100
  ) %>%
  hc_tooltip(
    headerFormat = "",
    pointFormat = "{point.tooltip}"
  ) %>%
  hc_add_theme(hc_theme_google())

# FCS Bar Chart —————————————————————————————————————————————————————
group_cols <- c("Baseline" = "#D6D4CF", "UFA only" = "#C45E0A60", "UFA+SNA" = "#C45E0A")
cdf_cols <- c("Baseline" = "#aaaaaa", "UFA only" = "#502A50", "UFA+SNA" = "#C45E0A")

cat_long_fig <- sum_data$cat_long %>%
  mutate(
    group = factor(Cat, levels = c("UFA+SNA", "UFA only", "Baseline"),
                   labels = c("Unconditional Food Assistance + Nutrition Top-up (UFA+SNA)",
                   "Unconditional Food Assistance (UFA only)",
                   "Baseline")),
    category = factor(Category, levels = c("Acceptable", "Borderline", "Poor")),
    pct = round(Pct, 1),
    tooltip  = paste0(
      "<b>Program Arm:</b> ", Cat, "<br>",
      "<b>Category:</b> ", Category, "<br>",
      "<b>Percentage:</b> ", round(Pct, 1), "%<br>",
      "<b>n:</b> ", n_total
    )
  )

fcs_cat <- hchart(
  cat_long_fig,
  "bar",
  hcaes(x = category, y = pct, group = group)
) %>%
  
  hc_colors(c("#C45E0A", "#C45E0A60", "#D6D4CF")) %>%
  
  hc_xAxis(title = list(text = "FCS Category")) %>%
  hc_yAxis(
    title = list(text = "% of Households"),
    labels = list(format = "{value}%"),
    max = 70
    
  ) %>%
  hc_title(text = "FCS Category Distribution by Program Arm") %>%
  hc_subtitle(text = "WFP thresholds: Poor ≤21 | Borderline 22–35 | Acceptable >35") %>%
 
   hc_tooltip(
    useHTML = TRUE,
    headerFormat = "",
    pointFormat = paste0(
      "<b>Program Arm:</b> {series.name}<br>",
      "<b>Percentage:</b> {point.y}%"
    )
  ) %>%
  hc_credits(
    enabled = TRUE,
    text    = paste0("Baseline n=", nrow(samples$fcs_bl),
                     " | UFA only n=", nrow(samples$fcs_ufa),
                     " | UFA+SNA n=", nrow(samples$fcs_sna),
                     ". Sample: Households including WRA or children 6–23m."),
    style   = list(fontSize = "10px")
  ) %>%
  
  hc_plotOptions(bar = list(
    grouping = TRUE,
    groupPadding = 0.1,
    pointPadding = 0.05
  )) %>%
  
  hc_legend(
    enabled = TRUE,
    layout = "horizontal",
    align = "left",
    verticalAlign = "top",
    reversed = TRUE
  ) %>%
  
  hc_add_theme(hc_theme_google())

# Save RDS ————————————————————————————————————————————————————————————————————————
saveRDS(
  list(
    fcs_cat = fcs_cat,
    assist_bar = assist_bar
  ),
  here("output/figures.rds")
)
