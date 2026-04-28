library(highcharter)
library(here)
library(tidyverse)

here::i_am("code/04_map.R")

samples <- readRDS(here("output/samples.rds"))

malawi_map <- download_map_data("countries/mw/mw-all")

# —————————————————————————————————————————————————

# I looked up how to do this out of curiosity. Used Claude to debug initial
# attempts of trying high charter. Mainly helped with the tool tips formatting.

chir_bl <- nrow(samples$baseline[samples$baseline$district == "Chiradzulu", ])
phal_bl <- nrow(samples$baseline[samples$baseline$district == "Phalombe", ])
thy_bl  <- nrow(samples$baseline[samples$baseline$district == "Thyolo", ])

chir_el <- nrow(samples$endline[samples$endline$district == "Chiradzulu", ])
phal_el <- nrow(samples$endline[samples$endline$district == "Phalombe", ])
thy_el  <- nrow(samples$endline[samples$endline$district == "Thyolo", ])



program_districts <- data.frame(
  name = c("Chiradzulu", "Phalombe", "Thyolo"),
  n_baseline = c(chir_bl, phal_bl, thy_bl),
  n_endline = c(chir_el, phal_el, thy_el)
) %>%
  mutate(tooltip_text = paste0("Baseline n: ", n_baseline,
                               "<br>Endline n: ", n_endline))


care_map <- highchart() %>%
  hc_add_series_map(
    map = malawi_map,
    df = program_districts,
    value = "n_baseline",
    joinBy = c("name", "name")
  ) %>%
  hc_colorAxis(
    minColor = "#fde8d4",
    maxColor = "#C45E0A"
  ) %>%
  hc_legend(
    enabled = TRUE,
    title   = list(text = "Survey Respondents (n)"),
    align   = "left",
    layout  = "vertical"
  ) %>%
  hc_title(text = "") %>%
  hc_subtitle(text = "") %>%
  hc_tooltip(
    useHTML = TRUE,
    formatter = JS("function() {
      return '<b>' + this.point.name + '</b><br>' +
             this.point.tooltip_text;
    }")
  ) %>%
  hc_credits(
    enabled = TRUE,
    text = "Baseline: October 2024 | Endline: January 2026",
    style = list(fontSize = "11px")
  ) %>%
  hc_mapNavigation(enabled = TRUE)

saveRDS(care_map, file = here::here("output/care_map.rds"))