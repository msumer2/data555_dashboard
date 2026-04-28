library(tidyverse)
library(here)
library(DT)

here::i_am("code/06_reg_table.R")

# Effect Estimates (Hard Coded from Poster) —————————————————————
reg_df <- data.frame(
  `Outcome` = c(
    "Food Consumption Score (FCS)",
    "MDD-W (Women 15\u201349)\u00b9",
    "MDD-C (Children 6\u201323m)\u00b2"
  ),
  `Crude Effect (p-value)` = c(
    "\u03b2 = 3.69 (p = 0.05)",
    "OR = 1.36 (p = 0.34)",
    "OR = 1.29 (p = 0.49)"
  ),
  `Adjusted Effect (p-value)` = c(
    "\u03b2 = 4.38 (p = 0.03)",
    "OR = 1.12 (p = 0.75)",
    "OR = 1.29 (p = 0.52)"
  ),
  `Significant` = c("Yes", "No", "No"),
  check.names = FALSE
)

# Function to Highlight Rows in Dashboard
# Helper to build table with a highlighted row
make_reg_table <- function(highlight_row) {
  datatable(
    reg_df,
    rownames = FALSE,
    caption  = htmltools::tags$caption(
      style = "caption-side: bottom; text-align: left; font-size: 11px; 
      color: #666; padding-top: 6px;",
      "\u03b2 = unstandardized regression coefficient. OR = odds ratio. ",
      "\u00b9 MDD-W: WRA only (15\u201349), sex of HH head excluded. ",
      "\u00b2 MDD-C: Firth penalized logistic regression due to small sample size (n=43)."
    ),
    options  = list(
      dom        = "t",
      columnDefs = list(
        list(className = "dt-left",   targets = 0),
        list(className = "dt-center", targets = 1:2),
        list(width = "40%", targets = 0)
      )
    ),
    class = "display compact"
  ) %>%
    formatStyle(
      "Outcome",
      target = "row",
      backgroundColor = styleEqual(
        reg_df$Outcome[highlight_row],
        "#fff3cd"
      )
    )
}


reg_table_fcs  <- make_reg_table(1) # FCS
reg_table_mddw <- make_reg_table(2) # MDD-W
reg_table_mddc <- make_reg_table(3) # MDD-C

# Save RDS ————————————————————————————————————————————————————————
saveRDS(
  list(
    reg_table_fcs  = reg_table_fcs,
    reg_table_mddw = reg_table_mddw,
    reg_table_mddc = reg_table_mddc
  ),
  here("output/reg_table.rds")
)
