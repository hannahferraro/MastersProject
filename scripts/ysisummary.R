# Ferraro Masters Thesis
# creating a new dataframe with ysi data averages, se's, sd's and saving it to the outputs folder for further use

# load libraries
library(tidyverse)
library(readxl)
library(dplyr)

ysiog <- read_xlsx("data/YSI.xlsx") %>% 
  mutate(SITE = factor(SITE)) %>%
  mutate(TIME = factor(TIME))

ysi_summary <- ysiog %>%
  group_by(SITE, TIME) %>%
  summarize(
    across(
      c(TEMP, SALINITY, DO, pH, TURBIDITY),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE),
        se   = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# write as csv
write.csv(
 ysi_summary,
  file = "outputs/ysi_summary.csv",
  row.names = FALSE
)