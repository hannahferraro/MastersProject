# Ferraro Masters Thesis
# creating a new dataframe with leaf length average, se, sd and saving it to the outputs folder for further use

# load libraries
library(tidyverse)
library(readxl)
library(dplyr)

#load data 
leafog <- read_xlsx("data/leaflength.xlsx") %>% 
  mutate(TIME = factor(TIME)) %>%
  mutate(QUADRAT = factor(QUADRAT))

# pivot longer to then group by and produce mean, sd, and se for leaf lenghts for each site, time and quadrat
summary_leaf <- leafog %>%
  pivot_longer(
    cols = leaf1:leaf5,
    names_to  = "leaf",
    values_to = "leaf_length"
  ) %>%
  group_by(SITE, TIME, QUADRAT) %>%
  summarize(
    mean_leaf = mean(leaf_length, na.rm = TRUE),
    sd_leaf   = sd(leaf_length, na.rm = TRUE),
    n_leaf    = sum(!is.na(leaf_length)),
    se_leaf   = sd_leaf / sqrt(n_leaf),
    .groups   = "drop"
  )

# save to outputs folder
write.csv(
  summary_leaf,
  file = "outputs/summary_leaf.csv",
  row.names = FALSE
)
