# Ferraro Masters Thesis
# combining scripts to one, sed data, leafsum, and quadrat points percent cover

# load libraries
library(tidyverse)

# load data

leafsum <- read_csv("outputs/summary_leaf.csv") %>% 
  mutate(TIME = factor(TIME)) %>%
  mutate(QUADRAT = factor(QUADRAT))

ysisum <- read_csv("outputs/ysi_summary.csv")%>%
  mutate(TIME = factor(TIME)) %>%
  mutate(Site = factor(SITE))

sedog <- read_xlsx("data/seddataR.xlsx")  %>% 
  mutate(TIME = factor(TIME)) %>%
  mutate(QUADRAT = factor(QUADRAT)) %>% 
  mutate(IDNUMBERS = factor(IDNUMBERS)) %>%
  mutate(SAMPLEID = factor(SAMPLEID))

quad <- read_xlsx("data/quadratcover.xlsx") %>% 
  mutate(TIME = factor(TIME)) %>%
  mutate(QUADRAT = factor(QUADRAT)) %>%
  mutate(SITE.HABITAT.QUADRAT = factor(SITE.HABITAT.QUADRAT)) %>%
  select(SITE, TIME, QUADRAT, PERCENTCOVER)

# Join sed + leafsum + quadrat percent cover + ysi
sed <- sedog %>%
  left_join(leafsum, by = c("SITE", "TIME", "QUADRAT")) %>%
  left_join(quad, by = c("SITE", "TIME", "QUADRAT"), 
  relationship = "many-to-many") %>%
  left_join(ysisum, by =c("SITE", "TIME"),
  relationship = "many-to-many")

# write as csv
write.csv(
  sed,
  file = "outputs/completedata.csv",
  row.names = FALSE
)