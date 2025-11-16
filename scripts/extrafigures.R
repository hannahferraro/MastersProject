Sediment Carbon Density over time for *H. stipulacea* inside and Sand inside (all sites included)

```{r}
#| echo: false
#| warning: false
#| message: false
sed_box_filt <- sed |>
  mutate(
    TIME = factor(TIME),
    TREATMENT = factor(TREATMENT)
  ) |>
  filter(!TREATMENT %in% c("SO", "HO"))   # keep only HI, SI (or whatever’s left)

SCD_treat_time_box_noout <- ggplot(
  sed_box_filt,
  aes(
    x = TIME,
    y = SCD,
    fill = TREATMENT
  )
) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "Sampling period",
    y = "Sediment carbon density (mg C cm^-3)",
    fill = "Treatment"
  ) +
  theme_bw()

SCD_treat_time_box_noout
```

```{r}
#| echo: false
#| warning: false
#| message: false
plot_site_scd_box_hi_si <- function(site_code) {
  ggplot(
    filter(sed_box_filt, SITE == site_code),
    aes(
      x = TIME,
      y = SCD,
      fill = TREATMENT
    )
  ) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    labs(
      x = "Sampling period",
      y = "Sediment carbon density (mg C cm^-3)",
      fill = "Treatment"
    ) +
    theme_bw()
}

# three separate plots
SCD_BB_box_hi_si <- plot_site_scd_box_hi_si("BB")
SCD_FB_box_hi_si <- plot_site_scd_box_hi_si("FB")
SCD_LB_box_hi_si <- plot_site_scd_box_hi_si("LB")

sed_filt <- sed |>
  mutate(
    TIME = factor(TIME),
    TREATMENT = factor(TREATMENT)
  ) |>
  filter(!TREATMENT %in% c("SO", "HO"))   # drop SO and HO

# 2. Summarize: mean SCD + SE for each SITE × TREATMENT × TIME
SCD_means_site <- sed_filt |>
  group_by(SITE, TREATMENT, TIME) |>
  summarise(
    mean_SCD = mean(SCD, na.rm = TRUE),
    sd_SCD   = sd(SCD, na.rm = TRUE),
    n        = n(),
    se_SCD   = sd_SCD / sqrt(n),
    .groups  = "drop"
  )

# 3. Helper function to make line+SE plot for one site
plot_site_scd_line <- function(site_code) {
  ggplot(
    filter(SCD_means_site, SITE == site_code),
    aes(
      x = TIME,
      y = mean_SCD,
      color = TREATMENT,
      group = TREATMENT
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_errorbar(
      aes(
        ymin = mean_SCD - se_SCD,
        ymax = mean_SCD + se_SCD
      ),
      width = 0.1
    ) +
    labs(
      x = "Sampling period",
      y = "Mean sediment carbon density (mg C cm^-3)",
      color = "Treatment"
    ) +
    theme_bw()
}

# 4. Three separate plots
SCD_BB_line <- plot_site_scd_line("BB")
SCD_FB_line <- plot_site_scd_line("FB")
SCD_LB_line <- plot_site_scd_line("LB")
```

SCD over time at Brewers for *H. stipulacea* inside and Sand inside

```{r}
#| echo: false
#| warning: false
#| message: false
SCD_BB_box_hi_si
SCD_BB_line
```

SCD over time at Fortuna for *H. stipulacea* inside and Sand inside

```{r}
#| echo: false
#| warning: false
#| message: false
SCD_FB_box_hi_si
SCD_FB_line
```

SCD over time at Lindbergh for *H. stipulacea* inside and Sand inside

```{r}
#| echo: false
#| warning: false
#| message: false
SCD_LB_box_hi_si
SCD_LB_line
```

####### Trying to made tabs #######

## Sites

### Brewers Bay

### Sediment Carbon Density
```{r}
#| echo: false
# Brewers Bay SCD plots
SCD_BB
SCD_BB_box
SCD_BB_box_hi_si
SCD_BB_line

```

### Percent Cover

### Grain Size

## Lindbergh Bay

### Sediment Carbon Density
```{r}
#| echo: false
# Lindbergh Bay SCD plots
SCD_LB
SCD_LB_box
SCD_LB_box_hi_si
SCD_LB_line

```
### Percent Cover
### Grain Size

## Fortuna Bay

### Sediment Carbon Density
```{r}
#| echo: false
# Fortuna Bay SCD plots
SCD_FB
SCD_FB_box
SCD_FB_box_hi_si
SCD_FB_line

```
### Percent Cover
### Grain Size

########

## Attempting Statistics lol
```{r}
# make sure factors are set up (reuse sed from above)
sed_mod <- sed |>
  mutate(
    SITE      = factor(SITE),
    TIME      = factor(TIME),
    TREATMENT = factor(
      TREATMENT,
      levels = c("HI", "HO", "SI", "SO")  # adjust if needed
    )
  )

BB_T1 <- sed_mod |>
  filter(SITE == "BB", TIME == "1")

BB_T1_lm <- lm(SCD ~ TREATMENT, data = BB_T1)

# ANOVA table for treatment at Brewers, Time 1
Anova(BB_T1_lm, type = 3)

# Treatment means + pairwise tests
emm_BB_T1_direct <- emmeans(BB_T1_lm, ~ TREATMENT)
emm_BB_T1_direct
pairs(emm_BB_T1_direct, adjust = "tukey")

```

##########
# Percent Cover
##########
# Figure of percent cover with two sifferent plots together of site habitat and time point. the habitats are the two different plots

cover_summary <- cover %>%
  group_by(SITE, HABITAT, TIMEPT) %>%
  summarize(
    mean_cover = mean(PERCENTCOVER, na.rm = TRUE),
    sd_cover   = sd(PERCENTCOVER, na.rm = TRUE),
    n          = n(),
    se_cover   = sd_cover / sqrt(n),
    .groups    = "drop"
  )

ggplot(cover_summary,
       aes(x = TIMEPT,
           y = mean_cover,
           color = SITE,
           group = SITE)) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_cover - se_cover,
                    ymax = mean_cover + se_cover),
                width = 0.15) +
  labs(
    x = "Time point",
    y = "Percent cover (%)",
    color = "Site"
  ) +
  theme_bw()

# percent cover over time for site and habitat as one figure, this time H is solid lines and S is dashed lines, kinda hard to see though

cover_summary <- cover %>%
  group_by(SITE, HABITAT, TIMEPT) %>%
  summarize(
    mean_cover = mean(PERCENTCOVER, na.rm = TRUE),
    sd_cover   = sd(PERCENTCOVER, na.rm = TRUE),
    n          = n(),
    se_cover   = sd_cover / sqrt(n),
    .groups    = "drop"
  )

ggplot(
  cover_summary,
  aes(
    x = TIMEPT,
    y = mean_cover,
    color = SITE,
    linetype = HABITAT,
    group = interaction(SITE, HABITAT)
  )
) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean_cover - se_cover,
        ymax = mean_cover + se_cover),
    width = 0.15
  ) +
  scale_linetype_manual(
    values = c("H" = "solid",
               "S" = "dashed")
  ) +
  labs(
    x = "Time point",
    y = "Percent cover (%)",
    color = "Site",
    linetype = "Habitat"
  ) +
  theme_bw()

######

sed <- read_xlsx("data/seddataR.xlsx")
sed_long <- sed %>%
  select(SITE, TIME, PERCENTCOVER, SCD) %>%
  pivot_longer(
    cols = c(PERCENTCOVER, SCD),
    names_to = "metric",
    values_to = "value"
  ) %>%
  group_by(SITE, TIME, metric) %>%
  summarize(
    mean_value = mean(value, na.rm = TRUE),
    sd_value   = sd(value, na.rm = TRUE),
    n          = n(),
    se_value   = sd_value / sqrt(n),
    .groups    = "drop"
  )

# make TIMEPT categorical if it's like T1, T2, T3
sed_long <- sed_long %>%
  mutate(TIME = factor(TIME))

# plot percent cover & SCD over time and sites
ggplot(
  sed_long,
  aes(
    x = TIME,
    y = mean_value,
    color = SITE,
    group = SITE
  )
) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean_value - se_value,
        ymax = mean_value + se_value),
    width = 0.15
  ) +
  facet_wrap(~ metric, scales = "free_y", labeller = labeller(
    metric = c(
      PERCENTCOVER = "Percent cover (%)",
      SCD          = "Sediment carbon density"
    )
  )) +
  labs(
    x = "Time point",
    y = NULL,
    color = "Site"
  ) +
  theme_bw()
#####
library(dplyr)
library(ggplot2)

scatter_dat <- sed %>%
  filter(!is.na(PERCENTCOVER), !is.na(SCD)) %>%
  mutate(
    TIME = factor(TIME),
    SITE = factor(SITE)
  )

ggplot(
  scatter_dat,
  aes(x = PERCENTCOVER,
      y = SCD,
      color = TIME,
      group = TIME)
) +
  geom_point(size = 2, alpha = 0.8) +
  geom_path(alpha = 0.6) +  # connects points in time order
  facet_wrap(~ SITE) +
  labs(
    x = "Percent cover (%)",
    y = "Sediment carbon density",
    color = "Time"
  ) +
  theme_bw()
