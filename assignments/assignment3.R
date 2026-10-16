# load the libraries
library(tidyverse)
library(tidycensus)
library(ggplot2)
library(dplyr)

# Part 1 — Reconstruct
# Pull the ACS data the article appears to rely on. Document:
#
# Which variable(s) you used and why. Include the variable code.
# Which years and survey (acs5 vs acs1) you used and why.
# What you had to assume, because the article does not say.
# State clearly whether the two periods the article compares overlap. If they do, say what that means for any conclusion drawn from the comparison.

# data for 2006-2010
philly_tracts_2010 <- get_acs(
  geography = "tract",
  state = "PA",
  county = "Philadelphia",
  variables = c(
    total_pop_pov_universe = "B17001_001",
    pop_below_poverty_line = "B17001_002",
    below_poverty_white = "B17001A_002",
    total_pop = "B01003_001",
    med_hh_income = "B19013_001"
  ),
  output = "wide",
  geometry = FALSE,
  year = 2010,
  survey = "acs5"
)
# data for 2013-2017
philly_tracts_2017 <- get_acs(
  geography = "tract",
  state = "PA",
  county = "Philadelphia",
  variables = c(total_pop_pov_universe = "B17001_001", #total population in poverty universe
                pop_below_poverty_line = "B17001_002", #population below the pov level
                below_poverty_white = "B17001A_002",
                total_pop = "B01003_001",
                med_hh_income = "B19013_001"),
  output = "wide",
  geometry = FALSE,
  year = 2017,
  survey = "acs5"
)







head(philly_tracts)



# Part 2 — Test (code plus interpretation)
# For each tract:
#
# Compute the poverty rate (or whatever variable you are using if you found your own article) and its margin of error. Use moe_prop() — a rate is a proportion, and the MOE does not carry over from the count.
# Compute the coefficient of variation and flag reliability using the Jurjevich et al. (2018) thresholds: CV < 12% reliable, 12–40% somewhat reliable, > 40% unreliable.
# Compute the change between the two periods and the margin of error of that change.
# Test whether the change is statistically distinguishable from zero at 90% confidence.
# Report how many tracts show a statistically detectable change and how many do not.
#
# Handle your missing values deliberately. Some tracts will produce NA. Find out why before you decide what to do about them, and say what you decided and why (often areas of no population like airports)


# get each tract in philly
# compute the poverty rate



philly_tracts_2010 <- philly_tracts_2010 %>%
  mutate(
    poverty_rate = 100 * pop_below_poverty_lineE / total_pop_pov_universeE
  )

# drop NA rows
philly_tracts_2010 <- na.omit(philly_tracts_2010)

philly_tracts_2017 <- philly_tracts_2017 %>%
  mutate(
    poverty_rate = 100 * pop_below_poverty_lineE / total_pop_pov_universeE
  )

# drop NA rows
philly_tracts_2017 <- na.omit(philly_tracts_2017)

philly_tracts_2010 <- philly_tracts_2010 %>%
  mutate(
    moe_prop = moe_prop(pop_below_poverty_lineE, total_pop_pov_universeE, pop_below_poverty_lineM, total_pop_pov_universeM)
  )

philly_tracts_2017 <- philly_tracts_2017 %>%
  mutate(
    moe_prop = moe_prop(pop_below_poverty_lineE, total_pop_pov_universeE, pop_below_poverty_lineM, total_pop_pov_universeM)
)


# calculate cv

philly_tracts_2010 <- philly_tracts_2010 %>%
  mutate(poverty_cv = 100 * moe_prop / poverty_rate) %>%
  mutate(reliability = case_when(
    poverty_cv < 12 ~ "reliable",
    poverty_cv <= 40 ~ "somewhat reliable",
    poverty_cv > 40 ~ "unreliable"
  ))

philly_tracts_2017 <- philly_tracts_2017 %>%
  mutate(poverty_cv = 100 * moe_prop / poverty_rate) %>%
  mutate(reliability = case_when(
    poverty_cv < 12 ~ "reliable",
    poverty_cv <= 40 ~ "somewhat reliable",
    poverty_cv > 40 ~ "unreliable"
  ))



philly_tract_combined <- left_join(philly_tracts_2017, philly_tracts_2010, by = "GEOID")
philly_tract_combined <- philly_tract_combined %>%
  mutate(st_as_sf())

Standard error = margin of error/1.645

x = 2017
y = 2010

change = estimate_2 - estimate_1

The poverty level is poverty_rate +/- moe_prop
We cannot just subtract the margin of error and call it a day.
We need to determine the uncertainty around the difference.



# CALCULATE STANDARD ERROR
philly_tract_combined <- philly_tract_combined %>%
  mutate(SE2010 = (moe_prop.y*100)/1.645) %>%
  mutate(SE2017 = (moe_prop.x*100)/1.645)

philly_tract_combined <- philly_tract_combined %>%
  mutate(SE_diff = sqrt((SE2010^2) + (SE2017^2)))

philly_tract_combined <- philly_tract_combined %>%
  mutate(z_score = ((poverty_rate.y-poverty_rate.x)/SE_diff))

philly_tract_combined <- philly_tract_combined %>%
  mutate(diff_poverty_rate = poverty_rate.y-poverty_rate.x)

plot_data <- philly_tracts_2010 %>%
  filter(!is.na(poverty_rate), !is.na(poverty_rate_moe)) %>%
  slice_max(poverty_rate, n = 20) %>%
  mutate(GEOID = reorder(GEOID, poverty_rate))

ggplot(philly_tracts_2010, aes(
  x = reorder(GEOID, poverty_rate),
  y = poverty_rate
)) +
  geom_errorbar(
    aes(
      ymin = poverty_rate - moe_prop,
      ymax = poverty_rate + moe_prop
    ),
    width = 0.25
  ) +
  geom_point(size = 2) +
  coord_flip() +
  labs(
    x = "Philly census tract",
    y = "Poverty rate (%)"
  ) +
  theme_minimal()

m  <- aggregate(len ~ dose, data = ToothGrowth, FUN = mean)
s  <- aggregate(len ~ dose, data = ToothGrowth, FUN = sd)
df <- data.frame(dose = factor(m$dose), len = m$len, sd = s$len)

df <- philly_tracts_2010 %>%
  st_drop_geometry() %>%
  filter(
    !is.na(poverty_rate),
    !is.na(poverty_rate_moe)
  ) %>%
  mutate(
    tract = factor(GEOID)
  )

ggplot(df, aes(x = tract, y = poverty_rate)) +
  geom_col(fill = "#3a86d4", alpha = 0.7) +
  geom_errorbar(
    aes(
      ymin = poverty_rate - poverty_rate_moe,
      ymax = poverty_rate + poverty_rate_moe
    ),
    width = 0.2
  ) +
  labs(
    x = "Philadelphia census tract",
    y = "Poverty rate (%)",
    title = "Poverty Rate by Philadelphia Census Tract",
    subtitle = "2006–2010 ACS 5-year estimates with 90% margins of error"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggplot(df, aes(x = dose, y = len)) +
  geom_col(fill = "#3a86d4", alpha = 0.7) +
  geom_errorbar(aes(ymin = len - sd, ymax = len + sd), width = 0.2) +
  labs(x = "Dose (mg/day)", y = "Tooth length (mean ± SD)") +
  theme_minimal()


%>%
  mutate(signigicant = case_when(
    z_score < 12 ~ "reliable",
    poverty_cv <= 40 ~ "somewhat reliable",
    poverty_cv > 40 ~ "unreliable"
  )


  philly_tract_combined <- philly_tract_combined %>%
    mutate(diff_med_hh_income = med_hh_incomeE.x-med_hh_incomeE.y)


#
change <- poverty_rate.x - poverty_rate.y

#determine the uncertainty around the difference
change_moe <- sqrt(moe_prop.x^2 + moe_prop.y^2)


reliability_table %>%
  gt() %>%
  fmt_number(columns = c(estimate, moe), decimals = 0) %>%
  fmt_number(columns = cv, decimals = 1) %>%
  cols_label(
    dot      = "",
    NAME     = "Name",
    estimate = "poverty_rate",
    moe      = "moe_prop",
    cv       = "CV (%)",
    flag     = "Reliability"
  ) %>%
  cols_align(align = "center", columns = dot) %>%
  tab_header(
    title    = "Population below 100% of the poverty level",
    subtitle = "New York counties, ACS 2017 5-year estimates"
  ) %>%
  tab_source_note(
    "Reliability thresholds follow Jurjevich et al. (2018): CV < 12% reliable, 12–40% somewhat reliable, > 40% unreliable."
  )


## Create a fancy reliability table like in the paper:

library(gt)

reliability_table <- philly_tracts_2017 %>%
  mutate(
    cv = (moe_prop / 1.645) / poverty_rate * 100,
    dot = case_when(
      cv < 12  ~ "🟢",
      cv <= 40 ~ "🟡",
      TRUE     ~ "🔴"
    ),
    flag = case_when(
      cv < 12  ~ "Reliable",
      cv <= 40 ~ "Somewhat reliable",
      TRUE     ~ "Unreliable"

  ))

reliability_table %>%
  gt() %>%
  fmt_number(columns = c(poverty_rate, moe_prop), decimals = 0) %>%
  fmt_number(columns = cv, decimals = 1) %>%
  cols_label(
    dot      = "",
    NAME     = "Name",
    estimate = "poverty_rate",
    moe      = "moe_prop",
    cv       = "CV (%)",
    flag     = "Reliability"
  ) %>%
  cols_align(align = "center", columns = dot) %>%
  tab_header(
    title    = "Population below 100% of the poverty level",
    subtitle = "New York counties, ACS 2023 5-year estimates"
  ) %>%
  tab_source_note(
    "Reliability thresholds follow Jurjevich et al. (2018): CV < 12% reliable, 12–40% somewhat reliable, > 40% unreliable."
  )

philly_tracts <- st_left_join(philly_tracts_2017, philly_tracts_2010, by="GEOID")



#| label: first-map
ggplot(philly_tracts_2010) +
  geom_sf(aes(fill = poverty_rate), color = NA) +
  scale_fill_viridis_c(labels = scales::percent) +
  theme_void() +
  labs(title = "Poverty Rate In Philadelphia Tracts",
       fill = NULL)

# Visualize with error bars
poverty_tested %>%
  filter(significant) %>%
  mutate(Name.x)) %>%
  ggplot(aes(x = reorder(Name.x, change), y = change)) +
  geom_col(aes(fill = change > 0)) +
  geom_errorbar(aes(ymin = change - moe_diff,
                    ymax = change + moe_diff), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Statistically significant change in poverty count",
       subtitle = "2013–2017 vs 2006–2010, ACS 5-year estimates",
       x = NULL, y = "Change in people below poverty level")

library(dplyr)
library(gt)

reliability_table <- philly_tracts_2017 %>%
  mutate(
    cv = case_when(TRUE ~ 100 * (moe_prop / 1.645) / poverty_rate),
    dot = case_when(
      cv < 12  ~ "🟢",
      cv <= 40 ~ "🟡",
      TRUE     ~ "🔴"
    ),
    flag = case_when(
      is.na(cv) ~ NA_character_,
      cv < 12 ~ "Reliable",
      cv <= 40 ~ "Somewhat reliable",
      TRUE ~ "Unreliable"
    )
  ) %>%
  select(
    GEOID,
    dot,
    NAME,
    moe_prop,
    poverty_rate,
    cv,
    flag
  )

reliability_table %>%
  gt() %>%
  fmt_number(
    columns = c(moe_prop, poverty_rate),
    decimals = 1
  ) %>%
  fmt_number(
    columns = cv,
    decimals = 1
  ) %>%
  cols_label(
    GEOID = "GEOID",
    dot = "",
    NAME = "Name",
    moe_prop = "Margin of error",
    poverty_rate = "Poverty rate (%)",
    cv = "CV (%)",
    flag = "Reliability"
  ) %>%
  cols_align(
    align = "center",
    columns = c(dot, cv, flag)
  ) %>%
  tab_header(
    title = "Population Below 100% of the Poverty Level",
    subtitle = "Philadelphia Tracts, ACS 2013–2017 5-year estimates"
  ) %>%
  tab_source_note(
    "Reliability thresholds follow Jurjevich et al. (2018): CV < 12% reliable, 12–40% somewhat reliable, > 40% unreliable."
  )
