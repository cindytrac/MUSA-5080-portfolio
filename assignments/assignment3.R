# load the libraries
library(tidyverse)
library(tidycensus)

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


%>%
  mutate(signigicant = case_when(
    z_score < 12 ~ "reliable",
    poverty_cv <= 40 ~ "somewhat reliable",
    poverty_cv > 40 ~ "unreliable"
  )





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
    NAME     = "County",
    estimate = "Estimate",
    moe      = "MOE",
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
philly_tracts <- st_left_join(philly_tracts_2017, philly_tracts_2010, by="GEOID")



#| label: first-map
ggplot(philly_tracts_2010) +
  geom_sf(aes(fill = poverty_rate), color = NA) +
  scale_fill_viridis_c(labels = scales::percent) +
  theme_void() +
  labs(title = "Poverty Rate In Philadelphia Tracts",
       fill = NULL)
