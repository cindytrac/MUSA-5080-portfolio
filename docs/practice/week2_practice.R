library(tidyverse)
library(tidycensus)

### LOAD DATA
pa_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "PA",
  year = 2023,
  survey = "acs5"
)

### LOOK AT THE DATA

# open up the table
view(pa_income)

# how many dimensions
dim(pa_income)

# quick look
glimpse(pa_income)

# first ten entries
head(pa_income, 10)

# remove a variable
rm(pa_income)



