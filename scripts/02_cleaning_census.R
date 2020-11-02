#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://usa.ipums.org/usa/index.shtml
# Author: Zhixing Hong, Lingyue Kong, Jinyu Luo
# Data: 22 October 2020
# Contact: wisteria.hong@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)


# Read in the raw data.
setwd("~/Prediction_2020_election")
raw_census <- read_dta("inputs/usa_00003.dta")


# Add the labels
raw_census <- labelled::to_factor(raw_census)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_census <- 
  raw_census %>% 
  select(statefip,
         sex,
         age, 
         race,
         gradeatt, 
         hcovany, hhwt, hhincome, perwt)


# change sex into gender
reduced_census <- reduced_census %>% rename(gender = sex)

# clean age
reduced_census <- reduced_census %>% 
  filter(age != "less than 1 year old") %>% 
  filter(age != "90 (90+ in 1980 and 1990)")
reduced_census$ age <- as.numeric(reduced_census$age)
reduced_census  <- reduced_census %>% filter(age > 17)

#create age group
Age <- cut(reduced_census$age, c(seq(18, 100, by = 15), Inf), include.lowest = TRUE)
reduced_census <- reduced_census%>%mutate(age_group = Age)

#remove age and education
summary(reduced_census)
reduced_census <- reduced_census %>% dplyr::select(-gradeatt, -age)

#clean race
reduced_census <- reduced_census %>% filter(! (race == "two major races" | race == "three or more major races"))

# clean state
reduced_census <- reduced_census %>% 
  mutate(state = state.abb[match(str_to_title(reduced_census$statefip), state.name)]) %>%
  filter(! is.na(state))

# clean income
head(reduced_census)
reduced_census <- reduced_census %>% mutate(income = case_when(hhincome <= 19999 ~ "Less than $19,999",
                                                               19999 < hhincome & hhincome <= 39999 ~ "$20,000 to $39,999",
                                                               39999 < hhincome & hhincome <= 59999 ~ "$40,000 to $59,999",
                                                               59999 < hhincome & hhincome <= 79999 ~ "$60,000 to $79,999",
                                                               79999 < hhincome & hhincome <= 99999 ~ "$80,000 to $99,999",
                                                               99999 < hhincome & hhincome <= 149999 ~ "$100,000 to $149,999",
                                                               149999 < hhincome & hhincome <= 199999 ~ "$150,000 to $199,999",
                                                               199999 < hhincome ~ "$200,000 and above"))


# create the cell format data
df <- reduced_census %>%
  count(age_group, gender, income, race, state, hcovany) %>%
  group_by(age_group, gender, income, race, state, hcovany)

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_census, "outputs/census_data.csv")
write_csv(df, "outputs/census_cell.csv")

