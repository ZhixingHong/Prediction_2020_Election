#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://www.voterstudygroup.org/publication/nationscape-data-set
# Author: Zhixing Hong, Lingyue Kong, Jinyu Luo
# Data: 22 October 2020
# Contact: wisteria.hong@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from the above mentioned website and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
library(labelled)


setwd("~/Prediction_2020_election")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_survey <- read_dta("inputs/Nationscape-DataRelease_WeeklyMaterials_DTA/phase_2_v20200814/ns20200625/ns20200625.dta")

raw_survey <- labelled::to_factor(raw_survey)
# Just keep some variables
reduced_survey <- 
  raw_survey %>% 
  select(vote_2020, age, gender, state, household_income, 
         race_ethnicity, gov_insurance,
         interest,
         registration,
         vote_2016,
         vote_intention,
         ideo5,
         census_region, 
         house_intent, trump_biden, 
         medicare_for_all,
         statements_confront_china, 
         weight)


# Trump's supporters in 2020
reduced_survey<- reduced_survey %>% mutate(voteTrump_2020 = ifelse(vote_2020 == "Donald Trump", 1, 0))

# Trump's supporters in 2016
reduced_survey<- reduced_survey %>% mutate(voteTrump_2016 = ifelse(vote_2016 == "Donald Trump", 1, 0))

# Biden's supporters in 2020 
reduced_survey<- reduced_survey %>% mutate(voteBiden_2020 = ifelse(vote_2020 == "Joe Biden", 1, 0))

# clean state 
reduced_survey$state <- as.factor(reduced_survey$state)

#create age group
Age <- cut(reduced_survey$age, c(seq(18, 100, by = 15), Inf), include.lowest = TRUE)
reduced_survey <- reduced_survey%>%mutate(age_group = Age)

# clean race
reduced_survey <- reduced_survey %>% mutate(race = 
                                              case_when(str_detect(race_ethnicity, "Chinese") ~ "chinese", 
                                                        str_detect(race_ethnicity, "Japanese") ~ "japanese",
                                                        str_detect(race_ethnicity, "White") ~ "white",
                                                        str_detect(race_ethnicity, "Black") ~ "black/african american/negro",
                                                        str_detect(race_ethnicity, "Alaska Native") ~ "american indian or alaska native",
                                                        str_detect(race_ethnicity, "Some") ~ "other race, nec"))
reduced_survey$race[is.na(reduced_survey$race)] <- "other asian or pacific islander"

# remove na
reduced_survey <- reduced_survey %>% filter( !(is.na(voteTrump_2020) |is.na(voteTrump_2016) |is.na(voteBiden_2020) |
                                                 is.na(age_group) | is.na(gender)|is.na(state)| is.na(household_income) |
                                                 is.na(house_intent) | is.na(trump_biden)| is.na(medicare_for_all)|
                                                 is.na(statements_confront_china)|is.na(weight)|
                                                 is.na(gov_insurance)))

# clean income
reduced_survey$household_income <- fct_collapse(reduced_survey$household_income, 
                                                'Less than $19,999' = c("Less than $14,999", "$15,000 to $19,999"),
                                                '$20,000 to $39,999' = c("$20,000 to $24,999","$25,000 to $29,999", "$30,000 to $34,999", "$35,000 to $39,999"),
                                                '$40,000 to $59,999' = c("$40,000 to $44,999","$45,000 to $49,999", "$50,000 to $54,999", "$55,000 to $59,999"),
                                                '$60,000 to $79,999'= c("$60,000 to $64,999","$65,000 to $69,999", "$70,000 to $74,999", "$75,000 to $79,999"),
                                                '$80,000 to $99,999'= c("$80,000 to $84,999","$85,000 to $89,999", "$90,000 to $94,999", "$95,000 to $99,999"),
                                                "$100,000 to $149,999" = c("$100,000 to $124,999","$125,000 to $149,999"),
                                                "$150,000 to $199,999" = c("$150,000 to $174,999","$175,000 to $199,999"),
                                                "$200,000 and above" = c("$200,000 to $249,999" ,"$250,000 and above"))

#gender case match
reduced_survey$gender <- str_to_lower(reduced_survey$gender)

reduced_survey <- reduced_survey%>%rename(income = household_income)
# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_survey, "outputs/survey_data.csv")
