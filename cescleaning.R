#devtools::install_github("hodgettsp/cesR")
library(janitor)
library(tidyverse)
library(readr)
library(cesR)
setwd("D:/STA304/FinalProject")
get_ces("ces2019_phone")
raw_data <- ces2019_phone %>%
  select(q2, interviewer_gender_CES, q4, q6, q61, q67, q11) %>%
  drop_na(interviewer_gender_CES) %>%
  drop_na(q4) %>%
  drop_na(q6) %>%
  drop_na(q61) %>%
  drop_na(q67) %>%
  drop_na(q11)

colnames(raw_data) <- c("age", "sex", "province", "feelings_life", "education", 
                         "language_home", "vote_for_lib")

raw_data <- raw_data %>%
  rowwise() %>%
  mutate(province = case_when(
    province == 1 ~ "Newfoundland and Labrador",
    province == 2 ~ "Prince Edward Island",
    province == 3 ~ "Nova Scotia",
    province == 4 ~ "New Brunswick",
    province == 5 ~ "Quebec",
    province == 6 ~ "Ontario",
    province == 7 ~ "Manitoba",
    province == 8 ~ "Saskatchewan",
    province == 9 ~ "Alberta",
    province == 10 ~ "British Columbia",
    province == 11 ~ "Northwest Territories",
    province == 12 ~ "Yukon",
    province == 13 ~ "Nunavut",
  ))

raw_data <- raw_data %>%
  filter(feelings_life != -9) %>%
  filter(feelings_life != -8) %>%
  rowwise() %>%
  mutate(feelings_life = case_when(
    feelings_life == 1 ~ 10,
    feelings_life == 2 ~ 7.5,
    feelings_life == 3 ~ 2.5,
    feelings_life == 4 ~ 0,
  ))

raw_data <- raw_data %>%
  filter(education != -9) %>%
  filter(education != -8) %>%
  rowwise() %>% 
  mutate(education = case_when(
    education == 1 ~ "Less than high school",
    education == 2 ~ "Less than high school",
    education == 3 ~ "Less than high school",
    education == 4 ~ "Less than high school",
    education == 5 ~ "High school or equ",
    education == 6 ~ "Trade certificate",
    education == 7 ~ "College",
    education == 8 ~ "Dipolma below bachelor",
    education == 9 ~ "Bachelor",
    education == 10 ~ "Above bachelor",
    education == 11 ~ "Above bachelor"
  ))


raw_data <- raw_data %>%
  filter(language_home != -8) %>%
  rowwise() %>%
  mutate(language_home = case_when(
    language_home == 1 ~ "EN",
    language_home == 4 ~ "FR",
    language_home == 2 ~ "Non-Official",
    language_home == 3 ~ "Non-Official",
    language_home == 5 ~ "Non-Official",
    language_home == 6 ~ "Non-Official",
    language_home == 7 ~ "Non-Official",
    language_home == 8 ~ "Non-Official",
    language_home == 9 ~ "Non-Official",
    language_home == 10 ~ "Non-Official",
    language_home == 11 ~ "Non-Official",
    language_home == 12 ~ "Non-Official",
    language_home == 13 ~ "Non-Official",
    language_home == 14 ~ "Non-Official",
    language_home == 15 ~ "Non-Official",
    language_home == 16 ~ "Non-Official",
    language_home == 17 ~ "Non-Official",
    language_home == 18 ~ "Non-Official",
    language_home == 19 ~ "Non-Official",
    language_home == 20 ~ "Non-Official",
    language_home == 21 ~ "Non-Official",
    language_home == 22 ~ "Non-Official",
    language_home == 23 ~ "Non-Official",
    language_home == 24 ~ "Non-Official",
    language_home == 25 ~ "Non-Official",
    language_home == 26 ~ "Non-Official",
    language_home == 27 ~ "Non-Official",
    language_home == 28 ~ "Non-Official",
    language_home == 29 ~ "Non-Official",
    language_home == 30 ~ "Non-Official",
    language_home == 31 ~ "Non-Official"
  ))


raw_data <- raw_data %>%
  mutate(age = 2019 - age)

raw_data <- raw_data %>%
  filter(vote_for_lib != -9) %>%
  filter(vote_for_lib != -8) %>%
  mutate(vote_for_lib = case_when(
    vote_for_lib == 1 ~ 1,
    vote_for_lib != 1 ~ 0
  ))


raw_data <- raw_data %>%
  select(age, sex, province, feelings_life, education, language_home, vote_for_lib)
write_csv(raw_data, "data.csv")