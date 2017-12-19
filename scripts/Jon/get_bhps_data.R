# Get data from BHPS 

rm(list = ls())


pacman::p_load(
  tidyverse,
  stringr
)

# Set up another repo as submodule to this repo
# https://git-scm.com/book/en/v2/Git-Tools-Submodules
# Fingers crossed this will work! Not something I've tried before! 

source("driving_segregation/scripts/manage_data_bhps.R")
# Note this will only work with my local machine as this is source of BHPS dataset

# Mean GHQ score

all_inds_drvs %>% 
  mutate(year = wave + 1990) %>% 
  select(sex, year, age, ghq) %>% 
  group_by(sex, year, age) %>% 
  summarise(mean_ghq = mean(ghq, na.rm = T)) %>% 
  ungroup() %>% 
  write_csv("data/csv_tidy/use_case_2_bhps/ghq_mean.csv")

# all_inds_drvs %>% 
#   mutate(year = wave + 1990) %>% 
#   select(sex, year, age, ghq) %>% 
#   group_by(sex, year, age) %>% 
#   summarise(mean_ghq = mean(ghq, na.rm = T)) %>% 
#   ggplot(aes(x = year, y = age, fill = mean_ghq)) + 
#   geom_tile()

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) ) %>% 
  filter( year > 1992) %>% 
  select(year, age, sex,  dlo) %>% 
  arrange(year, sex, age) %>% 
  group_by(year, sex, age) %>% 
  mutate(does_drive = car::recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(drive_prop = mean(does_drive, na.rm=T)) %>% 
  filter(age <= 80 & age >= 17) %>%
  ungroup() %>% 
  select(sex, year, age, drive_prop) %>% 
  write_csv("data/csv_tidy/use_case_2_bhps/prop_driving_BY_sex.csv")

# all_inds_drvs %>% 
#   mutate(
#     year = wave + 1990
#   ) %>% 
#   filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) ) %>% 
#   filter( year > 1992) %>% 
#   select(year, age, sex,  dlo) %>% 
#   arrange(year, sex, age) %>% 
#   group_by(year, sex, age) %>% 
#   mutate(does_drive = car::recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
#   summarise(drive_prop = mean(does_drive, na.rm=T)) %>% 
#   filter(age <= 80 & age >= 17) %>%
#   ungroup() %>% 
#   select(sex, year, age, drive_prop) %>% 
#   ggplot(aes(x = year, y = age, fill = drive_prop)) + 
#   geom_tile() + 
#   facet_wrap(~ sex)

# driving licence ownership further divided by highest qualification
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(highqual)) %>% 
  filter( year > 1992) %>% 
  mutate(highqual = car::recode(
    highqual, 
    "
    'further non-vocational' = 'High';
    'further vocational' = 'Med';
    'no further' = 'Low'
    ",
    levels = c("Low", "Med", "High"), as.factor.result =T
  ),
  sex = car::recode(sex, "'male' = 'M'; 'female' = 'F'")
  ) %>% 
  select(year, age, sex, highqual, dlo) %>% 
  arrange(year, sex, highqual, age) %>% 
  group_by(year, sex, highqual, age) %>% 
  mutate(does_drive = car::recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(drive_prop = mean(does_drive, na.rm=T)) %>% 
  ungroup() %>% 
  select(sex, highqual, year, age, drive_prop) %>% 
  write_csv("data/csv_tidy/use_case_2_bhps/prop_driving_BY_sex_highqual.csv")

# all_inds_drvs %>% 
#   mutate(
#     year = wave + 1990
#   ) %>% 
#   filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(highqual)) %>% 
#   filter( year > 1992) %>% 
#   mutate(highqual = car::recode(
#     highqual, 
#     "
#     'further non-vocational' = 'High';
#     'further vocational' = 'Med';
#     'no further' = 'Low'
#     ",
#     levels = c("Low", "Med", "High"), as.factor.result =T
#   ),
#   sex = car::recode(sex, "'male' = 'M'; 'female' = 'F'")
#   ) %>% 
#   select(year, age, sex, highqual, dlo) %>% 
#   arrange(year, sex, highqual, age) %>% 
#   group_by(year, sex, highqual, age) %>% 
#   mutate(does_drive = car::recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
#   summarise(drive_prop = mean(does_drive, na.rm=T)) %>% 
#   ungroup() %>% 
#   select(sex, highqual, year, age, drive_prop) %>% 
#   ggplot(aes(x = year, y = age, fill = drive_prop)) + 
#   geom_tile() +
#   facet_grid(sex ~ highqual)


