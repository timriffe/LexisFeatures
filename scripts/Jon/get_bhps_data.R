# Get data from BHPS 

rm(list = ls())


pacman::p_load(
  tidyverse,
  stringr
)

# Set up another repo as submodule to this repo
# https://git-scm.com/book/en/v2/Git-Tools-Submodules
# Fingers crossed this will work! Not something I've tried before! 

#source("driving_segregation/scripts/manage_data_bhps.R")
# Note this will only work with my local machine as this is source of BHPS dataset


all_inds_drvs <- read_csv("data/csv_tidy/use_case_2_bhps/many_vars.csv")
# Mean GHQ score

all_inds_drvs %>% 
  mutate(year = wave + 1990) %>% 
  select(sex, year, age, ghq) %>% 
  group_by(sex, year, age) %>% 
  summarise(mean_ghq = mean(ghq, na.rm = T)) %>% 
  ungroup() -> tmp 

tmp %>% 
  write_csv("data/csv_tidy/use_case_2_bhps/ghq_mean.csv")

tmp %>% 
  filter(sex == "female") %>% 
  select(year, age, mean_ghq) %>% 
  spread(age, mean_ghq) %>% 
  select(-year) %>% 
  as.matrix() %>% 
  write.csv("data/csv_matrix/use_case_2_bhps/ghq_mean_female.csv", row.names = F)

tmp %>% 
  filter(sex == "male") %>% 
  select(year, age, mean_ghq) %>% 
  spread(age, mean_ghq) %>% 
  select(-year) %>% 
  as.matrix() %>% 
  write.csv("data/csv_matrix/use_case_2_bhps/ghq_mean_male.csv", row.names = F)

tmp %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = year, y = age, fill = mean_ghq)) +
  geom_tile() + 
  scale_fill_gradientn(
    colours = scales::brewer_pal(palette = "Paired")(12)
  ) +
  coord_equal() +
  facet_wrap(~sex)

ggsave("figures/unprocessed/use_case_2_bhps/mean_ghq12_by_gender.png", width = 20, height = 30, dpi = 300, units = "cm")
  
# Standard aesthetics

# GHQ12 by age and highest qualification


all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(ghq) & !is.na(highqual)) %>% 
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
  select(year, age, sex, highqual, ghq) %>% 
  arrange(year, sex, highqual, age) %>% 
  group_by(year, sex, highqual, age) %>%  
  summarise(mean_ghq = mean(ghq, na.rm=T)) %>% 
  ungroup() %>% 
  select(sex, highqual, year, age, mean_ghq) -> tmp

tmp %>% 
  write.csv("data/csv_tidy/use_case_2_bhps/ghq_mean_by_gender_highqual.csv")



tmp %>% 
  filter(!is.na(sex), !is.na(highqual)) %>% 
  ggplot(aes(x = year, y = age, fill = mean_ghq)) +
  geom_tile() + 
  scale_fill_gradientn(
    colours = scales::brewer_pal(palette = "Paired")(12)
  ) +
  coord_equal() +
  facet_grid(highqual ~ sex)

ggsave("figures/unprocessed/use_case_2_bhps/mean_ghq12_by_gender_highqual.png", width = 20, height = 30, dpi = 300, units = "cm")



make_matrix <- function(DTA){
  DTA %>% 
    select(year, age, mean_ghq) %>% 
    spread(age, mean_ghq) %>% 
    as.matrix() %>% 
    .[,-1]
}

tmp %>% 
  group_by(sex, highqual) %>% 
  nest() %>% 
  mutate(mtrx = map(data, make_matrix)) %>% 
  mutate(junk = pmap(
    list(mtrx, sex, highqual), 
    function(mtrx, sex, highqual){
      write.csv(
        mtrx, row.names=F, 
        file = file.path("data/csv_tidy/use_case_2_bhps", paste0("mean_ghq_", sex, "_", highqual, ".csv"))
      )
      NULL
    }
  )
  )


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

# As matrices 

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
  select(sex, year, age, drive_prop) -> tmp

tmp %>% 
  filter(sex == "female") %>% 
  select(year, age, drive_prop) %>% 
  spread(age, drive_prop) %>% 
  as.matrix() %>% 
  .[,-1] %>% 
  write.csv("data/csv_matrix/use_case_2_bhps/prop_driving_female.csv", row.names = F)

tmp %>% 
  filter(sex == "male") %>% 
  select(year, age, drive_prop) %>% 
  spread(age, drive_prop) %>% 
  as.matrix() %>% 
  .[,-1] %>% 
  write.csv("data/csv_matrix/use_case_2_bhps/prop_driving_male.csv", row.names = F)

rm(tmp)


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
  ggplot(aes(x = year, y = age, fill = drive_prop)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = scales::brewer_pal(palette = "Paired")(12)
  ) +
  facet_wrap(~ sex) +
  coord_equal()


ggsave("figures/unprocessed/use_case_2_bhps/prop_driving_by_gender.png", dpi = 300, height = 30, width = 25, units = "cm")
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
  select(sex, highqual, year, age, drive_prop) -> tmp

tmp %>% 
  write_csv("data/csv_tidy/use_case_2_bhps/prop_driving_BY_sex_highqual.csv")

tmp %>% 
  ggplot(aes(x = year, y = age, fill = drive_prop)) +
  geom_tile() +
  facet_grid(sex ~ highqual) + 
  coord_equal() +
  scale_fill_gradientn(
    colours = scales::brewer_pal(palette = "Paired")(12)
  )

ggsave("figures/unprocessed/use_case_2_bhps/prop_driving_by_gender_and_highqual.png", dpi = 300, height = 30, width = 25, units = "cm")

make_matrix <- function(DTA){
  DTA %>% 
    select(year, age, drive_prop) %>% 
    spread(age, drive_prop) %>% 
    as.matrix() %>% 
    .[,-1]
}

tmp %>% 
  group_by(sex, highqual) %>% 
  nest() %>% 
  mutate(mtrx = map(data, make_matrix)) %>% 
  mutate(junk = pmap(
      list(mtrx, sex, highqual), 
      function(mtrx, sex, highqual){
        write.csv(
          mtrx, row.names=F, 
          file = file.path("data/csv_tidy/use_case_2_bhps", paste0("prop_driving_", sex, "_", highqual, ".csv"))
        )
        NULL
      }
    )
  )









