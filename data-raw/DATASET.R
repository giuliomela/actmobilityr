## Code to prepare external data

library(eurostat)
library(rsdmx)
library(tidyverse)
library(readxl)
library(here)
library(AMR)

ref_yr <- 2020 # reference year of the analysis

### Donwloading demographic from Eurostat

# loading nuts codes

nuts_codes_it <- read_excel(here("data-raw/nuts_codes_it.xlsx"))$nuts_codes_it

bottom_age <- seq(20, 60, by = 5)

upper_age <- bottom_age + 4

age_groups <- paste0("Y", bottom_age, "-", upper_age)

age_detail <- paste0("Y", min(bottom_age):max(upper_age))

std_filters <- list(age = c("TOTAL", age_groups),
                    geo = nuts_codes_it,
                    sex = "T",
                    time = seq(ref_yr - 4, ref_yr, by = 1)
)

# population and deaths at NUTS3 level

pop_death_codes <- c("demo_r_pjangrp3", "demo_r_magec3")

pop_death_names <- c("pop", "deaths")

pop_death_raw <- map2(pop_death_codes, pop_death_names, function(x, y) {

  data_raw <- get_eurostat(x,
                           time_format = "num",
                           filters = std_filters
  )

  data_raw <- mutate(data_raw, var = y)

  data_raw

}

)

# joining tibbles and calculating mortality rate and share of each age group on total population

pop_death <- bind_rows(pop_death_raw) %>%
  pivot_wider(names_from = var, values_from = values) %>%
  group_by(sex, unit, geo, time) %>%
  mutate(pop_share = pop / pop[age == "TOTAL"]) %>%
  ungroup() %>%
  filter(age != "TOTAL") %>%
  mutate(death_rate = deaths / pop) %>%
  mutate(geo_label = label_eurostat(geo, dic = "geo", fix_duplicated = T)) %>%
  select(!c(sex, unit))

# dowloading life expectancy data (at NUTS level only)

life_exp_raw <- get_eurostat("demo_r_mlifexp",
                             time_format = "num")


life_exp_raw <- life_exp_raw %>% # selecting value referring to italian nuts2
  filter(geo %in% nuts_codes_it & sex == "T")

life_exp_raw <- life_exp_raw %>%
  filter(age %in% age_detail) # selecting ages between 20 and 64

# identifying age groups

life_exp <- life_exp_raw %>%
  mutate(age = as.numeric(str_remove(age, "Y")),
         age = as.character(age_groups(age, split_at = upper_age + 1)),
         age = if_else(age == "0-24", "20-24", age)) %>%
  group_by(age, geo, time) %>%
  summarise(life_exp = mean(values, na.rm = TRUE),
            values = NULL) %>%
  ungroup() %>% # life expectancy for each age group is the average of single year's values
  rename(geo_sel = geo) %>%
  filter(time %in% unique(pop_death$time))

# joining pop and death data with life excpectancy data (life expectancy data are at NUTS2 levels also for NUTS3 aggregates)

demo_data <- pop_death %>%
  mutate(geo_sel = if_else(nchar(geo) <= 4, geo, str_sub(geo, 1, 4)),
         age = str_remove(age, "Y")) %>%
  left_join(life_exp) %>%
  select(!geo_sel) %>%
  filter(time <= ref_yr) %>%
  relocate(year = time, geo, geo_label, age, pop, pop_share, deaths, death_rate, life_exp)

### Physical activty data

phy_act_raw <- get_eurostat("hlth_ehis_pe2e", time_format = "num")

phy_act_raw <- phy_act_raw %>%
  filter(sex == "T" &
           geo == "IT" &
           isced11 == "TOTAL" &
           abs(time - ref_yr) == min(abs(time - ref_yr)))

# age groups are not the same as the other dataset, so a recoding is needed

phy_act <- phy_act_raw %>%
  filter(age %in% c("Y35-44", "Y45-54", "Y55-64")) %>%
  mutate(age_a = case_when(
    age == "Y35-44" ~ "35-39",
    age == "Y45-54" ~ "45-49",
    TRUE ~ "55-59"
  )) %>%
  bind_rows(phy_act_raw %>%
              mutate(age_a = case_when(
                age == "Y20-24" ~ "20-24",
                age == "Y25-29" ~ "25-29",
                age == "Y25-34" ~ "30-34",
                age == "Y35-44" ~ "40-44",
                age == "Y45-54" ~ "50-54",
                age == "Y55-64" ~ "60-64",
                TRUE ~ "not_considered"
              ))) %>%
  filter(age_a != "not_considered") %>%
  arrange(age_a) %>%
  mutate(values = values / 100) %>%
  select(duration, age = age_a, phy_act_share = values)

# removing data on physical activity longer than 150 min/week

phy_act <- phy_act %>%
  filter(duration != "MN_GE150")

### Preparing commuting data

# loading ISTAT's commuting matrix

commuting_matrix <- read_table(here("data-raw/matrix_pendolo.txt"), col_names = FALSE) %>%
  select(1:15)

# Renaming the commuting matrix's variables

names(commuting_matrix) <- c("record_type", "residence_type", "prov_code", "mun_code",
                             "sex", "travel_reason", "place_of_destination", "province_work", "municipality_work", "country_work",
                             "mean_of_transp", "leaving_hour", "travel_time", "individuals_estimate",
                             "individuals")

# Municipality names, in the commuting matrix, are coded. For this reason it is
# necessary to pair them with municipality names.

# Loading municipality names (manually downloaded from ISTAT)

mun_codes <- read_xls(here("data-raw/codici_comuni_2011.xls"), sheet = 2) %>%
  select(4, 7, 11, 12, 18:20) %>% # selecting only codes and names
  rename(code = 1, name = 2, region = 3, province = 4, nuts1 = 5,
         nuts2 = 6, nuts3 = 7) %>%
  mutate(name = if_else(name == "Roma Capitale", "Roma", name))

# Adding municipality names to the commuting matrix

commuting_matrix <- commuting_matrix %>%
  mutate(code = paste0(prov_code, mun_code),
         prov_code = NULL, mun_code = NULL) %>%
  left_join(mun_codes) %>%
  rename(municipality = name)

# Loading population data at municipality level (manually downloaded from ISTAT)

pop_mun_20 <- read_csv(here("data-raw/pop_comuni_20.csv"))  %>%
  rename(code = 1, pop20 = Value) %>%
  select(code, pop20)

commuting_matrix <- commuting_matrix %>%
  left_join(pop_mun_20)

# Manipulating the commuting matrix to extract relevant information
# although the original matrix distinguishes between male and female commuters, here they are summed up together

comm_matrix_light <- commuting_matrix %>%
  filter(record_type == "L") %>% # detailed data
  group_by(travel_reason, place_of_destination, code,
           municipality, region, province, nuts1, nuts2, nuts3,
           mean_of_transp, travel_time, pop20) %>% # performing some grouping, summing up males and females
  summarize(individuals = sum(as.numeric(individuals_estimate))) %>%
  ungroup()

# Recoding the variables to make them human-readble

comm_matrix_light <- comm_matrix_light %>%
  mutate(
    travel_reason = if_else(travel_reason == 1, "study", "work"),
    place_of_destination = case_when(
      place_of_destination == 1 ~ "same municipality",
      place_of_destination == 2 ~ "other municipality",
      TRUE ~ "other country"
    ),
    mean_of_transp = case_when(
      mean_of_transp == "01" ~ "train",
      mean_of_transp == "02" ~ "tram",
      mean_of_transp == "03" ~ "subway",
      mean_of_transp == "04" ~ "urban_bus",
      mean_of_transp == "05" ~ "extra_urban_bus",
      mean_of_transp == "06" ~ "school_company_bus",
      mean_of_transp == "07" ~ "private_car_driver",
      mean_of_transp == "08" ~ "private_car_passenger",
      mean_of_transp == "09" ~ "motorbike_scooter",
      mean_of_transp == "10" ~ "bike",
      mean_of_transp == "11" ~ "other",
      TRUE ~ "walk"
    ),
    travel_time = case_when(
      travel_time == 1 ~ "<15",
      travel_time == 2 ~ "15_30",
      travel_time == 3 ~ "31_60",
      TRUE ~ ">60"
    )) %>%
  mutate(avg_travel_time = case_when( # Creating a numeric variable with the mean travel time by travel time category
    travel_time == "<15" ~ 7.5,
    travel_time == "15_30" ~ (15+30)/2,
    travel_time == "31_60" ~ (31+60)/2,
    TRUE ~ 60
  ))

# loading data on average speed of transportation means

transport_speeds <- read_xlsx(here("data-raw/transport_speeds.xlsx"))

# restricting the analysis to trips within the same municipality and for work reasons only
# Calculating mean travelled distances using average speeds of different means of transportation
# in urban areas (Isfort data)

comm_matrix_cities_km <- comm_matrix_light %>%
  filter(travel_reason == "work" &
           place_of_destination == "same municipality") %>%
  left_join(transport_speeds) %>%
  mutate(km_one_way = avg_travel_time / 60 * speed_kmh,
         km_round_trip = km_one_way * 2) %>%
  select(!c(travel_reason, place_of_destination, pop20)) %>%
  as.data.frame()


speeds_met <- tibble(mode = c("walk", "bike", "ebike"),
                     speed = c(4.6, 15, 20),
                     met = c(4, 6.8, 5))


usethis::use_data(demo_data, phy_act, comm_matrix_cities_km, mun_codes, speeds_met, overwrite = TRUE)

