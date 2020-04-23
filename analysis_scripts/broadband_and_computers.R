library(tidyverse)
library(camiller)
library(cwi)
library(sf)

fetch <- readRDS("fetch/acs_fetch_internet.rds")

out <- list()

# no computer
out$hh_without_computer <- fetch$computer %>%
  label_acs() %>%
  janitor::clean_names() %>% 
  filter(label == "Total" | str_detect(label, "Desktop or laptop$")) %>%
  mutate(group = as_factor(label) %>%
           fct_collapse(total_households = "Total", other_level = "has_computer")) %>%
  select(level, name, group, estimate) %>%
  pivot_wider(names_from = group, values_from = estimate) %>%
  mutate(no_computer = total_households - has_computer) %>%
  pivot_longer(-c(level, name), names_to = "group", values_to = "estimate") %>%
  group_by(level, name) %>%
  calc_shares(denom = "total_households", digits = 2) %>%
  filter(group != "has_computer") %>%
  ungroup()

# no broadband, incl phone plan
out$hh_without_broadband <- fetch$broadband %>%
  label_acs() %>%
  janitor::clean_names() %>%
  separate(label, into = c("total", "comp1", "group"), sep = "!!") %>%
  filter(!is.na(group)) %>%
  group_by(level, name) %>%
  add_grps(list(total_households = 1:3, no_broadband = c(1, 3))) %>%
  calc_shares(denom = "total_households", digits = 2) %>%
  ungroup()

# no broadband at home by age
out$no_broadband_by_age <- fetch$by_age %>%
  label_acs() %>%
  janitor::clean_names() %>%
  separate(label, into = c("total", "age", "comp1", "group"), sep = "!!") %>%
  filter(!is.na(group)) %>%
  mutate_at(vars(age, group), as_factor) %>%
  group_by(level, name, group) %>%
  add_grps(list(total_pop = 1:3, ages00_17 = 1, ages18_64 = 2), group = age) %>%
  group_by(level, name, age) %>%
  add_grps(list(total = 1:3, no_broadband = c(1, 3))) %>%
  calc_shares(denom = "total", digits = 2) %>%
  ungroup() %>%
  unite(group, age, group)


out_df <- out %>%
  map_dfr(filter, !is.na(share)) %>%
  select(-estimate) %>%
  pivot_wider(names_from = group, values_from = share) %>%
  select(-total_pop_no_broadband) %>%
  mutate(level = fct_relabel(level, str_remove, "^\\d+_") %>% 
           fct_relabel(str_remove, "s$")) %>%
  filter(level %in% c("state", "town"))

saveRDS(out_df, "output_data/internet.rds")
write_csv(out_df, "to_distro/town_acs_internet_2018.csv")

meta <- tribble(
  ~indicator, ~display, ~denom,
  "no_computer", "Households with no desktop/laptop computer", "Share of households, 2018",
  "no_broadband", "Households without broadband or phone data plan", "Share of households, 2018",
  "ages00_17_no_broadband", "Pop. ages 0-17, no broadband at home", "Share of population ages 0-17, 2018",
  "ages18_64_no_broadband", "Pop. ages 18-64, no broadband at home", "Share of population ages 18-64, 2018"
) %>%
  mutate(format = "0.0%")

saveRDS(meta, "output_data/internet_meta.rds")

