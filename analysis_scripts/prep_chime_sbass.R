library(tidyverse)
library(jsonlite)
library(sf)

############### SBASS
towns <- cwi::xwalk %>% distinct(town)
region_dists <- read_delim("input_data/regional_school_dists.tsv", delim = ";") %>%
  separate_rows(towns, sep = ",") %>%
  mutate(district = paste("Regional District", str_pad(district, 2, "left", "0")))

enroll <- readRDS("input_data/student_enrollment_by_group_2017_2019.rds")$enroll_by_gender %>%
  filter(key == "total", year == "2018-2019", level == "district") %>%
  select(name, enroll = value)

sbass <- read_csv("input_data/school_based_asthma_2012_2014.csv") %>%
  # filter(level == "state" | name %in% unique(cwi::xwalk$town)) %>%
  filter(level != "regions") %>%
  filter(!is.na(rate)) %>%
  select(name, value = rate) %>%
  mutate(name = str_replace(name, "Regional School District", "Reg. Dist."))

no_own_district <- towns %>%
  anti_join(enroll, by = c("town" = "name")) %>%
  inner_join(region_dists, by = c("town" = "towns"))

own_district <- towns %>%
  semi_join(enroll, by = c("town" = "name")) %>%
  mutate(district = town)

town_sf <- tigris::county_subdivisions("09", cb = TRUE, class = "sf", refresh = T, year = 2017) %>%
  select(name = NAME, geometry) %>%
  rmapshaper::ms_simplify(keep = 0.4)

district_sf <- left_join(town_sf, bind_rows(own_district, no_own_district), by = c("name" = "town")) %>%
  st_as_sf() %>%
  group_by(district) %>%
  summarise() %>%
  rename(name = district) %>%
  mutate(name = str_replace(name, "Regional District", "Reg. Dist.")) %>% 
  st_cast("MULTIPOLYGON")

sbass %>%
  semi_join(district_sf, by = "name") %>%
  saveRDS("output_data/sbass.rds")

saveRDS(district_sf, "shapes/school_districts_sf.rds")

############### CHIME
conditions <- c("COPD", "Diabetes", "Heart Disease", "Lung Cancer", "Hypertension", "Sub Abuse")
regions <- c("Connecticut", levels(cwi::ct5_clusters$cluster))
meta <- conditions %>%
  set_names(~word(.) %>% tolower()) %>%
  enframe(name = "indicator", value = "orig") %>%
  mutate(display = as_factor(orig) %>% 
           fct_relabel(camiller::clean_titles, split_case = FALSE) %>%
           fct_recode("Substance abuse" = "Sub abuse"),
         format = ",d")

n_min <- 100
# indicator by town by age group, 2015-2017 if at least 100 encounters/population
# change by region

chime_read <- readRDS("input_data/data_gender_mod2.rds") %>%
  select(-aggr:-ratio_by_age) %>%
  rename(name = towns, value = val) %>%
  mutate(name = str_replace_all(name, "_", " ") %>% recode("Ct Aggregate" = "Connecticut"),
         value = round(value)) %>%
  filter(sex == "Total") %>%
  filter(age_range != "Age 0-19") %>%
  inner_join(meta %>% select(indicator, orig), by = c("clinical_feature" = "orig")) %>%
  select(-clinical_feature) %>%
  # filter(clinical_feature %in% conditions) %>%
  filter(name %in% c(cwi::xwalk$town, regions)) %>%
  mutate_all(na_if, "NA") %>%
  mutate_at(vars(-name, -value), as_factor) %>%
  mutate(age_range = age_range %>% 
           fct_relabel(str_replace, "Age\\b", "Ages") %>%
           fct_relabel(camiller::clean_titles),
         level = as_factor(name) %>%
           fct_collapse(state = "Connecticut", region = levels(cwi::ct5_clusters$cluster), other_level = "town"))

too_small <- chime_read %>%
  filter(metric %in% c("Census", "Num_Enc")) %>%
  filter(value < n_min) %>%
  distinct(name, year, age_range, indicator)

chime <- chime_read %>%
  filter((age_range != "All ages" & metric == "Enc_Rate") | metric == "Age_Adj") %>%
  # select(level, name, year, age_range, clinical_feature, value) %>%
  anti_join(too_small, by = c("name", "year", "age_range", "indicator")) %>%
  select(level, name, year, age = age_range, indicator, value) %>%
  arrange(year:indicator, level, name) %>%
  mutate(age = fct_recode(age, "All ages (age-adjusted)" = "All ages"))

chime_split <- chime %>%
  filter(year == "2015-2017") %>%
  split(.$age, drop = TRUE) %>%
  map(pivot_wider, names_from = indicator) %>%
  map(select, -year, -age)

trend <- chime %>%
  filter(level != "town", age == "All ages (age-adjusted)") %>%
  split(.$indicator, drop = TRUE) %>%
  map(select, -age, -indicator)

# dropping trend for now
# chime_nest <- list(map = chime_split, trend = trend)
saveRDS(chime_split, "output_data/chime_data.rds")


############## CHIME META
chime %>%
  filter(year == "2015-2017", !is.na(value)) %>%
  inner_join(meta, by = "indicator") %>%
  distinct(age, indicator, display, format) %>%
  mutate(denom = "Rate per 10,000 residents, 2015–2017") %>%
  split(.$age) %>%
  map(select, -age) %>%
  saveRDS("output_data/chime_meta.rds")


