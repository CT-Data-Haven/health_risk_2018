library(tidyverse)
library(jsonlite)
library(sf)


############# all data files
all_files <- list.files("output_data", full.names = TRUE) %>%
  set_names(str_extract, "\\w+(?=\\..+$)") %>%
  as.list()

# meta files
meta <- all_files[str_subset(names(all_files), "meta")] %>%
  set_names(str_remove, "_meta") %>%
  map(readRDS)

# copy over display names from cws meta to scores meta
meta$score <- meta$score %>%
  map(left_join, bind_rows(meta$cws) %>% select(indicator, display), by = "indicator") %>%
  map(mutate, display = coalesce(display, camiller::clean_titles(indicator))) %>%
  map(~split(., .$indicator)) %>%
  map_depth(2, select, -indicator) %>%
  map_depth(2, purrr::flatten)

# data files
dat <- all_files[str_subset(names(all_files), "meta", negate = TRUE)] %>%
  map(readRDS)

write_json(meta, "to_viz/meta.json", auto_unbox = TRUE)
write_json(dat, "to_viz/dash_data.json", auto_unbox = TRUE)


##########  topojson
town_topo <- tigris::county_subdivisions("09", cb = TRUE, class = "sf", refresh = T, year = 2017) %>%
  select(name = NAME, geometry) %>%
  rmapshaper::ms_simplify(keep = 0.4)# %>%
  # geojsonio::topojson_write(geometry = "polygon", object_name = "town", file = file.path("to_viz", "town_topo.json"))

shps <- list.files("shapes", full.names = TRUE) %>%
  set_names(str_extract, "\\w+(?=\\..+$)") %>%
  set_names(str_replace, "sf", "topo") %>%
  map(readRDS)

c(lst(town_topo), shps) %>%
  iwalk(function(shp, name) {
    geojsonio::topojson_write(shp, geometry = "polygon", object_name = str_remove(name, "_topo"),
                              file = str_glue("to_viz/{ name }.json"))
  })

########### intro text + downloads
intro_txt <- read_delim("input_data/intro_text.txt", delim = "|")

library(googlesheets4)
downloads <- googledrive::drive_get(id = "1C6K4_73M7iVfOnomcquAYT2bcbvWyvdGjq17COJrwZo") %>% 
  read_sheet()

full_join(intro_txt, downloads, by = "page") %>%
  nest(intro = c(headline, text), download = c(dwdownload, dwsite, github), source = source) %>%
  split(.$page) %>%
  map(select, -page) %>%
  map_depth(2, purrr::flatten) %>%
  map_depth(2, function(l) keep(l, ~!is.na(.))) %>%
  map(compact) %>%
  map(function(l) l["source"] <- { l["source"] <- l["source"][[1]]; l  }) %>%
  write_json("to_viz/page_meta.json", auto_unbox = TRUE)

 