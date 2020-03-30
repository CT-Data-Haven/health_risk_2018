library(tidyverse)
library(cwi)

nums <- list(computer = "B28001", broadband = "B28003", by_age = "B28005")
fetch <- map(nums, multi_geo_acs, regions = cwi::regions)

saveRDS(fetch, "fetch/acs_fetch_internet.rds")