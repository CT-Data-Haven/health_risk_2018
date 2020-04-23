library(tidyverse)
library(data.world)

dataset <- "camille86/neighborhoods18"

upload_file(dataset, "to_distro/town_acs_internet_2018.csv", "internet_access_2018.csv")

# doesn't work, not urgent
desc <- "2018 ACS indicators related to internet access"
req <- file_create_or_update_request(file_name = "internet_access_2018.csv", 
                                            description = desc, 
                                            labels = list("clean data"))

update_dataset(dataset, dataset_update_request(files = req))
