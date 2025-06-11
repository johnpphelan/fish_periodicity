library(xlsx)
library(tidyverse)

# Grab all .CSVs from output folder
all_dat = list.files(path = 'output/',
                     pattern = 'csv$',
           full.names = T) |>
  lapply(\(x) {
    readr::read_csv(x)
    }) |>
  dplyr::bind_rows()

# Find out which excel file each tab is from.
data_path <- "//sfp.idir.bcgov/s140/S40203/WFC AEB/General/2 Stewardship Mgt Climate Change/Drought/Communications/Periodicity tables/"
excel_files <- list.files(path = data_path, pattern = "\\.xlsx$", full.names = TRUE)
wbs_by_region = lapply(excel_files, \(x) {
  d = data.frame(loc = stringr::str_to_title(readxl::excel_sheets(x)))
  d$region = stringr::str_extract(x,"(?<=tables\\/)[A-Z]+")
  d
  }) |>
  dplyr::bind_rows() |>
  dplyr::mutate(loc = ifelse(loc == "Creston Water Management Precin", "Creston Water Management Precinct", loc))

all_dat = all_dat |>
  dplyr::left_join(wbs_by_region)

current_wd = getwd()
base_dir = stringr::str_extract(current_wd, 'C:/Users/[A-Z]+/')
onedrive_folder = paste0(base_dir,"OneDrive - Government of BC/data/")

saveRDS(all_dat, file = "output/fish_periodicity.rds")
saveRDS(all_dat, file = paste0(onedrive_folder,"fish_periodicity.rds"))
