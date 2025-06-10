# Grab all .CSVs from output folder
all_dat = list.files(path = 'output/',
                     pattern = 'csv$',
           full.names = T) |>
  lapply(\(x) readr::read_csv(x)) |>
  dplyr::bind_rows()

current_wd = getwd()
base_dir = stringr::str_extract(current_wd, 'C:/Users/[A-Z]+/')
onedrive_folder = paste0(base_dir,"OneDrive - Government of BC/data/")

saveRDS(all_dat, file = "output/fish_periodicity.rds")
saveRDS(all_dat, file = paste0(onedrive_folder,"fish_periodicity.rds"))
