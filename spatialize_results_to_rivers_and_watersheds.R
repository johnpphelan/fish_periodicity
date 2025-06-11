library(bcdata)
library(tidyverse)

fish_per = readRDS("output/fish_periodicity.rds")

fish_per = fish_per |>
  dplyr::mutate(ORG_UNIT = dplyr::case_when(
    region == 'WC' ~ 'RWC',
    region == 'KBR' ~ 'RKB',
    region == 'SC' ~ 'RSC',
    region == 'TOR' ~ 'RTO'
  )) |>
  dplyr::mutate(loc = dplyr::case_when(
    loc == 'Barriere River' ~ 'Barrière River',
    loc == 'Upper Nicola River' ~ 'Nicola River',
    loc == 'Tetayut (Sandhill) Creek' ~ 'Sandhill Creek',
    T ~ loc
  ))

natural_res_reg = bcmaps::nr_regions() |>
  dplyr::filter(ORG_UNIT %in% unique(fish_per$ORG_UNIT))

loc_names = unique(fish_per$loc)

loc_streams = bcdc_query_geodata('freshwater-atlas-stream-network') |>
  filter(GNIS_NAME %in% loc_names) |>
  collect() |>
  sf::st_zm()

# Summarise geometries by GNIS_NAME and FWA_WATERSHED_CODE
loc_streams = loc_streams |>
  dplyr::group_by(GNIS_NAME, FWA_WATERSHED_CODE) |>
  dplyr::summarise() |>
  dplyr::ungroup()

# unique(fish_per$loc)[!unique(fish_per$loc) %in% unique(loc_streams$GNIS_NAME)]
# We're just lacking the 'Creston Water Management Precinct'

# Join the natural resource region across to loc_streams so we can
# ensure we have the correct streams selected for each region.
loc_streams = loc_streams |>
  dplyr::left_join(
    fish_per |>
      dplyr::select(loc,ORG_UNIT) |>
      dplyr::distinct() |>
      dplyr::rename(GNIS_NAME = loc)
  )

loc_streams = loc_streams |>
  dplyr::mutate(row_id = dplyr::row_number()) |>
  sf::st_join(natural_res_reg |> dplyr::select(ORG_UNIT_sp_match = ORG_UNIT)) |>
  dplyr::filter(!duplicated(row_id))

loc_streams = loc_streams |>
  dplyr::filter(ORG_UNIT == ORG_UNIT_sp_match)

# We still have a couple of stream / river names with multiple rows.
# Let's check the length of their geometries... maybe drop the little guys?
loc_streams = loc_streams |>
  dplyr::mutate(geom_length = as.numeric(sf::st_length(geometry))) |>
  dplyr::group_by(GNIS_NAME) |>
  dplyr::slice_max(geom_length) |>
  dplyr::ungroup()

# Overlap with watershed groups.
if(!exists('ws')){
  ws = bcdata::bcdc_query_geodata('freshwater-atlas-watershed-groups') |>
    bcdata::collect()
}

loc_ws_overlap = loc_streams |>
  sf::st_join(ws |> dplyr::select(WATERSHED_GROUP_NAME)) |>
  sf::st_drop_geometry()

number_locs_per_ws = loc_ws_overlap |>
  dplyr::count(WATERSHED_GROUP_NAME)

# How many different rivers / streams do we have per watershed group?
num_wbs_per_watershed_plot = ggplot() +
  geom_sf(data = ws |> dplyr::left_join(number_locs_per_ws),
          aes(fill = n)) +
  labs(fill = 'number of streams/rivers\n with periodicity data')

ggsave(filename = "output/num_wbs_per_watershed_plot.jpg",
       plot = num_wbs_per_watershed_plot,
       width = 8,
       height = 6)

loc_ws_overlap = loc_ws_overlap |>
  dplyr::group_by(GNIS_NAME) |>
  dplyr::reframe(WATERSHED_GROUP_NAME = paste0(WATERSHED_GROUP_NAME, collapse = ', '))

loc_streams = loc_streams |>
  dplyr::left_join(loc_ws_overlap)

# Write out fish periodicity matched to rivers / streams
sf::write_sf(loc_streams, "output/fish_periodicity_stream_geometries.gpkg")

