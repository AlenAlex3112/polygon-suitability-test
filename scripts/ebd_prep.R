#LOAD & PREPARE BIRD DATA ----------------------------------------

data_path <- "data/data.RData" # change data file name if needed
load("../ebird-datasets/EBD/ebd_IN_relNov-2025.RData")
load("../ebird-datasets/EBD/ebd_IN_relNov-2025_unv.RData")

data_req <- bind_rows(data, data_unv) %>% 
  filter(!(EXOTIC.CODE %in% c("P", "X"))) %>% 
  #filter(ALL.SPECIES.REPORTED == 1) %>%
  select(COMMON.NAME, LATITUDE, LONGITUDE, GROUP.ID, ALL.SPECIES.REPORTED) %>%
  mutate(h3_id = point_to_cell(data.frame(LONGITUDE, LATITUDE), res = 7, simple = TRUE))

rm(data, data_sed, data_unv)

unique_lists <- data_req %>%
  distinct(GROUP.ID, LATITUDE, LONGITUDE) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

lists_within_polygon <- st_join(unique_lists, map %>% select(id), join = st_intersects) %>%
  st_drop_geometry() %>%
  select(GROUP.ID, polygon_id = id) 

data_req <- data_req %>%
  left_join(lists_within_polygon, by = "GROUP.ID") %>% 
  filter(!is.na(polygon_id))

rm(unique_lists, lists_within_polygon)

save(data_req, file = data_path)
