library(skimmr)
library(lubridate)
library(sf)
library(h3jsr)
library(dplyr)
library(purrr)
library(glue)
library(tidyverse)
library(leaflet)
library(htmlwidgets)

H3_RES <- 7 

# SECTION 1: LOAD & PREPARE SPATIAL DATA ----------------------------------

map <- st_read("data/map.geojson", quiet = TRUE) %>%
  filter(str_detect(subregion, regex("Kerala", ignore_case = TRUE))) %>%
  filter(!subregion %in% c("India--Kerala--Offshore", "India--Kerala--Pelagic")) %>%
  st_transform(4326)

# Generate the master list of H3 cells covering Kerala
kerala_h3_ids <- map %>%
  polygon_to_cells(res = H3_RES, simple = FALSE) %>%
  unnest(h3_addresses) %>%
  pull(h3_addresses) %>%
  unique()

# Create a spatial grid object for the background/empty zones
grid <- kerala_h3_ids %>%
  cell_to_polygon() %>%
  st_as_sf()
st_write(grid, "KL_grid.geojson", append = FALSE, quiet = TRUE)

# SECTION 2: LOAD & PREPARE BIRD DATA -------------------------------------

load("data/KL_data.RData") 
if (!exists("data_req")) {
  # ... [Keep your existing EBD loading logic here] ...
  # Ensure data_req is created and saved
}

# Overwrite data_req to save memory, keeping only essential columns
data_req <- data_req %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  select(COMMON.NAME, LATITUDE, LONGITUDE, GROUP.ID)

# Assign H3 IDs
data_req$id <- point_to_cell(
  data_req[, c("LONGITUDE", "LATITUDE")], 
  res = H3_RES, 
  simple = TRUE
)

# SECTION 3: AGGREGATE STATS ----------------------------------------------

# 1. Species counts per cell
grid_counts <- data_req %>%
  group_by(id) %>%  
  summarise(unique_species = n_distinct(COMMON.NAME), .groups = "drop")

# 2. Rank statistics per species per cell
grid_stats <- data_req %>%
  group_by(id) %>%
  mutate(total_lists = n_distinct(GROUP.ID)) %>%
  ungroup() %>%
  group_by(id, total_lists, COMMON.NAME) %>%
  summarise(detections = n_distinct(GROUP.ID), .groups = "drop") %>%
  mutate(frequency = detections / total_lists) %>%
  group_by(id) %>%
  arrange(desc(frequency)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 50) %>% 
  ungroup() %>%
  as.data.frame()

valid_h3_ids <- unique(grid_stats$id)

# SECTION 4: SPATIAL CORRELATION ------------------------------------------

corr_strict <- function(curr_id, all_ids_vec, stats_df) {
  nbs <- unlist(get_ring(curr_id, ring_size = 3))
  valid_nbs <- intersect(as.character(nbs), as.character(all_ids_vec))
  
  if (length(valid_nbs) == 0) return(NA) 
  
  curr_sp <- stats_df[stats_df$id == curr_id, c("COMMON.NAME", "rank")]
  
  cor_vals <- map_dbl(valid_nbs, function(nb_id) {
    nb_sp <- stats_df[stats_df$id == nb_id, c("COMMON.NAME", "rank")]
    merged <- inner_join(curr_sp, nb_sp, by = "COMMON.NAME", suffix = c("_cur", "_nb"))
    
    if (nrow(merged) < 3) return(NA)
    cor(merged$rank_cur, merged$rank_nb, method = "spearman")
  })
  
  if (all(is.na(cor_vals))) return(NA)
  return(mean(cor_vals, na.rm = TRUE))
}

# Generate results
results <- tibble(
  id = valid_h3_ids,
  spatial_corr = map_dbl(valid_h3_ids, ~corr_strict(., valid_h3_ids, grid_stats))
) %>% 
  filter(!is.na(spatial_corr))

# SECTION 5: SPATIAL CONVERSION FOR PLOTTING ------------------------------

# Correlation Map
map_corr <- cell_to_polygon(results$id, simple = FALSE) %>%
  rename(id = h3_address) %>%
  inner_join(results, by = "id")

# Density Map
map_density_h3 <- cell_to_polygon(grid_counts$id, simple = FALSE) %>%
  rename(id = h3_address) %>%
  inner_join(grid_counts, by = "id")

# Empty Grids (Kerala cells minus those with bird data)
empty_h3_ids <- setdiff(kerala_h3_ids, valid_h3_ids)
empty_grids_h3 <- cell_to_polygon(empty_h3_ids, simple = FALSE)

# SECTION 6: GENERATE MASTER LEAFLET MAP ----------------------------------

pal_density <- colorNumeric(palette = "YlOrRd", domain = map_density_h3$unique_species)
pal_corr    <- colorNumeric(palette = "RdYlBu", domain = c(-1, 1), na.color = "transparent")

m <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addPolygons(
    data = map, fill = FALSE, color = "black", weight = 2, group = "Kerala Subregions"
  ) %>%
  
  addPolygons(
    data = empty_grids_h3, fillColor = "#333333", fillOpacity = 0.4, 
    weight = 0.5, color = "#444444", group = "No Data Zones"
  ) %>%
  
  addPolygons(
    data = map_corr, fillColor = ~pal_corr(spatial_corr), fillOpacity = 0.8,
    weight = 1, color = "white", group = "Neighborhood Similarity",
    highlightOptions = highlightOptions(weight = 3, color = "cyan", bringToFront = TRUE),
    label = ~paste0("Similarity: ", round(spatial_corr, 2))
  ) %>%
  
  addPolygons(
    data = map_density_h3, fillColor = ~pal_density(unique_species), fillOpacity = 0.7,
    weight = 1, color = "white", group = "Species Density", 
    label = ~paste0("Species Count: ", unique_species),
    highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE)
  ) %>%
  
  addLegend(pal = pal_corr, values = c(-1, 1), title = "Similarity", position = "bottomright") %>%
  addLegend(pal = pal_density, values = map_density_h3$unique_species, title = "Species Count", position = "bottomleft") %>%
  
  addLayersControl(
    overlayGroups = c("Species Density", "Neighborhood Similarity", "No Data Zones", "Kerala Subregions"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup("Species Density")

saveWidget(m, file = "Kerala_Bird_Analysis_H3_Map.html", selfcontained = TRUE)
